# 02_descriptive.R — Stage 2: weighted descriptives (NA-robust) ----------
# Inputs:  output/analysis_ready.rds   Outputs: output/tables/t1…t8 .csv
# ------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr)
  library(readr); library(tibble); library(here); library(glue)
  library(survey); library(srvyr); library(scales)
})
source(here::here("R", "utils.R")); source(here::here("R", "variable_mapping.R")); ensure_dirs()

ado <- readRDS(here::here("output", "analysis_ready.rds"))

options(survey.lonely.psu = "adjust")
has_design <- all(c("psu","strata") %in% names(ado)) &&
              !all(is.na(ado$psu)) && !all(is.na(ado$strata))
des <- if (has_design)
  ado |> as_survey_design(ids = psu, strata = strata, weights = w, nest = TRUE) else {
  message("note: PSU/strata unavailable — weighted means only, SEs approximate")
  ado |> as_survey_design(ids = 1, weights = w) }

KEY_BIN <- c("in_school","employed","neet","internet","mobile","net_learn",
             "married_u18","disab_any","rural","poor_q1q2")
KEY_BIN <- KEY_BIN[KEY_BIN %in% names(ado)]
avail <- function(vars) vars[vapply(vars, \(v) any(!is.na(ado[[v]])), logical(1))]

# Weighted mean by group(s). Coerces the outcome to numeric internally (so
# logical/integer both work) and skips all-NA outcomes.
wt_summary <- function(design, vars, by = NULL) {
  if (!is.null(by)) vars <- setdiff(vars, by)
  map_dfr(vars, function(v) {
    if (all(is.na(design$variables[[v]]))) return(tibble())
    d <- design |> mutate(.y = as.numeric(.data[[v]]))
    if (is.null(by)) {
      m <- svymean(~.y, d, na.rm = TRUE)
      tibble(variable = v, estimate = as.numeric(m),
             se = as.numeric(sqrt(diag(attr(m, "var")))),
             n_pop = sum(weights(d)[!is.na(d$variables$.y)]))
    } else {
      df <- svyby(~.y, as.formula(paste0("~", paste(by, collapse = "+"))),
                  d, svymean, na.rm = TRUE) |> as_tibble()
      se_col <- intersect(c("se", "se..y"), names(df))[1]
      if (is.na(se_col)) { df$se <- NA_real_; se_col <- "se" }
      df |> rename(estimate = .y, se = !!sym(se_col)) |> mutate(variable = v)
    }
  })
}

# t1 — overall
write_table(wt_summary(des, avail(KEY_BIN)) |>
  mutate(pct = percent(estimate, accuracy = 0.1)) |>
  select(variable, estimate, se, pct, n_pop) |> arrange(variable),
  "t1_overall_adolescents")

# t2 — by sex
write_table(wt_summary(des, avail(KEY_BIN), by = "female") |>
  mutate(sex = ifelse(female == 1 | female == TRUE, "Girls", "Boys")) |>
  select(variable, sex, estimate, se) |>
  pivot_wider(names_from = sex, values_from = c(estimate, se)) |>
  mutate(gap_girls_minus_boys = estimate_Girls - estimate_Boys),
  "t2_by_sex")

des_g <- des |> filter(female)

# t3 — girls by area
write_table(wt_summary(des_g, avail(KEY_BIN), by = "rural") |>
  mutate(area = ifelse(rural == 1 | rural == TRUE, "Rural", "Urban")) |>
  select(variable, area, estimate, se) |>
  pivot_wider(names_from = area, values_from = c(estimate, se)),
  "t3_girls_by_area")

# t4 — girls by wealth quintile (only if wealth available)
if (any(!is.na(ado$wealth_q)))
  write_table(wt_summary(des_g, avail(KEY_BIN), by = "wealth_q") |>
    select(variable, wealth_q, estimate, se) |> arrange(variable, wealth_q),
    "t4_girls_by_wealth_quintile")

# t5 — girls by disability (only if available)
if (any(!is.na(ado$disab_any)))
  write_table(wt_summary(des_g, avail(setdiff(KEY_BIN,"disab_any")), by = "disab_any") |>
    mutate(group = ifelse(disab_any == 1, "Disability", "No disability")) |>
    select(variable, group, estimate, se) |>
    pivot_wider(names_from = group, values_from = c(estimate, se)),
    "t5_girls_by_disability")

# t6 — girls' NEET by kabupaten/kota
write_table(wt_summary(des_g, "neet", by = "kabkota") |>
  select(kabkota, estimate, se) |> arrange(desc(estimate)),
  "t6_girls_neet_by_kabkota")

# t7 — girls by senior-secondary age band
write_table(wt_summary(des_g, avail(c("in_school","neet","employed","married_u18")),
                       by = "ss_age") |>
  mutate(band = ifelse(ss_age == 1 | ss_age == TRUE, "16-18 (SS age)", "Other 15-24")) |>
  select(variable, band, estimate, se),
  "t7_girls_by_ss_age")

# t8 — target funnel (only segments whose underlying variable has data)
girl_n <- function(design, expr) { e <- rlang::enquo(expr); sum(weights(design |> filter(!!e))) }
add <- function(label, val) tibble(segment = label, n_pop = val)
t8 <- bind_rows(
  add("All girls 15–24",        girl_n(des, female)),
  add("Girls 15–24 — rural",    girl_n(des, female & rural %in% c(1, TRUE))),
  if (any(!is.na(ado$poor_q1q2))) add("Girls 15–24 — poor (Q1–Q2)",
        girl_n(des, female & poor_q1q2 %in% c(1, TRUE))),
  if (any(!is.na(ado$disab_any))) add("Girls 15–24 — disability (WG 3+)",
        girl_n(des, female & disab_any %in% c(1, TRUE))),
  if (any(!is.na(ado$internet)))  add("Girls 15–24 — no internet use",
        girl_n(des, female & internet %in% c(0, FALSE)))
) |> mutate(share_of_all_girls = n_pop / n_pop[1])
write_table(t8, "t8_target_funnel")

message("Stage 2 complete → output/tables/t1…t8"); print(t8)
