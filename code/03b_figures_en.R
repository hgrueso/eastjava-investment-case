# 03b_figures_en.R — Stage 3: descriptive figures (NA-robust) ------------
# Inputs:  output/analysis_ready.rds, output/tables/t8_target_funnel.csv
# Outputs: output/figures/*.png  (skips figures with no available data)
# ------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr)
  library(readr); library(tibble); library(here); library(glue)
  library(ggplot2); library(scales); library(survey); library(srvyr)
})
source(here::here("R","utils.R")); source(here::here("R","theme.R"))
source(here::here("R","variable_mapping.R")); ensure_dirs()

ado <- readRDS(here::here("output","analysis_ready.rds"))
IS_REAL <- isTRUE(attr(ado,"is_real"))
options(survey.lonely.psu = "adjust")
des <- if (all(c("psu","strata") %in% names(ado)) && !all(is.na(ado$psu)))
  ado |> as_survey_design(ids = psu, strata = strata, weights = w, nest = TRUE) else
  ado |> as_survey_design(ids = 1, weights = w)

cap_src <- paste0("Source: ", attr(ado,"vintage"),
                  ". Weighted, adolescents 15–24.",
                  if (!IS_REAL) "  ⚠ TEMPLATE — not East Java." else "")
has_data <- function(v) v %in% names(ado) && any(!is.na(ado[[v]]))

# Weighted %, outcome coerced to numeric; returns empty tibble if all-NA.
wt_pct <- function(design, var, by = NULL) {
  if (!has_data(var)) return(tibble())
  d <- design |> mutate(.y = as.numeric(.data[[var]]))
  if (is.null(by)) tibble(estimate = as.numeric(svymean(~.y, d, na.rm = TRUE)))
  else svyby(~.y, as.formula(paste0("~", paste(by, collapse="+"))), d,
             svymean, na.rm = TRUE) |> as_tibble() |> rename(estimate = .y)
}

OUTC     <- c("Attending school","NEET (15-24)","Employed","Internet use","Married before 18")
OUT_VARS <- c(in_school="Attending school", neet="NEET (15-24)", employed="Employed",
              internet="Internet use", married_u18="Married before 18")
OUT_VARS <- OUT_VARS[names(OUT_VARS) %in% names(ado) & vapply(names(OUT_VARS), has_data, logical(1))]
fill_sx  <- c("Boys"=ACCENT_BOY, "Girls"=ACCENT_GIRL)

outcomes_by <- function(design, by_var = NULL) {
  grp <- c("female", by_var)
  map_dfr(names(OUT_VARS), \(v) wt_pct(design, v, grp) |> mutate(indicator = OUT_VARS[[v]])) |>
    mutate(sexo = ifelse(female == 1 | female == TRUE, "Girls", "Boys"),
           indicator = droplevels(factor(indicator, levels = OUTC)))
}
edu_theme <- function() theme(strip.text = element_text(face="bold", size=12, colour=GREY_DARK),
                              panel.spacing.x = unit(1.1,"lines"), legend.position = "top")
pct1 <- scale_y_continuous(labels = percent_format(1), limits = c(0,1),
                           expand = expansion(mult = c(0,.08)))
lab1 <- geom_text(aes(label = percent(estimate, accuracy = 1)),
                  position = position_dodge(0.7), vjust = -0.4, size = 3.1, colour = GREY_DARK)

# f1a — by sex
f1a_d <- outcomes_by(des)
if (nrow(f1a_d)) {
  save_fig(ggplot(f1a_d, aes(sexo, estimate, fill = sexo)) +
    geom_col(width = .55) +
    geom_text(aes(label = percent(estimate, accuracy = 1)), vjust = -.4, size = 4, colour = GREY_DARK) +
    facet_wrap(~indicator, nrow = 1) + pct1 +
    scale_fill_manual(values = fill_sx, name = NULL) +
    labs(subtitle = "Adolescents 15–24, by sex", x = NULL, y = NULL, caption = cap_src) +
    edu_theme(), "f1a_outcomes_by_sex_en", height = 4.6)
}

# f1b — by sex × area
f1b_d <- outcomes_by(des, "rural") |> filter(!is.na(rural)) |>
  mutate(zona = ifelse(rural == 1 | rural == TRUE, "Rural", "Urban"))
if (nrow(f1b_d)) {
  save_fig(ggplot(f1b_d, aes(zona, estimate, fill = sexo)) +
    geom_col(position = position_dodge(.7), width = .6) + lab1 +
    facet_wrap(~indicator, nrow = 1) + pct1 +
    scale_fill_manual(values = fill_sx, name = NULL) +
    labs(subtitle = "By sex and urban/rural area", x = NULL, y = NULL, caption = cap_src) +
    edu_theme(), "f1b_outcomes_by_sex_area_en", width = 11, height = 4.8)
}

# f1d — by sex × wealth (only if available)
if (has_data("wealth_q")) {
  f1d_d <- outcomes_by(des, "wealth_q") |> filter(!is.na(wealth_q))
  if (nrow(f1d_d)) save_fig(ggplot(f1d_d, aes(wealth_q, estimate, fill = sexo)) +
    geom_col(position = position_dodge(.7), width = .6) + facet_wrap(~indicator, nrow = 1) + pct1 +
    scale_fill_manual(values = fill_sx, name = NULL) +
    labs(subtitle = "By sex and per-capita expenditure quintile (Q1 poorest → Q5)",
         x = NULL, y = NULL, caption = cap_src) + edu_theme(),
    "f1d_outcomes_by_sex_wealth_en", width = 11, height = 4.8)
}

# f5 — by sex × disability (only if available)
if (has_data("disab_any")) {
  f5_d <- outcomes_by(des, "disab_any") |> filter(!is.na(disab_any)) |>
    mutate(grupo = ifelse(disab_any == 1, "Disability (WG3+)", "No disability"))
  if (nrow(f5_d)) save_fig(ggplot(f5_d, aes(grupo, estimate, fill = sexo)) +
    geom_col(position = position_dodge(.7), width = .6) + facet_wrap(~indicator, nrow = 1) + pct1 +
    scale_fill_manual(values = fill_sx, name = NULL) +
    labs(subtitle = "By sex and disability (WG, severity ≥ 3)", x = NULL, y = NULL,
         caption = paste0(cap_src," Small subsample — caution.")) + edu_theme(),
    "f5_outcomes_by_sex_disab_en", width = 11, height = 4.8)
}

# f2 — digital access/use (only if available)
f2_d <- bind_rows(
  wt_pct(des, "internet",  c("female","rural")) |> mutate(indicator = "Internet use"),
  wt_pct(des, "mobile",    c("female","rural")) |> mutate(indicator = "Owns mobile"),
  wt_pct(des, "net_learn", c("female","rural")) |> mutate(indicator = "Internet for learning"))
if (nrow(f2_d)) {
  f2_d <- f2_d |> mutate(sexo = ifelse(female == 1 | female == TRUE,"Girls","Boys"),
                         zona = ifelse(rural == 1 | rural == TRUE,"Rural","Urban"))
  save_fig(ggplot(f2_d, aes(zona, estimate, fill = sexo)) +
    geom_col(position = position_dodge(.7), width = .6) + lab1 + facet_wrap(~indicator, nrow = 1) + pct1 +
    scale_fill_manual(values = fill_sx, name = NULL) +
    labs(subtitle = "Digital access and use, by sex and area", x = NULL, y = NULL, caption = cap_src) +
    edu_theme(), "f2_digital_by_area_sex_en", width = 10, height = 4.8)
} else message("skip f2 — no digital variables on this data source")

# f6 — girls' NEET by kabupaten/kota (East Java district names) ----------
# East Java (prov 35) BPS district codes -> names. Verify against IFLS5_BPS_2014_codes.
EJ_DISTRICT <- c(
  "1"="Pacitan","2"="Ponorogo","3"="Trenggalek","4"="Tulungagung","5"="Blitar",
  "6"="Kediri","7"="Malang","8"="Lumajang","9"="Jember","10"="Banyuwangi",
  "11"="Bondowoso","12"="Situbondo","13"="Probolinggo","14"="Pasuruan","15"="Sidoarjo",
  "16"="Mojokerto","17"="Jombang","18"="Nganjuk","19"="Madiun","20"="Magetan",
  "21"="Ngawi","22"="Bojonegoro","23"="Tuban","24"="Lamongan","25"="Gresik",
  "26"="Bangkalan","27"="Sampang","28"="Pamekasan","29"="Sumenep",
  "71"="Kota Kediri","72"="Kota Blitar","73"="Kota Malang","74"="Kota Probolinggo",
  "75"="Kota Pasuruan","76"="Kota Mojokerto","77"="Kota Madiun","78"="Kota Surabaya",
  "79"="Kota Batu")
f6_d <- wt_pct(des |> filter(female), "neet", "kabkota") |>
  filter(!is.na(estimate)) |>
  group_by(kabkota) |> filter(n() >= 1) |> ungroup() |>      # one row per district already
  mutate(name = dplyr::coalesce(EJ_DISTRICT[as.character(kabkota)],
                                paste0("code ", kabkota))) |>
  arrange(estimate)
# keep only districts with an adequate raw sample of girls
nkeep <- ado |> filter(female == TRUE | female == 1) |> count(kabkota) |>
  filter(n >= 15) |> pull(kabkota)
f6_d <- f6_d |> filter(kabkota %in% nkeep)
if (nrow(f6_d)) save_fig(
  ggplot(f6_d, aes(estimate, reorder(name, estimate))) +
    geom_col(fill = UNICEF_BLUE, width = .7) +
    geom_text(aes(label = percent(estimate, accuracy = 1)), hjust = -0.15,
              size = 2.9, colour = GREY_DARK) +
    scale_x_continuous(labels = percent_format(1), expand = expansion(mult = c(0,.12))) +
    labs(subtitle = "Girls' NEET rate, by East Java district (≥15 sampled girls)",
         x = NULL, y = NULL, caption = cap_src) +
    theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 8)),
  "f6_neet_by_kabkota_en", width = 8, height = 6)

# f7 — target funnel
fp <- here::here("output","tables","t8_target_funnel.csv")
if (file.exists(fp)) {
  funnel <- read_csv(fp, show_col_types = FALSE) |>
    filter(segment != "All adolescents 15–24 (weighted)") |>
    mutate(segment = factor(segment, levels = rev(segment)))
  save_fig(ggplot(funnel, aes(n_pop, segment)) + geom_col(fill = UNICEF_DARK, width = .7) +
    geom_text(aes(label = comma(round(n_pop))), hjust = -0.1, size = 3.3, colour = GREY_DARK) +
    scale_x_continuous(labels = comma_format(), expand = expansion(mult = c(0,.2))) +
    labs(subtitle = "Weighted adolescent girls 15–24, by segment", x = NULL, y = NULL, caption = cap_src) +
    theme(panel.grid.major.y = element_blank()), "f7_target_funnel_en", width = 10, height = 5)
}
message("Stage 3 complete → output/figures/")
