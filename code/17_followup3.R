# 17_followup3.R: third round of follow-ups (UNICEF team comments).
#  A. Programme (S4G) districts vs the rest of East Java, 2024 to 2025: pooled
#     comparison of changes (descriptive difference-in-differences). Districts
#     are pooled, not reported one by one, because district samples are small.
#  B. What changed between 2024 and 2025, East Java vs rest of Indonesia: NEET by
#     age band and education, school vs work, type and sector of work. Tests the
#     explanations raised by the team (internships, informal/gig work, schooling).
#  C. Pass-through: share of girls who leave school before completing senior
#     secondary who are NEET. Replaces the 0.45 assumption in the CBA.
# Both years use the same definitions. Renumbered variables:
#   attendance R610 (2024) / R611 (2025); temporarily absent R705 / R704;
#   certificate R614 / R615; level now R612 / R613; status R707 / R706; sector R706 / R705.
suppressPackageStartupMessages({ library(haven); library(dplyr); library(tidyr); library(ggplot2)
  library(survey); library(readr); library(readxl); library(here) })
source(here::here("R","utils.R")); source(here::here("R","theme.R"))
if (!exists("save_fig")) save_fig <- function(p, name, width = 8, height = 5, dpi = 220) {
  ggsave(here::here("output","figures", paste0(name, ".png")), p, width = width, height = height, dpi = dpi, bg = "white")
  message("  saved figure: ", name, ".png") }
options(survey.lonely.psu = "adjust"); MOD <- here::here("output","models")
num <- function(x) as.numeric(x)

# ---- programme districts ----
s4g <- read_excel(here::here("data","Susenas 2025","master_wilayah_KOR_2025.xlsx"), sheet = "S4G Programme Area") |>
  transmute(kab = as.numeric(kab), s4g = as.numeric(s4g_dist) == 1, district = trimws(nama_kab))
cat("Programme districts:", paste(s4g$district[s4g$s4g], collapse = ", "), "\n")

# ---- harmonised build ----
build <- function(r, yr) {
  v <- if (yr == 2024) list(att = "R610", abs = "R705", cert = "R614", lvl = "R612", st = "R707", sec = "R706", w = "FWT")
       else            list(att = "R611", abs = "R704", cert = "R615", lvl = "R613", st = "R706", sec = "R705", w = "WEIND")
  tibble(year = yr, prov = num(r$R101), kab = num(r$R102) %% 100, urban = num(r$R105) == 1,
         female = num(r$R405) == 2, age = num(r$R407), w = num(r[[v$w]]),
         psu = paste(yr, r$PSU), strata = paste(yr, r$STRATA),
         married_u18 = num(r$R404) %in% 2:4 & num(r$R409) %in% 1:17,
         in_school = num(r[[v$att]]) %in% 2 | as.character(r$R703_B) == "B",
         working = as.character(r$R703_A) == "A" | num(r[[v$abs]]) %in% 1,
         cert = num(r[[v$cert]]), level_now = num(r[[v$lvl]]),
         status = num(r[[v$st]]), sector = num(r[[v$sec]])) |>
    mutate(employed = working & !in_school, neet = !in_school & !employed,
           tertiary_cert = cert %in% 18:24,
           sec_enrol = in_school & level_now %in% 11:17,
           tert_enrol = in_school & level_now %in% 18:24)
}
c24 <- c("R101","R102","R105","R404","R405","R407","R409","R610","R612","R614","R703_A","R703_B","R705","R706","R707","FWT","PSU","STRATA")
c25 <- c("R101","R102","R105","R404","R405","R407","R409","R611","R613","R615","R703_A","R703_B","R704","R705","R706","WEIND","PSU","STRATA")
d <- bind_rows(
  build(read_dta(here::here("data","Susenas 2024","ssn202403_kor_ind1.dta"), col_select = all_of(c24)), 2024),
  build(read_dta(here::here("data","Susenas 2025","SUSENAS_25-ALL-Merged.dta"), col_select = all_of(c25)), 2025)) |>
  filter(!is.na(w)) |>
  mutate(unit = ifelse(prov == 35, "East Java", "Rest of Indonesia"), y25 = as.numeric(year == 2025))
gc()

# ===== A. programme vs other districts =====
ej <- d |> filter(prov == 35, female) |> left_join(s4g |> select(kab, s4g), by = "kab") |>
  mutate(group = ifelse(s4g %in% TRUE, "Programme districts", "Other districts"), prog = as.numeric(s4g %in% TRUE))
cat("Unmatched East Java districts:", sum(is.na(ej$s4g)), "rows\n")
outcomes <- list(NEET = list(v = "neet", a = c(15, 24)), `In school` = list(v = "in_school", a = c(15, 24)),
                 Employed = list(v = "employed", a = c(15, 24)), `Married before 18` = list(v = "married_u18", a = c(15, 24)),
                 `Senior-secondary enrolment (16-18)` = list(v = "sec_enrol", a = c(16, 18)),
                 `Tertiary enrolment (18-20)` = list(v = "tert_enrol", a = c(18, 20)))
A <- bind_rows(lapply(names(outcomes), function(nm) {
  o <- outcomes[[nm]]; dd <- ej |> filter(age >= o$a[1], age <= o$a[2]) |> mutate(yv = as.numeric(.data[[o$v]]))
  des <- svydesign(ids = ~psu, strata = ~strata, weights = ~w, data = dd, nest = TRUE)
  fit <- svyglm(yv ~ prog * y25, des); co <- summary(fit)$coefficients["prog:y25", ]
  ch <- sapply(c("Programme districts", "Other districts"), function(gg)
    summary(svyglm(yv ~ y25, subset(des, group == gg)))$coefficients["y25", 1:2])
  lv <- dd |> group_by(group, year) |> summarise(r = 100 * weighted.mean(yv, w), n = n(), .groups = "drop") |>
    pivot_wider(names_from = year, values_from = c(r, n))
  tibble(Outcome = nm,
         `Programme 2024` = round(lv$r_2024[lv$group == "Programme districts"], 1), `Programme 2025` = round(lv$r_2025[lv$group == "Programme districts"], 1),
         `Other 2024` = round(lv$r_2024[lv$group == "Other districts"], 1), `Other 2025` = round(lv$r_2025[lv$group == "Other districts"], 1),
         `Difference in change (pp)` = round(100 * co[1], 1), SE = round(100 * co[2], 1),
         `95% CI` = sprintf("%.1f to %.1f", 100 * (co[1] - 1.96 * co[2]), 100 * (co[1] + 1.96 * co[2])), p = round(co[4], 3),
         `Programme change SE` = round(100 * ch[2, 1], 2), `Other change SE` = round(100 * ch[2, 2], 2),
         `N girls, programme (2025)` = lv$n_2025[lv$group == "Programme districts"])
}))
A <- A |> mutate(`Programme change` = `Programme 2025` - `Programme 2024`, `Other change` = `Other 2025` - `Other 2024`)
write_csv(A, file.path(MOD, "followup3_A_programme_districts.csv"))
cat("\n=== A. Programme vs other districts, girls, East Java ===\n"); print(as.data.frame(A))

fa <- bind_rows(
  A |> transmute(Outcome, grp = "Programme districts", chg = `Programme change`, se = `Programme change SE`),
  A |> transmute(Outcome, grp = "Other districts", chg = `Other change`, se = `Other change SE`)) |>
  mutate(Outcome = factor(Outcome, names(outcomes)), lo = chg - 1.96 * se, hi = chg + 1.96 * se)
f29 <- ggplot(fa, aes(Outcome, chg, fill = grp)) +
  geom_col(position = position_dodge(.7), width = .62) + geom_hline(yintercept = 0, colour = "#374649") +
  geom_errorbar(aes(ymin = lo, ymax = hi), position = position_dodge(.7), width = .18, colour = "#374649", linewidth = .4) +
  geom_text(aes(y = ifelse(chg >= 0, hi, lo), label = sprintf("%+.1f", chg), vjust = ifelse(chg >= 0, -0.6, 1.5)),
            position = position_dodge(.7), size = 3.3, colour = "#374649") +
  scale_fill_manual(values = c("Programme districts" = "#E2007A", "Other districts" = "#00377C"), name = NULL) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, 14)) +
  scale_y_continuous(expand = expansion(mult = c(.15, .15))) +
  labs(x = NULL, y = "Change 2024 to 2025 (percentage points)",
       caption = "Bars: change in each group. Lines: 95% confidence intervals. Programme districts pooled.") +
  theme_minimal(base_size = 12) + theme(legend.position = "top", panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
                                        plot.caption = element_text(colour = "#7A8487", size = 8))
save_fig(f29, "f29_programme_districts_en", width = 9.5, height = 5.2)

# ===== B. what changed, East Java vs rest of Indonesia (girls) =====
g <- d |> filter(female, age >= 15, age <= 24)
rate <- function(df, cond, v) df |> filter({{ cond }}) |> group_by(unit, year) |>
  summarise(r = 100 * weighted.mean(.data[[v]], w, na.rm = TRUE), .groups = "drop")
lines <- list(
  list("NEET, 15-17", quote(age <= 17), "neet"), list("NEET, 18-21", quote(age >= 18 & age <= 21), "neet"),
  list("NEET, 22-24", quote(age >= 22), "neet"),
  list("NEET, 22-24 with tertiary certificate", quote(age >= 22 & tertiary_cert), "neet"),
  list("NEET, 22-24 without tertiary certificate", quote(age >= 22 & !tertiary_cert), "neet"),
  list("In school, 15-24", quote(TRUE), "in_school"), list("Employed (not in school), 15-24", quote(TRUE), "employed"))
B1 <- bind_rows(lapply(lines, function(l) rate(g, !!l[[2]], l[[3]]) |> mutate(Indicator = l[[1]])))
emp <- g |> filter(employed, !is.na(status)) |>
  mutate(own = status %in% 1:3, employee = status == 4, unpaid = status == 6, trade_transport = sector %in% c(12, 13))
B2 <- bind_rows(lapply(list(c("Own business, % of employed", "own"), c("Employee, % of employed", "employee"),
                            c("Unpaid family worker, % of employed", "unpaid"), c("Trade or transport sector, % of employed", "trade_transport")),
                       function(x) rate(emp, TRUE, x[2]) |> mutate(Indicator = x[1])))
B <- bind_rows(B1, B2) |> pivot_wider(names_from = c(unit, year), values_from = r) |>
  transmute(Indicator, `East Java 2024` = round(`East Java_2024`, 1), `East Java 2025` = round(`East Java_2025`, 1),
            `East Java change` = round(`East Java_2025` - `East Java_2024`, 1),
            `Rest of Indonesia change` = round(`Rest of Indonesia_2025` - `Rest of Indonesia_2024`, 1))
write_csv(B, file.path(MOD, "followup3_B_decomposition.csv"))
cat("\n=== B. What changed 2024 -> 2025, girls ===\n"); print(as.data.frame(B))
cat("\nSector codes among employed girls, East Java 2025 (12 = trade, 13 = transport in the VSEN code list):\n")
print(head(sort(table(emp$sector[emp$prov == 35 & emp$year == 2025]), decreasing = TRUE), 8))

# ===== C. pass-through: girls who leave school before completing senior secondary =====
pt <- d |> filter(prov == 35, year == 2025, female, age >= 16, age <= 20, !in_school, !(cert %in% 11:24))
des_pt <- svydesign(ids = ~psu, strata = ~strata, weights = ~w, data = pt |> mutate(n1 = as.numeric(neet)), nest = TRUE)
m <- svymean(~n1, des_pt)
write_csv(tibble(measure = "NEET share, girls 16-20 who left school without a senior-secondary certificate, East Java 2025",
                 value = round(coef(m)[1], 4), se = round(SE(m)[1], 4), n = nrow(pt)), file.path(MOD, "pass_through.csv"))
cat(sprintf("\n=== C. Pass-through: %.1f%% of girls 16-20 who left school without completing senior secondary are NEET (SE %.1f, n = %d)\n",
            100 * coef(m)[1], 100 * SE(m)[1], nrow(pt)))
message("17 done")
