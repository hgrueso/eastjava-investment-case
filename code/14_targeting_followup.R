# 14_targeting_followup.R — reviewer follow-ups that need no new data.
#  A. Madura vs rest of East Java: NEET gender gap and child marriage (2025)
#  B. What "employment" means for girls married before 18 (status, hours) (2025)
#  C. Senior-secondary completion and NEET, restricted to girls who have left school (2025)
#  D. Child marriage by PKH receipt, a poverty-targeted programme (2025 poverty proxy)
#  E. Tertiary enrolment at 18-20, East Java vs national, 2024 and 2025
#  F. East Java vs national trend in girls' NEET and child marriage, 2024 to 2025
# Outputs: output/models/followup_*.csv, f20-f23 figures
suppressPackageStartupMessages({
  library(haven); library(dplyr); library(tidyr); library(ggplot2)
  library(survey); library(readr); library(here)
})
source(here::here("R","utils.R")); source(here::here("R","theme.R"))
if (!exists("GREY_MID"))  GREY_MID  <- "#7A8487"
if (!exists("GREY_DARK")) GREY_DARK <- "#374649"
if (!exists("ACCENT_GIRL")) ACCENT_GIRL <- "#E2007A"
if (!exists("ACCENT_BOY"))  ACCENT_BOY  <- "#374EA2"
if (!exists("UNICEF_DARK")) UNICEF_DARK <- "#00377C"
ensure_dirs(); options(survey.lonely.psu = "adjust")
MOD <- here::here("output","models")
num <- function(x) as.numeric(x)
wpct <- function(x, w) 100 * weighted.mean(x, w, na.rm = TRUE)

# ---------------------------------------------------------------------------
# Load 2025 (national) once
# ---------------------------------------------------------------------------
c25 <- c("R101","R102","R105","R404","R405","R407","R409","R611","R613","R615",
         "R703_A","R703_B","R704","R706","R707","R2003A","WEIND","PSU","STRATA")
r25 <- read_dta(here::here("data","Susenas 2025","SUSENAS_25-ALL-Merged.dta"), col_select = all_of(c25))
d25 <- tibble(
  prov = num(r25$R101), kab = num(r25$R102) %% 100, urban = num(r25$R105) == 1,
  female = num(r25$R405) == 2, age = num(r25$R407), w = num(r25$WEIND),
  psu = num(r25$PSU), strata = num(r25$STRATA),
  married_u18 = num(r25$R404) %in% 2:4 & num(r25$R409) %in% 1:17,
  in_school = as.character(r25$R703_B) == "B" | num(r25$R611) %in% 2,
  working   = as.character(r25$R703_A) == "A" | num(r25$R704) %in% 1,
  level_now = num(r25$R613), cert = num(r25$R615),
  status = num(r25$R706), hours = num(r25$R707),
  pkh = num(r25$R2003A) %in% 1
) |>
  mutate(employed = working & !in_school, neet = !in_school & !employed,
         completed_usec = cert %in% 11:24,
         tertiary_now = in_school & level_now %in% 18:24,
         region = ifelse(kab %in% 26:29, "Madura", "Rest of East Java"))
rm(r25)
ej <- d25 |> filter(prov == 35)
cat("East Java 2025 rows:", nrow(ej), "| Madura rows:", sum(ej$region == "Madura"),
    "(expect roughly 12-15% of East Java)\n")

# ===========================================================================
# A. Madura vs rest
# ===========================================================================
y <- ej |> filter(age >= 15, age <= 24)
A <- y |> group_by(region, sex = ifelse(female, "Girls", "Boys")) |>
  summarise(n = n(), NEET = wpct(neet, w), In_school = wpct(in_school, w),
            Employed = wpct(employed, w), Married_u18 = wpct(married_u18, w), .groups = "drop")
gapA <- A |> select(region, sex, NEET) |> pivot_wider(names_from = sex, values_from = NEET) |>
  mutate(`NEET gender gap (pp)` = round(Girls - Boys, 1))
write_csv(A |> mutate(across(where(is.numeric) & !n, ~ round(.x, 1))), file.path(MOD, "followup_A_madura.csv"))
cat("\n=== A. Madura vs rest of East Java (15-24, 2025, %) ===\n"); print(as.data.frame(A)); print(as.data.frame(gapA))

des_y <- svydesign(ids = ~psu, strata = ~strata, weights = ~w, data = y |> filter(female), nest = TRUE)
fA <- bind_rows(lapply(c("neet","married_u18"), function(v) {
  svyby(as.formula(paste0("~as.numeric(", v, ")")), ~region, des_y, svymean, na.rm = TRUE) |>
    as.data.frame() |> setNames(c("region","rate","se")) |>
    mutate(outcome = ifelse(v == "neet", "NEET", "Married before 18")) }))
f20 <- ggplot(fA, aes(region, rate, fill = region)) +
  geom_col(width = .55, show.legend = FALSE) +
  geom_errorbar(aes(ymin = pmax(0, rate - 1.96*se), ymax = rate + 1.96*se), width = .12, colour = GREY_DARK) +
  geom_text(aes(y = rate + 1.96*se, label = scales::percent(rate, accuracy = 1)), vjust = -0.7, size = 3.6, colour = GREY_DARK) +
  facet_wrap(~outcome, scales = "free_y") +
  scale_fill_manual(values = c("Madura" = ACCENT_GIRL, "Rest of East Java" = UNICEF_DARK)) +
  scale_y_continuous(labels = scales::percent_format(1), breaks = scales::breaks_pretty(n = 4), expand = expansion(mult = c(0, .22))) +
  labs(x = NULL, y = NULL, subtitle = "Girls 15-24: Madura (Bangkalan, Sampang, Pamekasan, Sumenep) vs rest of East Java",
       caption = "SUSENAS Maret 2025, survey-weighted; 95% CIs") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.subtitle = element_text(colour = GREY_DARK, margin = margin(b = 8)),
        plot.caption = element_text(colour = GREY_MID, size = 8))
save_fig(f20, "f20_madura_en", width = 9, height = 5)

# ===========================================================================
# B. Employment quality, girls married before 18
# ===========================================================================
emp <- y |> filter(female, employed, !is.na(status)) |>
  mutate(group = ifelse(married_u18, "Married before 18", "Not married before 18"),
         type = case_when(status == 4 ~ "Employee (wage)",
                          status %in% 1:3 ~ "Own business",
                          status == 5 ~ "Casual worker",
                          status == 6 ~ "Unpaid family worker"))
B <- emp |> group_by(group) |> mutate(tot = sum(w)) |>
  group_by(group, type) |> summarise(share = round(100 * sum(w) / first(tot), 1), n = n(), .groups = "drop")
Bh <- emp |> group_by(group) |> summarise(n_employed = n(), mean_hours = round(weighted.mean(hours, w, na.rm = TRUE), 1),
                                          share_under_20h = round(100 * weighted.mean(hours < 20, w, na.rm = TRUE), 1), .groups = "drop")
Breg <- y |> filter(female) |> mutate(group = ifelse(married_u18, "Married before 18", "Not married before 18")) |>
  group_by(region, group) |> summarise(n = n(), employed = round(wpct(employed, w), 1), .groups = "drop")
write_csv(B, file.path(MOD, "followup_B_employment_type.csv"))
write_csv(Bh, file.path(MOD, "followup_B_hours.csv"))
write_csv(Breg, file.path(MOD, "followup_B_employment_by_region.csv"))
cat("\n=== B. Type of work among employed girls 15-24 (2025, % of employed) ===\n"); print(as.data.frame(B))
cat("\nHours of work:\n"); print(as.data.frame(Bh))
cat("\nEmployment rate by region and marriage status (girls 15-24):\n"); print(as.data.frame(Breg))

f21 <- ggplot(B, aes(group, share, fill = type)) +
  geom_col(width = .6) +
  geom_text(aes(label = ifelse(share >= 5, paste0(round(share), "%"), "")),
            position = position_stack(vjust = .5), colour = "white", size = 3.6, fontface = "bold") +
  scale_fill_manual(values = c("Employee (wage)" = UNICEF_DARK, "Own business" = "#1CABE2",
                               "Casual worker" = "#7a5b00", "Unpaid family worker" = ACCENT_GIRL), name = NULL) +
  labs(x = NULL, y = "% of employed girls",
       subtitle = "Type of work among employed girls 15-24, by child-marriage status",
       caption = "SUSENAS Maret 2025, East Java, survey-weighted") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top", panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        plot.subtitle = element_text(colour = GREY_DARK, margin = margin(b = 8)),
        plot.caption = element_text(colour = GREY_MID, size = 8))
save_fig(f21, "f21_employment_type_en", width = 8.5, height = 5.4)

# ===========================================================================
# C. Completion and NEET among those who have LEFT school
# (the earlier 15-24 comparison put 15-17 year olds still in school, who
#  cannot be NEET, into the non-completer group, inverting the result)
# ===========================================================================
left <- ej |> filter(!in_school, age >= 18, age <= 24) |>
  mutate(sex = ifelse(female, "Girls", "Boys"),
         status_c = ifelse(completed_usec, "Completed senior secondary", "Did not complete"),
         cohort = ifelse(age <= 20, "18-20", "21-24"))
C <- bind_rows(
  left |> filter(cohort == "18-20") |> group_by(cohort, sex, status_c) |> summarise(n = n(), NEET = round(wpct(neet, w), 1), .groups = "drop"),
  left |> mutate(cohort = "18-24") |> group_by(cohort, sex, status_c) |> summarise(n = n(), NEET = round(wpct(neet, w), 1), .groups = "drop"))
des_c <- svydesign(ids = ~psu, strata = ~strata, weights = ~w,
                   data = left |> filter(female) |> mutate(comp = as.numeric(completed_usec), rur = as.numeric(!urban),
                                                           NEETn = as.numeric(neet)), nest = TRUE)
fitC <- svyglm(NEETn ~ comp + age + rur, des_c)
co <- summary(fitC)$coefficients["comp", ]
write_csv(C, file.path(MOD, "followup_C_completion.csv"))
cat("\n=== C. NEET among young people 18-24 who have left school, by completion (2025, %) ===\n"); print(as.data.frame(C))
cat(sprintf("Adjusted (girls 18-24 out of school; age, rural): completing senior secondary changes NEET by %.1fpp (SE %.1f, p=%.3f)\n",
            100*co[1], 100*co[2], co[4]))

f22 <- ggplot(C |> filter(cohort == "18-20"), aes(status_c, NEET/100, fill = sex)) +
  geom_col(position = position_dodge(.7), width = .6) +
  geom_text(aes(label = paste0(round(NEET), "%")), position = position_dodge(.7), vjust = -0.6, size = 3.6, colour = GREY_DARK) +
  scale_fill_manual(values = c("Girls" = ACCENT_GIRL, "Boys" = ACCENT_BOY), name = NULL) +
  scale_y_continuous(labels = scales::percent_format(1), expand = expansion(mult = c(0, .18))) +
  labs(x = NULL, y = NULL, subtitle = "NEET rate among 18-20 year olds who have left school, by senior-secondary completion",
       caption = "SUSENAS Maret 2025, East Java, survey-weighted") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top", panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        plot.subtitle = element_text(colour = GREY_DARK, margin = margin(b = 8)),
        plot.caption = element_text(colour = GREY_MID, size = 8))
save_fig(f22, "f22_completion_left_school_en", width = 8.5, height = 5.2)

# ===========================================================================
# D. Child marriage by PKH receipt (PKH targets poor households)
# ===========================================================================
D <- y |> filter(female) |> group_by(region, PKH = ifelse(pkh, "PKH household", "Not PKH")) |>
  summarise(n = n(), Married_u18 = round(wpct(married_u18, w), 1), NEET = round(wpct(neet, w), 1), .groups = "drop")
write_csv(D, file.path(MOD, "followup_D_pkh.csv"))
cat("\n=== D. Girls 15-24 by household PKH receipt (2025, %) — PKH is poverty-targeted, so this reflects poverty, not a programme effect ===\n")
print(as.data.frame(D))

# ===========================================================================
# E/F. East Java vs national, 2024 and 2025
# ===========================================================================
c24 <- c("R101","R404","R405","R407","R409","R610","R612","R704","FWT")
r24 <- read_dta(here::here("data","Susenas 2024","ssn202403_kor_ind1.dta"), col_select = all_of(c24))
d24 <- tibble(prov = num(r24$R101), female = num(r24$R405) == 2, age = num(r24$R407), w = num(r24$FWT),
              married_u18 = num(r24$R404) %in% 2:4 & num(r24$R409) %in% 1:17,
              in_school = num(r24$R610) %in% 2 | num(r24$R704) %in% 2,
              employed = num(r24$R704) %in% 1,
              tertiary_now = num(r24$R610) %in% 2 & num(r24$R612) %in% 18:24) |>
  mutate(employed = employed & !in_school, neet = !in_school & !employed)
rm(r24)

summ <- function(d, yr) {
  d <- d |> filter(age >= 15, age <= 24) |> mutate(unit = ifelse(prov == 35, "East Java", "Rest of Indonesia"))
  bind_rows(d, d |> mutate(unit = "Indonesia (national)")) |>
    group_by(unit) |>
    summarise(girls_NEET = wpct(neet[female], w[female]), boys_NEET = wpct(neet[!female], w[!female]),
              girls_married_u18 = wpct(married_u18[female], w[female]),
              girls_tertiary_18_20 = wpct(tertiary_now[female & age <= 20 & age >= 18], w[female & age <= 20 & age >= 18]),
              .groups = "drop") |>
    mutate(year = yr, gap = girls_NEET - boys_NEET)
}
EF <- bind_rows(summ(d24, 2024), summ(d25, 2025)) |> mutate(across(where(is.double) & !year, ~ round(.x, 1)))
write_csv(EF, file.path(MOD, "followup_EF_trend_national.csv"))
chg <- EF |> select(unit, year, girls_NEET, girls_married_u18, girls_tertiary_18_20) |>
  pivot_wider(names_from = year, values_from = c(girls_NEET, girls_married_u18, girls_tertiary_18_20)) |>
  mutate(NEET_change = girls_NEET_2025 - girls_NEET_2024,
         married_change = girls_married_u18_2025 - girls_married_u18_2024,
         tertiary_change = girls_tertiary_18_20_2025 - girls_tertiary_18_20_2024)
write_csv(chg, file.path(MOD, "followup_EF_changes.csv"))
cat("\n=== E/F. East Java vs national, 2024 and 2025 (%) ===\n"); print(as.data.frame(EF))
cat("\nChange 2024 -> 2025 (pp):\n"); print(as.data.frame(chg |> select(unit, NEET_change, married_change, tertiary_change)))
cat("NOTE: 2025 NEET uses the multi-select activity block (R703) because the single main-activity question was dropped;\n",
    "small definitional differences between years apply equally to East Java and the rest of Indonesia, so the\n",
    "comparison of CHANGES between them is more robust than either year-on-year level change on its own.\n")

f23 <- EF |> filter(unit != "Indonesia (national)") |>
  ggplot(aes(factor(year), girls_NEET/100, colour = unit, group = unit)) +
  geom_line(linewidth = 1.2) + geom_point(size = 3) +
  geom_text(aes(label = paste0(girls_NEET, "%")), vjust = -1.1, size = 3.6, show.legend = FALSE) +
  scale_colour_manual(values = c("East Java" = ACCENT_GIRL, "Rest of Indonesia" = GREY_DARK), name = NULL) +
  scale_y_continuous(labels = scales::percent_format(1), expand = expansion(mult = c(.15, .2))) +
  labs(x = NULL, y = NULL, subtitle = "Girls' NEET rate (15-24), East Java vs rest of Indonesia",
       caption = "SUSENAS Maret 2024 and Maret 2025, survey-weighted") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        plot.subtitle = element_text(colour = GREY_DARK, margin = margin(b = 8)),
        plot.caption = element_text(colour = GREY_MID, size = 8))
save_fig(f23, "f23_trend_ej_vs_national_en", width = 8, height = 5)
message("14 done -> followup_*.csv, f20-f23")
