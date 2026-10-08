# 15_followup2.R: second round of reviewer follow-ups.
#  A. Child marriage and outcomes (NEET, in school, employed), Madura vs rest, with a
#     regression test of marriage -> employment in each region
#  B. Senior-secondary enrolment (16-18), East Java vs rest of Indonesia, 2024 and 2025
#  C. Monthly earnings by type of work, young women (IFLS5, the only source with earnings)
suppressPackageStartupMessages({ library(haven); library(dplyr); library(tidyr); library(ggplot2); library(survey); library(readr); library(here) })
source(here::here("R","utils.R")); source(here::here("R","theme.R"))
if (!exists("GREY_MID"))  GREY_MID  <- "#7A8487"
if (!exists("GREY_DARK")) GREY_DARK <- "#374649"
if (!exists("ACCENT_GIRL")) ACCENT_GIRL <- "#E2007A"
if (!exists("ACCENT_BOY"))  ACCENT_BOY  <- "#374EA2"
if (!exists("UNICEF_DARK")) UNICEF_DARK <- "#00377C"
ensure_dirs(); options(survey.lonely.psu = "adjust"); MOD <- here::here("output","models")
num <- function(x) as.numeric(x); wpct <- function(x, w) 100 * weighted.mean(x, w, na.rm = TRUE)
theme_ej <- theme_minimal(base_size = 13) + theme(legend.position = "top", panel.grid.major.x = element_blank(),
  panel.grid.minor = element_blank(), plot.subtitle = element_text(colour = GREY_DARK, margin = margin(b = 8)),
  plot.caption = element_text(colour = GREY_MID, size = 8), strip.text = element_text(face = "bold"))

# ---- load 2025 national ----
c25 <- c("R101","R102","R105","R404","R405","R407","R409","R611","R613","R703_A","R703_B","R704","WEIND","PSU","STRATA")
r <- read_dta(here::here("data","Susenas 2025","SUSENAS_25-ALL-Merged.dta"), col_select = all_of(c25))
d25 <- tibble(prov = num(r$R101), kab = num(r$R102) %% 100, urban = num(r$R105) == 1, female = num(r$R405) == 2,
  age = num(r$R407), w = num(r$WEIND), psu = num(r$PSU), strata = num(r$STRATA),
  married_u18 = num(r$R404) %in% 2:4 & num(r$R409) %in% 1:17,
  in_school = as.character(r$R703_B) == "B" | num(r$R611) %in% 2,
  working = as.character(r$R703_A) == "A" | num(r$R704) %in% 1, level = num(r$R613)) |>
  mutate(employed = working & !in_school, neet = !in_school & !employed,
         sec_enrol = in_school & level %in% 11:17, region = ifelse(kab %in% 26:29, "Madura", "Rest of East Java"))
rm(r)

# ===== A. marriage x outcomes by region =====
g <- d25 |> filter(prov == 35, female, age >= 15, age <= 24) |>
  mutate(grp = ifelse(married_u18, "Married before 18", "Not married before 18"))
des <- svydesign(ids = ~psu, strata = ~strata, weights = ~w, data = g, nest = TRUE)
A <- bind_rows(lapply(c("neet","in_school","employed"), function(v)
  svyby(as.formula(paste0("~as.numeric(", v, ")")), ~region + grp, des, svymean, na.rm = TRUE) |> as.data.frame() |>
    setNames(c("region","grp","rate","se")) |> mutate(outcome = c(neet="NEET", in_school="In school", employed="Employed")[v])))
A <- A |> mutate(outcome = factor(outcome, c("NEET","In school","Employed")), lo = pmax(0, rate - 1.96*se), hi = pmin(1, rate + 1.96*se))
ns <- g |> count(region, grp); write_csv(A |> left_join(ns, by = c("region","grp")), file.path(MOD, "followup2_A_marriage_by_region.csv"))
cat("\n=== A. Outcomes by child marriage status and region (girls 15-24, 2025) ===\n"); print(A |> mutate(across(c(rate,se), ~round(100*.x,1))) |> as.data.frame())
for (rg in c("Madura","Rest of East Java")) {
  f <- ggplot(A |> filter(region == rg), aes(outcome, rate, fill = grp)) +
    geom_col(position = position_dodge(.72), width = .62) +
    geom_errorbar(aes(ymin = lo, ymax = hi), position = position_dodge(.72), width = .15, colour = GREY_DARK, linewidth = .4) +
    geom_text(aes(y = hi, label = scales::percent(rate, accuracy = 1)), position = position_dodge(.72), vjust = -0.8, size = 3.3, colour = GREY_DARK) +
    scale_fill_manual(values = c("Not married before 18" = "#1CABE2", "Married before 18" = ACCENT_GIRL), name = NULL) +
    scale_y_continuous(labels = scales::percent_format(1), expand = expansion(mult = c(0, .22))) +
    labs(x = NULL, y = NULL, subtitle = paste0("Girls 15-24, ", rg), caption = "SUSENAS Maret 2025, survey-weighted; 95% CIs") + theme_ej
  save_fig(f, paste0("f24_marriage_", ifelse(rg == "Madura", "madura", "rest"), "_en"), width = 8.5, height = 5.2)
}
cf <- function(fit, term) { co <- summary(fit)$coefficients; co[term, c("Estimate","Std. Error","Pr(>|t|)")] }
tests <- bind_rows(lapply(c("Madura","Rest of East Java","All East Java"), function(rg) {
  dd <- if (rg == "All East Java") des else subset(des, region == rg)
  bind_rows(lapply(c("employed","neet","in_school"), function(v) {
    fit <- svyglm(as.formula(paste0("as.numeric(", v, ") ~ as.numeric(married_u18) + age + as.numeric(!urban)")), dd)
    x <- cf(fit, "as.numeric(married_u18)")
    tibble(Region = rg, Outcome = c(employed="Employed", neet="NEET", in_school="In school")[v],
           `Effect (pp)` = round(100*x[1],1), SE = round(100*x[2],1), p = round(x[3],3), N = nobs(fit)) })) }))
write_csv(tests, file.path(MOD, "followup2_A_marriage_tests_by_region.csv"))
cat("\n=== A. Marriage -> outcome, by region (age and urban/rural controls) ===\n"); print(as.data.frame(tests))

# ===== B. senior-secondary enrolment 16-18, EJ vs rest, 2024 and 2025 =====
c24 <- c("R101","R405","R407","R610","R612","FWT")
r <- read_dta(here::here("data","Susenas 2024","ssn202403_kor_ind1.dta"), col_select = all_of(c24))
d24 <- tibble(prov = num(r$R101), female = num(r$R405) == 2, age = num(r$R407), w = num(r$FWT),
              sec_enrol = num(r$R610) %in% 2 & num(r$R612) %in% 11:17); rm(r)
summ <- function(d, yr) d |> filter(age >= 16, age <= 18) |> mutate(unit = ifelse(prov == 35, "East Java", "Rest of Indonesia")) |>
  group_by(unit, sex = ifelse(female, "Girls", "Boys")) |> summarise(rate = wpct(sec_enrol, w), .groups = "drop") |> mutate(year = yr)
B <- bind_rows(summ(d24, 2024), summ(d25, 2025)); write_csv(B |> mutate(rate = round(rate, 1)), file.path(MOD, "followup2_B_secondary_enrolment.csv"))
cat("\n=== B. Senior-secondary enrolment, 16-18 (%) ===\n"); print(B |> pivot_wider(names_from = year, values_from = rate) |> mutate(change = round(`2025` - `2024`, 1), across(c(`2024`,`2025`), ~round(.x,1))) |> as.data.frame())
f <- ggplot(B |> filter(sex == "Girls"), aes(factor(year), rate/100, colour = unit, group = unit)) +
  geom_line(linewidth = 1.2) + geom_point(size = 3) +
  geom_text(aes(label = sprintf("%.1f%%", rate)), vjust = -1.1, size = 3.6, show.legend = FALSE) +
  scale_colour_manual(values = c("East Java" = ACCENT_GIRL, "Rest of Indonesia" = GREY_DARK), name = NULL) +
  scale_y_continuous(labels = scales::percent_format(1), expand = expansion(mult = c(.15, .25))) +
  labs(x = NULL, y = NULL, subtitle = "Girls 16-18 enrolled in senior secondary, East Java vs rest of Indonesia",
       caption = "SUSENAS Maret 2024 and 2025, survey-weighted") + theme_ej
save_fig(f, "f25_secondary_enrolment_trend_en", width = 8, height = 5)

# ===== C. earnings by type of work, young women (IFLS5) =====
# Employees and casual workers report wages (tk25a1); the self-employed report
# net profit in a separate question (tk26a*), so each type uses its own measure.
IFLS <- here::here("data","IFLS5")
cov <- read_dta(file.path(IFLS,"hh14_b3a_dta","b3a_cov.dta")); tk <- read_dta(file.path(IFLS,"hh14_b3a_dta","b3a_tk2.dta"))
st <- grep("^tk24a$", names(tk), value = TRUE)[1]
pf <- c(grep("^tk26a1$", names(tk), value = TRUE), grep("^tk26a", names(tk), value = TRUE))[1]
lb <- function(v) { a <- attr(tk[[v]], "label"); if (is.null(a)) "" else a }
cat("\nIFLS5 status:", st, "| wage: tk25a1 (", lb("tk25a1"), ") | profit:", pf, "(", if (!is.na(pf)) lb(pf) else "not found", ")\n")
yw <- tibble(pidlink = cov$pidlink, age = num(cov$age), female = cov$sex == 3) |>
  inner_join(tibble(pidlink = tk$pidlink, code = num(tk[[st]]), wage = num(tk$tk25a1),
                    profit = if (!is.na(pf)) num(tk[[pf]]) else NA_real_), by = "pidlink") |>
  filter(female, age >= 15, age <= 30, code %in% 1:8) |>
  mutate(type = case_when(code %in% 4:5 ~ "Employee (wage)", code %in% 1:3 ~ "Own business",
                          code %in% 7:8 ~ "Casual worker", code == 6 ~ "Unpaid family worker"),
         earn = case_when(type == "Own business" ~ profit, type == "Unpaid family worker" ~ 0, TRUE ~ wage),
         earn = ifelse(earn < 0 | earn > 9e8, NA, earn))
C <- yw |> group_by(Type = type) |>
  summarise(`Women 15-30` = n(), `With earnings reported` = sum(!is.na(earn)),
            `Median monthly earnings (IDR)` = median(earn, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(`Median monthly earnings (IDR)`))
write_csv(C, file.path(MOD, "followup2_C_earnings_by_status_ifls.csv"))
cat("\n=== C. Women 15-30, IFLS5 (2014/15 prices): median monthly earnings by type of work ===\n"); print(as.data.frame(C))

# Redraw f21 (type of work by child-marriage status) with median earnings in the legend
B <- read_csv(file.path(MOD, "followup_B_employment_type.csv"), show_col_types = FALSE)
med <- setNames(C$`Median monthly earnings (IDR)`, C$Type)
lab <- function(t) { m <- med[t]
  if (t == "Unpaid family worker") return(paste0(t, ": no earnings"))
  if (is.na(m)) return(t); paste0(t, ": Rp", formatC(m/1e6, format = "f", digits = 2), "m/month") }
types <- c("Employee (wage)","Own business","Casual worker","Unpaid family worker")
labs_t <- setNames(sapply(types, lab), types)
f21 <- ggplot(B, aes(group, share, fill = type)) +
  geom_col(width = .6) +
  geom_text(aes(label = ifelse(share >= 5, paste0(round(share), "%"), "")),
            position = position_stack(vjust = .5), colour = "white", size = 3.6, fontface = "bold") +
  scale_fill_manual(values = c("Employee (wage)" = UNICEF_DARK, "Own business" = "#1CABE2",
                               "Casual worker" = "#7a5b00", "Unpaid family worker" = ACCENT_GIRL),
                    labels = labs_t, breaks = types, name = NULL) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(x = NULL, y = "% of employed girls",
       subtitle = "Type of work among employed girls 15-24, by child-marriage status",
       caption = "SUSENAS Maret 2025, East Java, survey-weighted. Legend: median monthly earnings of women 15-30 by type of work, IFLS5 (2014/15 prices).") +
  theme_ej
save_fig(f21, "f21_employment_type_en", width = 8.5, height = 5.6)
message("15 done")
