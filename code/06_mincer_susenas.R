# 06_mincer_susenas.R — returns to schooling on per-capita CONSUMPTION (SUSENAS 2024)
# SUSENAS collects no wages, so we estimate the welfare return to education using
# household per-capita expenditure (KAPITA) as the outcome. Prime-age adults 25-54,
# survey-weighted, PSU-clustered SEs. A positive quadratic term => convex returns.
# Outputs: output/models/mincer_susenas.csv + output/figures/f10_consumption_returns_en.png
suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(survey); library(here)
})
source(here::here("R","utils.R")); source(here::here("R","theme.R"))
if (!exists("GREY_MID"))  GREY_MID  <- "#7A8487"
if (!exists("GREY_DARK")) GREY_DARK <- "#374649"
if (!exists("ACCENT_GIRL")) ACCENT_GIRL <- "#E2007A"
if (!exists("ACCENT_BOY"))  ACCENT_BOY  <- "#374EA2"
ensure_dirs(); options(survey.lonely.psu = "adjust")

d <- readRDS(here::here("output","analysis_ready_full.rds")) |>
  filter(age >= 25, age <= 54, !is.na(years_school), !is.na(kapita), kapita > 0) |>
  mutate(lconsum = log(kapita), age2 = age^2, urban_n = as.numeric(urban %in% TRUE),
         yrs2 = years_school^2,
         sex = factor(ifelse(female, "Women", "Men"), c("Women","Men")))
des <- svydesign(ids = ~psu, strata = ~strata, weights = ~w, data = d, nest = TRUE)

pull1 <- function(fit, term){ co <- summary(fit)$coefficients
  if (term %in% rownames(co)) co[term, c("Estimate","Std. Error","Pr(>|t|)")] else c(NA,NA,NA) }

# linear return + convexity (quadratic), overall and by sex
fit_lin <- svyglm(lconsum ~ years_school + age + age2 + urban_n + female, design = des)
fit_quad<- svyglm(lconsum ~ years_school + yrs2 + age + age2 + urban_n + female, design = des)
fit_f   <- svyglm(lconsum ~ years_school + age + age2 + urban_n, design = subset(des, female))
fit_m   <- svyglm(lconsum ~ years_school + age + age2 + urban_n, design = subset(des, !female))
fit_quad_f <- svyglm(lconsum ~ years_school + yrs2 + age + age2 + urban_n, design = subset(des, female))
fit_quad_m <- svyglm(lconsum ~ years_school + yrs2 + age + age2 + urban_n, design = subset(des, !female))

tab <- tibble::tribble(~Model, ~Term, ~Estimate, ~SE, ~p,
  "Overall (linear)","years_school", pull1(fit_lin,"years_school")[1], pull1(fit_lin,"years_school")[2], pull1(fit_lin,"years_school")[3],
  "Overall (quadratic)","years_school", pull1(fit_quad,"years_school")[1], pull1(fit_quad,"years_school")[2], pull1(fit_quad,"years_school")[3],
  "Overall (quadratic)","years_school^2", pull1(fit_quad,"yrs2")[1], pull1(fit_quad,"yrs2")[2], pull1(fit_quad,"yrs2")[3],
  "Women","years_school", pull1(fit_f,"years_school")[1], pull1(fit_f,"years_school")[2], pull1(fit_f,"years_school")[3],
  "Men","years_school", pull1(fit_m,"years_school")[1], pull1(fit_m,"years_school")[2], pull1(fit_m,"years_school")[3],
  "Women (quadratic)","years_school", pull1(fit_quad_f,"years_school")[1], pull1(fit_quad_f,"years_school")[2], pull1(fit_quad_f,"years_school")[3],
  "Women (quadratic)","years_school^2", pull1(fit_quad_f,"yrs2")[1], pull1(fit_quad_f,"yrs2")[2], pull1(fit_quad_f,"yrs2")[3],
  "Men (quadratic)","years_school", pull1(fit_quad_m,"years_school")[1], pull1(fit_quad_m,"years_school")[2], pull1(fit_quad_m,"years_school")[3],
  "Men (quadratic)","years_school^2", pull1(fit_quad_m,"yrs2")[1], pull1(fit_quad_m,"yrs2")[2], pull1(fit_quad_m,"yrs2")[3])
readr::write_csv(tab, here::here("output","models","mincer_susenas.csv"))

# --- convexity figure: weighted mean per-capita consumption by years of schooling x sex
gm <- d |> group_by(sex, years_school) |>
  summarise(mean_kapita = weighted.mean(kapita, w, na.rm = TRUE),
            n = n(), .groups = "drop") |> filter(n >= 30)
f10 <- ggplot(gm, aes(years_school, mean_kapita/1e6, colour = sex)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_colour_manual(values = c("Women"=ACCENT_GIRL, "Men"=ACCENT_BOY), name = NULL) +
  scale_x_continuous(breaks = c(0,6,9,12,16,18,21)) +
  labs(x = "Years of schooling (highest certificate)",
       y = "Mean per-capita expenditure (million IDR/month)",
       subtitle = "Consumption rises with schooling — steepening at senior-secondary and tertiary (convex)",
       caption = "SUSENAS Maret 2024, East Java; survey-weighted; adults 25-54") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = "top",
        plot.subtitle = element_text(colour = GREY_DARK, margin = margin(b = 8)),
        plot.caption = element_text(colour = GREY_MID, size = 8))
save_fig(f10, "f10_consumption_returns_en", width = 8.5, height = 5.4)

cat("\n=== Returns to schooling on log per-capita consumption (SUSENAS 2024) ===\n")
print(tab |> mutate(across(c(Estimate,SE,p), ~round(.,4))))
r <- pull1(fit_lin,"years_school")[1]; q <- pull1(fit_quad,"yrs2")[1]
cat(sprintf("\nLinear: each extra year of schooling ~ %.1f%% higher per-capita consumption.\n", 100*r))
cat(sprintf("Convexity: years_school^2 coefficient = %.5f (%s) -> returns are %s.\n",
    q, ifelse(pull1(fit_quad,"yrs2")[3] < .05, "significant","n.s."),
    ifelse(q > 0, "CONVEX (increasing)", "concave/linear")))
# --- f14: predicted consumption by years of schooling, girls vs boys ---------
# Mirrors f8 (IFLS wage-Mincer): fitted values from the by-sex regressions,
# holding age at the sample mean, so the two figures are directly comparable.
age_fix <- round(mean(d$age))
grid <- expand.grid(years_school = 0:18, age = age_fix, urban_n = 1) |> mutate(age2 = age^2, yrs2 = years_school^2)
predC <- function(m) exp(predict(m, newdata = grid))
plot_df <- bind_rows(
  tibble(sex="Women", years_school=grid$years_school, kapita=predC(fit_quad_f)),
  tibble(sex="Men",   years_school=grid$years_school, kapita=predC(fit_quad_m)))
retF <- pull1(fit_f,"years_school")[1]; retM <- pull1(fit_m,"years_school")[1]
ann <- tibble(sex=c("Women","Men"),
  lab=c(sprintf("Women: +%.1f%% / yr", 100*retF), sprintf("Men: +%.1f%% / yr", 100*retM)),
  x=1, y=c(max(plot_df$kapita)*.95, max(plot_df$kapita)*.82))
f14 <- ggplot(plot_df, aes(years_school, kapita/1e6, colour=sex)) +
  geom_line(linewidth=1.3) +
  geom_text(data=ann, aes(x, y/1e6, label=lab, colour=sex), hjust=0, fontface="bold",
            size=4.3, show.legend=FALSE) +
  scale_colour_manual(values=c("Women"=ACCENT_GIRL,"Men"=ACCENT_BOY), name=NULL) +
  scale_x_continuous(breaks=seq(0,16,2)) +
  labs(subtitle=sprintf("Model-based, age-adjusted (age held at %d) — includes the quadratic term", age_fix),
       x="Years of schooling", y="Predicted monthly per-capita consumption (million IDR)",
       caption="SUSENAS Maret 2024, East Java. Same Mincer specification as the IFLS5 wage regression (f8), estimated on consumption instead of wages.") +
  theme_minimal(base_size=13) +
  theme(legend.position="top", panel.grid.minor=element_blank(),
        plot.subtitle=element_text(colour=GREY_DARK, margin=margin(b=8)),
        plot.caption=element_text(colour=GREY_MID, size=8))
save_fig(f14, "f14_consumption_mincer_bysex_en", width=9, height=5.4)

message("06_mincer_susenas done → mincer_susenas.csv, f10_consumption_returns_en.png, f14_consumption_mincer_bysex_en.png")
