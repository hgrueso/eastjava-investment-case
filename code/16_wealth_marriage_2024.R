# 16_wealth_marriage_2024.R: child marriage by household expenditure quintile,
# girls 15-24, SUSENAS 2024 (the latest year with the expenditure module).
suppressPackageStartupMessages({ library(dplyr); library(ggplot2); library(survey); library(readr); library(here) })
source(here::here("R","utils.R")); source(here::here("R","theme.R")); ensure_dirs(); options(survey.lonely.psu = "adjust")
g <- readRDS(here::here("output","analysis_ready_2024.rds")) |>
  filter(female %in% c(TRUE, 1), !is.na(wealth_q)) |> mutate(married = as.numeric(married_u18 %in% TRUE))
des <- svydesign(ids = ~psu, strata = ~strata, weights = ~w, data = g, nest = TRUE)
q <- svyby(~married, ~wealth_q, des, svymean, na.rm = TRUE) |> as.data.frame() |> setNames(c("wealth_q","rate","se")) |>
  mutate(lo = pmax(0, rate - 1.96*se), hi = rate + 1.96*se)
write_csv(q |> mutate(across(c(rate, se, lo, hi), ~ round(100*.x, 1))), here::here("output","models","marriage_by_wealth_2024.csv"))
print(q |> mutate(across(c(rate, lo, hi), ~ round(100*.x, 1))))
f <- ggplot(q, aes(wealth_q, rate)) +
  geom_col(fill = "#E2007A", width = .65) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = .15, colour = "#374649", linewidth = .4) +
  geom_text(aes(y = hi, label = scales::percent(rate, accuracy = .1)), vjust = -0.8, size = 3.6, colour = "#374649") +
  scale_y_continuous(labels = scales::percent_format(1), expand = expansion(mult = c(0, .2))) +
  labs(x = "Household per-capita expenditure quintile (Q1 = poorest)", y = NULL,
       subtitle = "Girls 15-24 married before 18, by household expenditure quintile",
       caption = "SUSENAS Maret 2024 (latest year with expenditure data), East Java, survey-weighted; 95% CIs") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        plot.subtitle = element_text(colour = "#374649"), plot.caption = element_text(colour = "#7A8487", size = 8))
save_fig(f, "f11_marriage_by_wealth_2024_en", width = 8.5, height = 5)
message("16 done")
