# 03g_key_gaps_en.R — retarget the headline "outcomes by sex" figure (f1a)
# Leads with the transition gap (NEET, employment) where girls are worse;
# schooling shown last as the parity point. Overwrites f1a_outcomes_by_sex_en.png.
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(survey); library(here)
})
source(here::here("R","utils.R"))            # save_fig, ensure_dirs
source(here::here("R","theme.R"))
# colour fallbacks (theme.R may not define all of these)
if (!exists("GREY_MID"))  GREY_MID  <- "#7A8487"
if (!exists("GREY_DARK")) GREY_DARK <- "#374649"
if (!exists("ACCENT_GIRL")) ACCENT_GIRL <- "#E2007A"
if (!exists("ACCENT_BOY"))  ACCENT_BOY  <- "#374EA2"
if (!exists("UNICEF_BLUE")) UNICEF_BLUE <- "#1CABE2"
ensure_dirs(); options(survey.lonely.psu = "adjust")

ado <- readRDS(here::here("output","analysis_ready.rds")) |>
  filter(adolescent) |>
  mutate(NEET = as.numeric(neet), Employed = as.numeric(employed),
         `In school` = as.numeric(in_school),
         sex = factor(ifelse(female, "Girls", "Boys"), c("Girls","Boys")))

des <- svydesign(ids = ~psu, strata = ~strata, weights = ~w, data = ado, nest = TRUE)

one <- function(v) {
  f <- as.formula(paste0("~`", v, "`"))
  svyby(f, ~sex, des, svymean, na.rm = TRUE) |>
    as.data.frame() |>
    setNames(c("sex","estimate","se")) |>
    mutate(outcome = v)
}
dat <- bind_rows(lapply(c("NEET","Employed","In school"), one)) |>
  mutate(outcome = factor(outcome, levels = c("NEET","Employed","In school")),
         lo = pmax(0, estimate - 1.96*se), hi = estimate + 1.96*se)

pal <- c("Boys" = ACCENT_BOY, "Girls" = ACCENT_GIRL)
f1a <- ggplot(dat, aes(outcome, estimate, fill = sex)) +
  geom_col(position = position_dodge(.75), width = .68) +
  geom_errorbar(aes(ymin = lo, ymax = hi), position = position_dodge(.75),
                width = .16, colour = GREY_DARK, linewidth = .4) +
  geom_text(aes(y = hi, label = scales::percent(estimate, accuracy = 1)),
            position = position_dodge(.75), vjust = -0.8, size = 3.4, colour = GREY_DARK) +
  scale_fill_manual(values = pal, name = NULL) +
  scale_y_continuous(labels = scales::percent_format(1),
                     expand = expansion(mult = c(0, .20))) +
  labs(x = NULL, y = NULL,
       subtitle = "Girls match boys in school — but carry ~2× the NEET rate and far lower employment",
       caption = attr(ado,"vintage")) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(colour = GREY_DARK, margin = margin(b = 8)),
        plot.caption = element_text(colour = GREY_MID, size = 8),
        legend.position = "top")

save_fig(f1a, "f1a_outcomes_by_sex_en", width = 8.5, height = 5.6)
message("f1a regenerated → NEET / Employed / In school by sex")
print(dat |> transmute(sex, outcome, pct = round(100*estimate,1)))
