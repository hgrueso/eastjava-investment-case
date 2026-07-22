# 04_mincer_ifls.R — Mincer returns to schooling (IFLS5), girls vs boys ---
# ln(wage) = b0 + b_ed*years_school + b1*exp + b2*exp^2, by sex (weighted).
# Outputs: output/models/mincer_ifls.csv  +  output/figures/f8_mincer_earnings_en.png
#
# IFLS specifics (verified against labels): sex 1=male,3=female;
#   wage = tk25a1 (net monthly, job 1; 9999999x = missing codes);
#   level = dl06; grade = dl07 (7 = graduated).
# ------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(haven); library(dplyr); library(tidyr); library(stringr)
  library(here); library(glue); library(fixest); library(ggplot2); library(scales)
})
source(here::here("R","utils.R")); source(here::here("R","theme.R")); ensure_dirs()
rd <- function(b,f) read_dta(file.path(here::here("data"), b, f)) |>
  mutate(across(where(\(x) inherits(x,"haven_labelled")), as.numeric))

# ---- CONFIG (verified against IFLS5 labels) -----------------------------
WAGE_VAR  <- "tk25a1"          # net monthly wage, job 1
WAGE_MAX  <- 9e8               # drop 9999999x missing codes
EDU_LEVEL <- "dl06"            # highest level attended
EDU_GRADE <- "dl07"            # highest grade (7 = graduated)
AGE_LO <- 20L; AGE_HI <- 60L   # prime working age for the Mincer

# years BEFORE entering each dl06 level
PREV_YEARS <- c("2"=0,"72"=0,            # SD / MI (elementary)
                "3"=6,"4"=6,"73"=6,      # SMP / SMP-vocational / MTs
                "5"=9,"6"=9,"74"=9,      # SMA / SMK / MA
                "60"=12,"61"=12,         # diploma (D1-D3) / S1
                "62"=16,"63"=18,         # S2 / S3
                "15"=0,"17"=0,"95"=0)    # adult education / other
# full length of each level (used when dl07 == 7 "graduated")
GRADE_IF_DONE <- c("2"=6,"72"=6,
                   "3"=3,"4"=3,"73"=3,
                   "5"=3,"6"=3,"74"=3,
                   "60"=3,"61"=4,"62"=2,"63"=3,"15"=0,"17"=0,"95"=0)

# ---- assemble person-level wage + schooling + demographics --------------
cov <- rd("hh14_b3a_dta","b3a_cov.dta") |>
  transmute(pidlink, age = as.numeric(age), female = sex == 3)
dl  <- rd("hh14_b3a_dta","b3a_dl1.dta")
tk  <- rd("hh14_b3a_dta","b3a_tk2.dta")
message("wage variable: ", WAGE_VAR)

edu <- tibble(pidlink = dl$pidlink,
              lvl = as.character(dl[[EDU_LEVEL]]),
              g   = suppressWarnings(as.numeric(dl[[EDU_GRADE]]))) |>
  mutate(
    grade = case_when(g == 7      ~ GRADE_IF_DONE[lvl],   # graduated -> full level
                      g %in% 0:6  ~ g,                     # in-progress grade
                      TRUE        ~ NA_real_),
    years_school = PREV_YEARS[lvl] + ifelse(is.na(grade), 0, grade)
  ) |>
  select(pidlink, lvl, years_school)

wag <- tibble(pidlink = tk$pidlink,
              wage = suppressWarnings(as.numeric(tk[[WAGE_VAR]]))) |>
  mutate(wage = ifelse(wage > 0 & wage < WAGE_MAX, wage, NA))

ptr  <- rd("hh14_trk_dta","ptrack.dta")
wcol <- grep("^pwt?14", names(ptr), value = TRUE); wcol <- c(wcol[grepl("x",wcol)], wcol)[1]
wt   <- tibble(pidlink = ptr$pidlink, w = if (!is.na(wcol)) as.numeric(ptr[[wcol]]) else 1)

d <- cov |> left_join(edu, "pidlink") |> left_join(wag, "pidlink") |>
  left_join(wt, "pidlink") |>
  mutate(exp = pmax(age - years_school - 6, 0), exp2 = exp^2) |>
  filter(age >= AGE_LO, age <= AGE_HI, !is.na(years_school), wage > 0, is.finite(wage))

# ---- sanity-check prints ------------------------------------------------
message("\n-- education level (dl06) -> derived years_school --")
print(d |> count(lvl, years_school) |> arrange(desc(n)) |> head(14))
message("\n-- wage summary (IDR, ", WAGE_VAR, ") --"); print(summary(d$wage))
message(glue("\nMincer sample: {nrow(d)}  (girls {sum(d$female)}, boys {sum(!d$female)})"))

# ---- estimate Mincer by sex --------------------------------------------
mF <- feols(log(wage) ~ years_school + exp + exp2, data = filter(d, female),  weights = ~w)
mM <- feols(log(wage) ~ years_school + exp + exp2, data = filter(d, !female), weights = ~w)
retF <- coef(mF)["years_school"]; retM <- coef(mM)["years_school"]
message(glue("Return to one extra year -- girls {sprintf('%.1f%%',100*retF)} | boys {sprintf('%.1f%%',100*retM)}"))

bind_rows(tibble(sex="Girls", term=names(coef(mF)), estimate=coef(mF), se=se(mF)),
          tibble(sex="Boys",  term=names(coef(mM)), estimate=coef(mM), se=se(mM))) |>
  readr::write_csv(here::here("output","models","mincer_ifls.csv"))

# ---- plot predicted monthly earnings vs years of schooling --------------
exp_fix <- round(mean(d$exp))
grid <- expand.grid(years_school = 0:16, exp = exp_fix) |> mutate(exp2 = exp^2)
predW <- function(m) exp(predict(m, newdata = grid))
plot_df <- bind_rows(
  tibble(sex="Girls", years_school = grid$years_school, wage = predW(mF)),
  tibble(sex="Boys",  years_school = grid$years_school, wage = predW(mM)))

ann <- tibble(sex = c("Girls","Boys"),
              lab = c(sprintf("Girls: +%.1f%% / yr", 100*retF),
                      sprintf("Boys: +%.1f%% / yr",  100*retM)),
              x = 1, y = c(max(plot_df$wage)*.95, max(plot_df$wage)*.82))

f8 <- ggplot(plot_df, aes(years_school, wage, colour = sex)) +
  geom_line(linewidth = 1.3) +
  geom_text(data = ann, aes(x, y, label = lab, colour = sex),
            hjust = 0, fontface = "bold", size = 4.3, show.legend = FALSE) +
  scale_colour_manual(values = c("Girls"=ACCENT_GIRL, "Boys"=ACCENT_BOY), name = NULL) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale(), prefix = "Rp ")) +
  scale_x_continuous(breaks = seq(0,16,2)) +
  labs(subtitle = glue("Predicted monthly earnings by years of schooling (experience held at {exp_fix} yrs)"),
       x = "Years of schooling", y = "Predicted monthly wage",
       caption = glue("Source: IFLS5 2014/15, East Java. Mincer log-wage regression, weighted; ages {AGE_LO}-{AGE_HI}, wage earners. Preliminary.")) +
  theme(legend.position = "top")
save_fig(f8, "f8_mincer_earnings_en", width = 9, height = 5.4)
message("Stage 4 complete -> output/figures/f8_mincer_earnings_en.png")
