# 04b_mincer_ifls_controls.R: wage returns to schooling (IFLS5) with controls.
# Builds on 04_mincer_ifls.R (same schooling, wage and weight construction) and
# estimates, separately for women and men, four nested specifications:
#   (1) Baseline:       ln(wage) ~ S + Exp + Exp^2
#   (2) + Region:       + urban + province fixed effects
#   (3) + Background:   + father's and mother's education (fixed effects)
#   (4) + Selection:    Heckman two-step, using children under 5 in the
#                       household as the variable that affects working but
#                       not the wage itself
# Region, background and household variables are detected automatically and
# printed, so the console shows exactly which IFLS5 variables were used.
# Output: output/models/mincer_ifls_controls.csv
suppressPackageStartupMessages({
  library(haven); library(dplyr); library(fixest); library(readr); library(here)
})
source(here::here("R","utils.R")); ensure_dirs()
IFLS <- here::here("data","IFLS5")
rd <- function(b, f) read_dta(file.path(IFLS, b, f)) |>
  mutate(across(where(\(x) inherits(x, "haven_labelled")), as.numeric))
lab <- function(x) { l <- attr(x, "label"); if (is.null(l)) "" else l }

WAGE_VAR <- "tk25a1"; WAGE_MAX <- 9e8; AGE_LO <- 20; AGE_HI <- 60
PREV_YEARS <- c("2"=0,"72"=0,"3"=6,"4"=6,"73"=6,"5"=9,"6"=9,"74"=9,"60"=12,"61"=12,"62"=16,"63"=18,"15"=0,"17"=0,"95"=0)
GRADE_IF_DONE <- c("2"=6,"72"=6,"3"=3,"4"=3,"73"=3,"5"=3,"6"=3,"74"=3,"60"=3,"61"=4,"62"=2,"63"=3,"15"=0,"17"=0,"95"=0)

# ---- core sample (same construction as 04) --------------------------------
cov <- rd("hh14_b3a_dta","b3a_cov.dta") |> transmute(pidlink, hhid14, age = as.numeric(age), female = sex == 3)
dl  <- rd("hh14_b3a_dta","b3a_dl1.dta")
tk  <- rd("hh14_b3a_dta","b3a_tk2.dta")
edu <- tibble(pidlink = dl$pidlink, lvl = as.character(dl$dl06), g = suppressWarnings(as.numeric(dl$dl07))) |>
  mutate(grade = case_when(g == 7 ~ GRADE_IF_DONE[lvl], g %in% 0:6 ~ g, TRUE ~ NA_real_),
         years_school = PREV_YEARS[lvl] + ifelse(is.na(grade), 0, grade)) |>
  select(pidlink, years_school)
wag <- tibble(pidlink = tk$pidlink, wage = suppressWarnings(as.numeric(tk[[WAGE_VAR]]))) |>
  mutate(wage = ifelse(wage > 0 & wage < WAGE_MAX, wage, NA)) |>
  group_by(pidlink) |> summarise(wage = suppressWarnings(max(wage, na.rm = TRUE)), .groups = "drop") |>
  mutate(wage = ifelse(is.finite(wage), wage, NA))
ptr  <- rd("hh14_trk_dta","ptrack.dta")
wcol <- grep("^pwt?14", names(ptr), value = TRUE); wcol <- c(wcol[grepl("x", wcol)], wcol)[1]
wt   <- tibble(pidlink = ptr$pidlink, w = if (!is.na(wcol)) as.numeric(ptr[[wcol]]) else 1) |> distinct(pidlink, .keep_all = TRUE)

# ---- region: province and urban/rural from Book K (bk_sc) -----------------
sc_file <- list.files(file.path(IFLS, "hh14_bk_dta"), pattern = "^bk_sc.*\\.dta$")[1]
cat("Region file:", sc_file, "\n")
sc <- rd("hh14_bk_dta", sc_file)
pv <- grep("^sc01", names(sc), value = TRUE)[1]; ur <- grep("^sc05", names(sc), value = TRUE)[1]
cat("Region variables: province =", pv, "(", lab(sc[[pv]]), ") | urban =", ur, "(", lab(sc[[ur]]), ")\n")
reg <- tibble(hhid14 = sc$hhid14, prov = sc[[pv]], urban = as.numeric(sc[[ur]] == 1)) |> distinct(hhid14, .keep_all = TRUE)

# ---- children under 5 in the household (exclusion variable) ---------------
ar_file <- list.files(file.path(IFLS, "hh14_bk_dta"), pattern = "^bk_ar1.*\\.dta$")[1]
ar <- rd("hh14_bk_dta", ar_file)
cat("Household roster age variable: ar09 (", lab(ar$ar09), ")\n")
kids <- ar |> group_by(hhid14) |> summarise(kids_u5 = sum(ar09 < 5, na.rm = TRUE), .groups = "drop")

# ---- family background: parents' education (Book 3B, section BA) ---------
# b3b_ba0 has one row per parent per respondent: ba04x = parent type,
# ba07a = parent ever attended school, ba08 = parent's highest level.
ba <- read_dta(file.path(IFLS, "hh14_b3b_dta", "b3b_ba0.dta"))
ptype  <- tolower(as.character(haven::as_factor(ba$ba04x)))
school <- tolower(as.character(haven::as_factor(ba$ba07a)))
cat("Parent types in b3b_ba0 (ba04x):\n"); print(table(ptype, useNA = "ifany"))
pe <- tibble(pidlink = ba$pidlink, ptype,
             edu = case_when(grepl("^no|^3|tidak", school) ~ 0,            # never attended school
                             !is.na(as.numeric(ba$ba08)) ~ as.numeric(ba$ba08),
                             TRUE ~ NA_real_))
pick <- function(pat) pe |> filter(grepl(pat, ptype), !grepl("step|tiri|in-law|mertua|adopt|angkat", ptype)) |>
  distinct(pidlink, .keep_all = TRUE)
bg <- full_join(pick("father|bapak|ayah") |> transmute(pidlink, fedu = edu),
                pick("mother|ibu")        |> transmute(pidlink, medu = edu), by = "pidlink")
cat(sprintf("Parents' education matched for %s respondents (father known: %.0f%%, mother known: %.0f%%)\n",
    format(nrow(bg), big.mark = ","), 100 * mean(!is.na(bg$fedu)), 100 * mean(!is.na(bg$medu))))
if (nrow(bg) == 0) cat("WARNING: no father/mother rows matched; check the parent-type table above.\n")

# ---- assemble: all adults 20-60 with schooling (workers and non-workers) ---
a <- cov |> left_join(edu, "pidlink") |> left_join(wag, "pidlink") |> left_join(wt, "pidlink") |>
  left_join(reg, "hhid14") |> left_join(kids, "hhid14") |>
  (\(z) if (nrow(bg)) left_join(z, bg, "pidlink") else mutate(z, fedu = NA_real_, medu = NA_real_))() |>
  filter(age >= AGE_LO, age <= AGE_HI, !is.na(years_school)) |>
  mutate(exp = pmax(age - years_school - 6, 0), exp2 = exp^2, works = !is.na(wage),
         lw = log(wage), kids_u5 = coalesce(kids_u5, 0L),
         fedu = ifelse(is.na(fedu), -1, fedu), medu = ifelse(is.na(medu), -1, medu),
         w = coalesce(w, 1))

est <- function(dd, sex) {
  wk <- dd |> filter(works)
  m1 <- feols(lw ~ years_school + exp + exp2, wk, weights = ~w, vcov = "hetero")
  m2 <- feols(lw ~ years_school + exp + exp2 + urban | prov, wk, weights = ~w, cluster = ~prov)
  m3 <- if (nrow(bg)) feols(lw ~ years_school + exp + exp2 + urban | prov + fedu + medu, wk, weights = ~w, cluster = ~prov) else NULL
  # Heckman two-step: probit for working, then add the inverse Mills ratio
  pr <- glm(works ~ years_school + exp + exp2 + urban + kids_u5 + factor(prov), dd, family = binomial(link = "probit"))
  xb <- predict(pr, newdata = dd, type = "link")
  dd$imr <- dnorm(xb) / pnorm(xb)
  wk <- dd |> filter(works)
  f4 <- if (nrow(bg)) lw ~ years_school + exp + exp2 + urban + imr | prov + fedu + medu else lw ~ years_school + exp + exp2 + urban + imr | prov
  m4 <- feols(f4, wk, weights = ~w, cluster = ~prov)
  kz <- summary(pr)$coefficients["kids_u5", ]
  cat(sprintf("\n[%s] working share %.0f%% | children under 5 in selection equation: %.3f (p=%.3f)\n",
              sex, 100 * mean(dd$works), kz[1], kz[4]))
  ms <- list("(1) Baseline" = m1, "(2) + Region" = m2, "(3) + Family background" = m3, "(4) + Selection (Heckman)" = m4)
  bind_rows(lapply(names(ms), function(n) { m <- ms[[n]]; if (is.null(m)) return(NULL)
    ct <- coeftable(m)["years_school", ]
    tibble(Sex = sex, Model = n, `Return per year` = round(ct[1], 4), SE = round(ct[2], 4), p = round(ct[4], 4), N = nobs(m)) }))
}
res <- bind_rows(est(a |> filter(female), "Women"), est(a |> filter(!female), "Men"))
write_csv(res, here::here("output","models","mincer_ifls_controls.csv"))
cat("\n================ RETURN TO A YEAR OF SCHOOLING (IFLS5 wages) ================\n")
print(as.data.frame(res |> mutate(`Return per year` = sprintf("%.1f%%", 100 * `Return per year`))))
message("04b done -> output/models/mincer_ifls_controls.csv")
