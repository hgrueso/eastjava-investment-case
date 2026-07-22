# 01_clean_data.R — Stage 1: load SUSENAS Kor, construct adolescent sample
# ------------------------------------------------------------------------
# Inputs  (DATA_SOURCE):
#   "template" : CRAN SUSENAS::SUSENAS2020  (Jambi 2020, prov 15) — TESTING ONLY
#   "file"     : data/<kor_ind>.dta  (+ optional data/<kor_rt>.dta for EXP_CAP)
# Outputs : output/analysis_ready.rds       (adolescents 15–24)
#           output/analysis_ready_full.rds  (all persons)
#           output/tables/analysis_dictionary.csv
# ------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(haven); library(dplyr); library(tidyr); library(purrr)
  library(stringr); library(tibble); library(here); library(glue)
})

source(here::here("R", "utils.R"))
source(here::here("R", "variable_mapping.R"))
ensure_dirs(); check_mapping()

# ---- choose data source -------------------------------------------------
DATA_SOURCE <- getOption("susenas.source", "template")
KOR_IND <- here::here("data", "susenas2025_kor_ind_jatim.dta")  # individual file
KOR_RT  <- here::here("data", "susenas2025_kor_rt_jatim.dta")   # household file (EXP_CAP)

# 1. Load -----------------------------------------------------------------
if (DATA_SOURCE == "template") {
  if (!requireNamespace("SUSENAS", quietly = TRUE))
    stop("install.packages('SUSENAS') to use the template")
  data("SUSENAS2020", package = "SUSENAS")
  raw <- SUSENAS2020 |> mutate(across(where(\(x) inherits(x,"haven_labelled")), to_plain))
  rt  <- NULL
  VINTAGE <- "Jambi 2020 (TEMPLATE — not East Java)"; IS_REAL <- FALSE
} else {
  raw <- read_dta(KOR_IND) |> mutate(across(where(\(x) inherits(x,"haven_labelled")), to_plain))
  if (KOR$prov %in% names(raw)) raw <- raw |> filter(.data[[KOR$prov]] == PROV_EASTJAVA)
  rt  <- if (file.exists(KOR_RT)) read_dta(KOR_RT) |>
            mutate(across(where(\(x) inherits(x,"haven_labelled")), to_plain)) else NULL
  VINTAGE <- "East Java (prov 35), SUSENAS Kor"; IS_REAL <- TRUE
}
message(glue("Loaded {VINTAGE}: {nrow(raw)} rows x {ncol(raw)} cols"))

g <- function(code) if (!is.na(code) && code %in% names(raw)) raw[[code]] else NA

# 2. Construct individual variables ---------------------------------------
HAS_DISAB <- !any(is.na(KOR$disab)) && all(KOR$disab %in% names(raw))

dat <- tibble(
  prov    = g(KOR$prov),
  kabkota = g(KOR$kabkota),
  urban   = g(KOR$urban) == URBAN_CODE,                 # TRUE=urban
  rural   = g(KOR$urban) != URBAN_CODE,
  female  = g(KOR$sex) == SEX_FEMALE,
  age     = as.numeric(g(KOR$age)),

  # --- education ---
  in_school = g(KOR$attend) == ATTEND_NOW,
  cert      = g(KOR$cert),

  # --- employment ---
  worked   = as.character(g(KOR$worked)) == WORKED_MARK,
  job_away = g(KOR$job_temp_away) == JOBAWAY_YES,

  # --- digital ---
  internet  = g(KOR$internet) == 1L,
  mobile    = g(KOR$mobile)   == 1L,
  net_learn = g(KOR$net_learn) == 1L,

  # --- protection ---
  marital     = g(KOR$marital),
  married_u18 = { a <- as.numeric(g(KOR$age_first_marriage)); !is.na(a) & a > 0 & a < 18 },

  # --- design ---
  w      = as.numeric(g(KOR$weight)),
  psu    = g(KOR$psu),
  strata = g(KOR$strata),
  hhid   = g(KOR$hhid)
) |>
  mutate(
    employed = worked | job_away,
    # NEET (15–24): not in education AND not employed.
    # Kor has no clean "training" item — documented caveat; reconcile w/ BPS.
    neet = case_when(is.na(in_school) | is.na(employed) ~ NA,
                     !in_school & !employed ~ TRUE, TRUE ~ FALSE),
    sex  = factor(if_else(female, "Girls", "Boys"), c("Girls","Boys"))
  )

# disability (only if WG block present in this round)
if (HAS_DISAB) {
  dmat <- sapply(KOR$disab, \(v) raw[[v]] >= 3L)
  dat$disab_any <- apply(dmat, 1, \(r) any(r, na.rm = TRUE))
} else {
  dat$disab_any <- NA
  message("note: disability (WG) block not in this round — disab_any = NA")
}

# 3. Wealth quintile ------------------------------------------------------
# EXP_CAP (per-capita monthly expenditure) ships in the Kor HOUSEHOLD file.
# Quintiles need only the base Kor, not the consumption module.
# ntile() is robust to discrete distributions (the STRATA proxy) where cut()
# on weighted quantile breaks can fail; for EXP_CAP it gives clean quintiles.
qlab <- function(x) factor(dplyr::ntile(x, 5), levels = 1:5, labels = paste0("Q",1:5))
if (!is.null(rt) && KOR$exp_cap %in% names(rt) && KOR$hhid %in% names(rt)) {
  rtx <- rt |> transmute(hhid = .data[[KOR$hhid]], exp_cap = .data[[KOR$exp_cap]])
  dat <- dat |> left_join(rtx, by = "hhid")
  dat$wealth_q <- qlab(dat$exp_cap)
  WEALTH_BASIS <- "per-capita expenditure EXP_CAP (Kor household file)"
} else {
  dat$wealth_q <- qlab(as.numeric(dat$strata))
  WEALTH_BASIS <- "STRATA proxy (template — replace with EXP_CAP)"
}
dat$poor_q1q2 <- dat$wealth_q %in% c("Q1","Q2")
message("wealth_q basis: ", WEALTH_BASIS)

# 4. Cohort + priority-girl flags ----------------------------------------
dat <- dat |>
  mutate(
    adolescent = age >= AGE_MIN & age <= AGE_MAX,
    ss_age     = age >= SS_AGE[1] & age <= SS_AGE[2],     # senior-secondary age
    girl                = as.integer(female & adolescent),
    girl_rural          = as.integer(girl == 1 & rural),
    girl_poor           = as.integer(girl == 1 & poor_q1q2),
    girl_disab          = as.integer(girl == 1 & disab_any %in% TRUE),
    girl_no_internet    = as.integer(girl == 1 & internet %in% FALSE),
    girl_priority       = as.integer(girl == 1 &
                            (rural | poor_q1q2 | disab_any %in% TRUE))
  )

# 5. Save -----------------------------------------------------------------
attr(dat, "vintage") <- VINTAGE; attr(dat, "is_real") <- IS_REAL
saveRDS(dat, here::here("output", "analysis_ready_full.rds"))
ado <- dat |> filter(adolescent)
saveRDS(ado, here::here("output", "analysis_ready.rds"))
message(glue("Adolescents 15–24: {nrow(ado)} (weighted {format(round(sum(ado$w)), big.mark=',')})"))

# 6. QA + dictionary ------------------------------------------------------
qa <- ado |> summarise(
  n = n(), pct_female = mean(female, na.rm=TRUE),
  pct_rural = mean(rural, na.rm=TRUE),
  pct_neet  = mean(neet,  na.rm=TRUE),
  pct_inschool = mean(in_school, na.rm=TRUE),
  pct_internet = mean(internet,  na.rm=TRUE),
  pct_married_u18 = mean(married_u18, na.rm=TRUE)
)
print(qa)
if (!IS_REAL) message(">>> TEMPLATE DATA — numbers are Jambi 2020, NOT East Java. <<<")

dict <- tibble(var = names(ado),
               type = vapply(ado, \(x) class(x)[1], character(1)),
               n_miss = vapply(ado, \(x) sum(is.na(x)), integer(1)))
write_table(dict, "analysis_dictionary")
message("Stage 1 complete → output/analysis_ready.rds")
