## =====================================================================
## 00_config.R  —  single source of truth for paths, constants, benchmarks
## East Java adolescent girls skills-development investment case
## =====================================================================

## ---- packages -------------------------------------------------------
pkgs <- c("haven", "dplyr", "tidyr", "srvyr", "survey", "fixest",
          "modelsummary", "kableExtra", "ggplot2", "scales")
invisible(lapply(pkgs, function(p)
  if (!requireNamespace(p, quietly = TRUE))
    message("Missing package (install before running): ", p)))
suppressPackageStartupMessages(lapply(pkgs, require, character.only = TRUE))

## ---- data source ----------------------------------------------------
## SWAP POINT. Everything downstream is data-source agnostic.
##   "template"   : CRAN SUSENAS::SUSENAS2020  (Jambi 2020, prov 15) — TESTING ONLY
##   "eastjava24" : BPS Susenas Maret 2024 KOR, East Java extract (prov 35) — PRODUCTION
DATA_SOURCE <- "template"

PATHS <- list(
  eastjava_kor_2024    = "data/susenas2024_maret_kor_ind_jatim.dta",  # individual Kor file (KOR..IND)
  eastjava_kor_rt_2024 = "data/susenas2024_maret_kor_rt_jatim.dta",   # HOUSEHOLD Kor file (KOR..RT) — has EXP_CAP
  eastjava_kp_2024     = "data/susenas2024_maret_kp_jatim.dta",       # KP module — GRANULAR spending only (+~USD10k)
  sakernas_2024        = "data/sakernas2024_agustus_jatim.dta",       # wages -> Mincer
  out                  = "outputs/"
)

## ---- geography & cohort --------------------------------------------
PROV_EASTJAVA <- 35L          # BPS province code for Jawa Timur
PROV_TEMPLATE <- 15L          # Jambi — the template's only province
AGE_MIN <- 15L
AGE_MAX <- 24L                # adolescent/youth girls cohort (ToR)
SEC_AGE <- c(16L, 18L)        # senior-secondary age band (transition focus, cmt 29)

## ---- BPS Kor variable codes (March questionnaire) -------------------
## Verified against the bundled Kor microdata structure.
KOR <- list(
  prov="R101", kabkota="R102", urban="R105",        # 1=urban 2=rural
  hh_rel="R403", marital="R404", sex="R405",         # sex 1=male 2=female
  age="R407", age_first_marriage="R409",             # 0 = never married
  attend="R612",                                     # 2 = currently attending
  level_attended="R613", grade="R614", cert="R615",  # highest level / certificate
  kip="R616", pip="R617",                            # social-assistance flags
  worked="R702_A",        # "A" = worked in last week
  in_school_wk="R702_B", housekeep="R702_C",
  main_activity="R703",   # 1 work 2 school 3 housekeeping 4 other
  job_temp_away="R704",   # 1 = has job, temporarily not working
  ind_main="R705", emp_status="R706", hours_main="R707",
  mobile_own="R802",      # 1 = owns/controls mobile
  internet_3mo="R808",    # 1 = used internet in last 3 months
  net_for_learning="R811_B",                          # internet used for learning
  ## disability (Washington-Group): present in 2020 Kor; NOT in 2016 block list.
  ## CONFIRM these exist and their numbering in the 2025 file before using (cmt 41).
  disab=c("R1002","R1003","R1004","R1005","R1006","R1007","R1008","R1009"),
  ## household social assistance (KOR..RT household file)
  bsm="R1606", kps_kks="R1609", blt="R1601",
  ## household per-capita expenditure aggregates — SHIP IN THE KOR HOUSEHOLD FILE
  ## (verified on IHSN 2016 KOR16RT). Quintiles need only base Kor, not the KP module.
  food="FOOD", nfood="NFOOD", expend="EXPEND", exp_cap="EXP_CAP",
  ## survey design
  psu="PSU", ssu="SSU", strata="STRATA", weight="FWT",
  ## household merge key — YEAR-SPECIFIC: URUT (2016) vs RENUM (2020). Verify for 2025.
  hhid="URUT"
)
YES_MARK <- "A"   # multi-mark "worked" coding in Kor
ATTEND_NOW <- 2L  # R612 == currently attending

## ---- EU report benchmark parameters (May 2026 V.6) ------------------
## Kept verbatim for comparability; we will stress-test, not adopt, the high BCRs.
EU <- list(
  discount_rate = 0.03, inflation = 0.025, gdp_growth = 0.05,
  productive_years = 40L, wage_growth = 0.02,
  female_wage_idr = 32.59e6,   # BPS 2025, annual
  female_lfpr = 0.526,         # BPS 2024
  ## headline BCRs (Table 17) — vocational training = 22.4 is the one we interrogate
  bcr = c(PKH=10.1, PIP=3.1, Vocational=22.4, IFA=2.3, ILP_PKPR=4.3, MentalHealth=11.2),
  ## EU effect sizes (Table 4) we treat as borrowed national elasticities
  voc_lfpr_pp_female = 0.034, voc_employment_pp_female = 0.042, voc_income_pct_female = 0.056,
  mincer_return_female = c(0.049, 0.053)  # EU's 4.9–5.3%/yr for females
)

## ---- our anchors (girl-specific; from the Bolivia machinery) --------
## Evans & Yuan (2022) girl-specific access-outcome distribution, for transportability.
ANCHOR <- list(
  evans_yuan_sd = c(median = 0.07, p25 = 0.02, p75 = 0.14)  # SD on access outcomes, 73 RCTs
)

dir.create(PATHS$out, showWarnings = FALSE, recursive = TRUE)
message("Config loaded. DATA_SOURCE = ", DATA_SOURCE)
