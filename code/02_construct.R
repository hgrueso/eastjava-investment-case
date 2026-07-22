## =====================================================================
## 02_construct.R  —  derive analysis variables + survey-design object
## Produces `ind` (tibble) and `svy` (srvyr design).
## =====================================================================
## Run 00_config.R, 01_load.R first.

g <- function(code) raw[[ KOR[[code]] ]]

ind <- tibble::tibble(
  prov   = g("prov"),
  kabkota= g("kabkota"),
  urban  = g("urban") == 1L,                 # TRUE = urban, FALSE = rural
  sex    = factor(ifelse(g("sex") == 2L, "Female", "Male"), c("Female","Male")),
  female = g("sex") == 2L,
  age    = as.numeric(g("age")),
  ## --- education ---
  in_school = g("attend") == ATTEND_NOW,     # currently attending
  cert      = g("cert"),
  ## --- employment ---
  worked    = as.character(g("worked")) == YES_MARK,
  job_away  = g("job_temp_away") == 1L,
  employed  = (as.character(g("worked")) == YES_MARK) | (g("job_temp_away") == 1L),
  ## --- NEET (15-24): not in education AND not employed ---
  ##     Kor has no clean "training" item; documented as a caveat. Cross-checks vs R703.
  ## --- digital ---
  internet  = g("internet_3mo") == 1L,
  mobile    = g("mobile_own")   == 1L,
  net_learn = g("net_for_learning") == 1L,
  ## --- protection ---
  marital   = g("marital"),
  married_u18 = { a <- as.numeric(g("age_first_marriage")); a > 0 & a < 18 },
  ## --- design ---
  w   = as.numeric(g("weight")),
  psu = g("psu"), strata = g("strata")
)

## NEET (depends on in_school & employed above)
ind$neet <- with(ind, !in_school & !employed)

## Disability: any functional difficulty "a lot / cannot at all"
## Washington-Group style. BPS codes typically 1=no,2=some,3=a lot,4=cannot.
disab_mat <- sapply(KOR$disab, function(v) if (v %in% names(raw)) raw[[v]] >= 3L else NA)
ind$disability <- if (is.matrix(disab_mat)) apply(disab_mat, 1, function(r) any(r, na.rm=TRUE)) else NA

## Wealth quantile — per-capita monthly expenditure (EXP_CAP).
##  KEY FINDING (IHSN 2016 KOR16RT): EXP_CAP / FOOD / NFOOD / EXPEND ship in the Kor
##  HOUSEHOLD file. So expenditure quintiles need only the base Kor household file merged
##  onto individuals by the household key — NOT the +~USD10k KP module. The KP module is
##  required ONLY for granular item-level spending (e.g. education outlays, Ali's cmt 8).
##  CONFIRM with BPS that the priced 2025 Kor delivery includes EXP_CAP.
if (DATA_SOURCE == "eastjava24" && file.exists(PATHS$eastjava_kor_rt_2024)) {
  rt <- haven::read_dta(PATHS$eastjava_kor_rt_2024)
  rt <- dplyr::transmute(rt, hhid = .data[[KOR$hhid]],
                         exp_cap = .data[[KOR$exp_cap]])
  ind$hhid <- g("hhid")
  ind <- dplyr::left_join(ind, rt, by = "hhid")
  ind$wealth_q <- dplyr::ntile(ind$exp_cap, 5)
  attr(ind$wealth_q, "basis") <- "per-capita monthly expenditure EXP_CAP (Kor household file)"
} else {
  ## template fallback: STRATA wealth-index proxy (no expenditure in the CRAN individual file)
  ind$wealth_q <- dplyr::ntile(as.numeric(raw[[KOR$strata]]), 5)
  attr(ind$wealth_q, "basis") <- "STRATA/wealth-index PROXY (template — replace with EXP_CAP)"
}
ind$poor_q1_q2 <- ind$wealth_q %in% c(1L, 2L)   # bottom-40 flag (cmt 43)

## --- analytic cohort: adolescent girls 15-24 ---
ind$cohort_ag <- with(ind, female & age >= AGE_MIN & age <= AGE_MAX)
ind$cohort_ab <- with(ind, !female & age >= AGE_MIN & age <= AGE_MAX)  # boys comparator (cmt 19/21)

## --- survey-design object (weighted, PSU-clustered) ---
svy <- ind |>
  srvyr::as_survey_design(ids = psu, strata = strata, weights = w, nest = TRUE)

message("Constructed ", nrow(ind), " individuals; wealth_q basis: ",
        attr(ind$wealth_q, "basis"))
