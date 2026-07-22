# variable_mapping.R — SUSENAS Kor variable codes (single source of truth)
# ------------------------------------------------------------------------
# SUSENAS variable numbering DRIFTS between rounds. Two documented code sets
# are kept below; set CODESET to match the file you actually load, then
# verify against that file's codebook before trusting any number.
#   "kor2020" : layout in the CRAN `SUSENAS::SUSENAS2020` template (Jambi).
#   "kor2016" : layout in the IHSN 2016 dictionary (KOR16IND / KOR16RT).
# For the East Java 2025 purchase, copy one of these, rename to "kor2025",
# and reconcile each code against the VSEN25 codebook.
# ------------------------------------------------------------------------

CODESET <- getOption("susenas.codeset", "kor2020")

KOR_SETS <- list(

  kor2020 = list(
    prov="R101", kabkota="R102", urban="R105",      # urban: 1=urban, 2=rural
    sex="R405", age="R407", marital="R404",          # sex: 1=male, 2=female
    age_first_marriage="R409",                       # 0 = never married
    attend="R612", cert="R615",                      # attend: 2 = currently attending
    worked="R702_A", job_temp_away="R704",           # worked: "A"; job_away: 1=yes
    main_activity="R703",
    internet="R808", mobile="R802", net_learn="R811_B",
    disab=c("R1002","R1003","R1004","R1005","R1006","R1007","R1008","R1009"),
    kip="R616", pip="R617",                          # individual social assistance
    hhid="RENUM",
    psu="PSU", ssu="SSU", strata="STRATA", weight="FWT",
    # household-file (KOR..RT) fields — used only in "file" mode merge
    exp_cap="EXP_CAP", bsm="R1606", kps_kks="R1609"
  ),

  kor2016 = list(
    prov="R101", kabkota="R102", urban="R105",
    sex="R405", age="R407", marital="R404",
    age_first_marriage="R409",
    attend="R507", cert="R510",                      # 2016: education block V
    worked="R1101A", job_temp_away="R1102",          # 2016: activity block XI
    main_activity=NA,
    internet="R1006", mobile="R1004A", net_learn="R1009B",
    disab=NA,                                        # WG block NOT in 2016 Kor
    kip=NA, pip=NA,
    hhid="URUT",
    psu=NA, ssu=NA, strata=NA, weight="FWT",         # confirm design vars for 2016
    exp_cap="EXP_CAP", bsm="R1606", kps_kks="R1609"
  )
)

KOR <- KOR_SETS[[CODESET]]
stopifnot(!is.null(KOR))

# --- value constants -----------------------------------------------------
SEX_FEMALE  <- 2L
URBAN_CODE  <- 1L
ATTEND_NOW  <- 2L          # currently attending school
WORKED_MARK <- "A"         # multi-mark "worked last week"
JOBAWAY_YES <- 1L

# --- geography & cohort --------------------------------------------------
PROV_EASTJAVA <- 35L       # Jawa Timur
PROV_JAMBI    <- 15L       # template province (do NOT report as East Java)
AGE_MIN <- 15L             # adolescent / youth girls cohort (NEET convention)
AGE_MAX <- 24L
SS_AGE  <- c(16L, 18L)     # senior-secondary age band (transition focus)

check_mapping <- function() {
  message("CODESET = ", CODESET,
          " | attend=", KOR$attend, " worked=", KOR$worked,
          " internet=", KOR$internet, " hhid=", KOR$hhid)
  invisible(TRUE)
}
