# 01b_clean_ifls.R — Stage 1 (IFLS5 path): build adolescent sample 15–24
# ------------------------------------------------------------------------
# Reads IFLS5 Stata section files and produces the SAME schema as the
# SUSENAS path (output/analysis_ready.rds), so 02 / 03b / 06 run unchanged.
#
# What IFLS5 gives well : age, sex, education/attendance, employment → NEET,
#                         and (later) a female Mincer from the TK wage module.
# What IFLS5 LACKS       : internet/mobile use, disability (WG), and a ready
#                         per-capita expenditure (no PCE file for IFLS5).
#                         Those columns are set NA here and flagged.
# ------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(haven); library(dplyr); library(tidyr); library(stringr)
  library(here); library(glue)
})
source(here::here("R", "utils.R")); ensure_dirs()

DATA <- here::here("data")
dpath <- function(book, file) file.path(DATA, book, file)

# ---- CONFIG: variable names I am INFERRING from the IFLS5 codebook -------
# If any pick() below errors, it prints the module's columns — set the right
# name here (User's Guide Vol 2 / hh codebooks) and re-run. 1-minute fix.
CFG <- list(
  attend_var  = "dl07",   # "still attending school?"  (1 = yes, 3 = no)
  edu_level   = "dl04",   # highest level ever attended
  worked_var  = "tk01",   # worked ≥1h last week       (1 = yes, 3 = no)
  jobaway_var = "tk02",   # had a job but absent        (1 = yes)
  area_var    = "sc05",   # household urban/rural        (verify coding!)
  prov_var    = "sc01_14_14",
  kab_var     = "sc02_14_14"
)
PROV_EASTJAVA <- 35L      # set to NA to keep all 13 IFLS provinces
AGE_MIN <- 15L; AGE_MAX <- 24L; SS_AGE <- c(16L, 18L)

pick <- function(df, name, where) {
  if (name %in% names(df)) return(df[[name]])
  stop(glue("'{name}' not in {where}. Available:\n  ",
            paste(names(df), collapse = ", ")), call. = FALSE)
}
rd <- function(book, file) read_dta(dpath(book, file)) |>
  mutate(across(where(\(x) inherits(x, "haven_labelled")), \(x) as.numeric(x)))

# ---- 1. demographics (Book 3A cov: one row per person 15+) --------------
cov <- rd("hh14_b3a_dta", "b3a_cov.dta")
demo <- tibble(
  pidlink = cov$pidlink, hhid14 = cov$hhid14,
  age     = as.numeric(cov$age),
  female  = cov$sex == 3,                         # IFLS: 1=male, 3=female
  marstat = cov$marstat
)

# ---- 2. education (DL) & employment (TK), one row per person ------------
dl <- rd("hh14_b3a_dta", "b3a_dl1.dta")
tk <- rd("hh14_b3a_dta", "b3a_tk1.dta")
edu <- tibble(pidlink = dl$pidlink,
              in_school = pick(dl, CFG$attend_var, "b3a_dl1") == 1,
              edu_level = pick(dl, CFG$edu_level, "b3a_dl1"))
emp <- tibble(pidlink = tk$pidlink,
              worked   = pick(tk, CFG$worked_var,  "b3a_tk1") == 1,
              job_away = pick(tk, CFG$jobaway_var, "b3a_tk1") == 1)

# ---- 3. household geography (Book K, sc module) -------------------------
sc <- rd("hh14_bk_dta", "bk_sc1.dta")
geo <- tibble(hhid14 = sc$hhid14,
              prov    = pick(sc, CFG$prov_var, "bk_sc1"),
              kabkota = pick(sc, CFG$kab_var,  "bk_sc1"),
              area    = if (CFG$area_var %in% names(sc)) sc[[CFG$area_var]] else NA)

# ---- 4. cross-sectional individual weight (ptrack, auto-detect) ---------
ptr <- rd("hh14_trk_dta", "ptrack.dta")
wcol <- grep("^pwt?14", names(ptr), value = TRUE)          # e.g. pwt14xa
wcol <- c(wcol[grepl("x", wcol)], wcol)[1]                 # prefer cross-section
wt <- if (!is.na(wcol) && length(wcol)) {
  message("using IFLS5 weight: ", wcol)
  tibble(pidlink = ptr$pidlink, w = as.numeric(ptr[[wcol]]))
} else { message("⚠ no pwt14* weight found — UNWEIGHTED (set manually)"); NULL }

# ---- 5. assemble --------------------------------------------------------
dat <- demo |>
  left_join(edu, by = "pidlink") |>
  left_join(emp, by = "pidlink") |>
  left_join(geo, by = "hhid14") |>
  { \(d) if (!is.null(wt)) left_join(d, wt, by = "pidlink") else mutate(d, w = 1) }() |>
  mutate(
    employed = coalesce(worked, FALSE) | coalesce(job_away, FALSE),
    neet = case_when(is.na(in_school) | age < AGE_MIN ~ NA,
                     !in_school & !employed ~ TRUE, TRUE ~ FALSE),
    rural = case_when(area == 2 ~ TRUE, area == 1 ~ FALSE, TRUE ~ NA),  # verify!
    sex   = factor(if_else(female, "Girls", "Boys"), c("Girls","Boys")),
    # --- columns IFLS5 cannot fill yet (kept for schema compatibility) ---
    internet = NA, mobile = NA, net_learn = NA, disab_any = NA,
    married_u18 = NA,                          # add later from KW marriage history
    wealth_q = factor(NA, levels = paste0("Q",1:5)),  # no PCE in IFLS5
    poor_q1q2 = NA,
    psu = NA, strata = NA,                     # design vars in restricted data
    adolescent = age >= AGE_MIN & age <= AGE_MAX,
    ss_age     = age >= SS_AGE[1] & age <= SS_AGE[2]
  )

if (!is.na(PROV_EASTJAVA) && !all(is.na(dat$prov)))
  dat <- dat |> filter(prov == PROV_EASTJAVA)

# priority-girl flags (same names as SUSENAS path; NA-tolerant)
dat <- dat |> mutate(
  girl             = as.integer(female & adolescent),
  girl_rural       = as.integer(girl == 1 & rural %in% TRUE),
  girl_no_internet = NA_integer_,
  girl_priority    = as.integer(girl == 1 & rural %in% TRUE)
)

attr(dat, "vintage") <- glue("IFLS5 2014/15{ifelse(!is.na(PROV_EASTJAVA),' — East Java (prov 35)','')}")
attr(dat, "is_real") <- TRUE
saveRDS(dat, here::here("output", "analysis_ready_full.rds"))
ado <- dat |> filter(adolescent)
saveRDS(ado, here::here("output", "analysis_ready.rds"))

message(glue("IFLS5 adolescents 15–24: {nrow(ado)} rows"))
print(ado |> summarise(n = n(), pct_female = mean(female, na.rm=TRUE),
                       pct_neet = mean(neet, na.rm=TRUE),
                       pct_inschool = mean(in_school, na.rm=TRUE),
                       pct_rural = mean(rural, na.rm=TRUE)))
message("Stage 1 (IFLS5) complete → output/analysis_ready.rds")
message("NOTE: internet/mobile/disability/wealth/marriage are NA on the IFLS path.")
