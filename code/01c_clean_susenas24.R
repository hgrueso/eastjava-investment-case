# 01c_clean_susenas24.R — Stage 1 (SUSENAS Maret 2024): East Java sample
# ------------------------------------------------------------------------
# Reads the 2024 KOR individual file + per-capita expenditure (KAPITA),
# filters East Java (prov 35), and writes output/analysis_ready.rds in the
# SAME schema as the IFLS/template paths, so 02 / 03b / 06 run unchanged.
#
# Codes verified from value distributions (labels are stripped by BPS):
#   R405 sex 1=M,2=F · R407 age · R409 age@first marriage · R105 urban(1)/rural(2)
#   R610 school participation: 2 = CURRENTLY ATTENDING
#   R704 main activity: 1=working, 2=school, 3=housekeeping, 4=other (RELIABLE)
#   (R701/R702 invert the gender pattern — NOT employment; excluded)
#   KAPITA = per-capita monthly expenditure (KP block 43), merged by URUT
# Verified from VSEN24.K: internet R805, mobile R802, disability R1002-R1009 (WG),
#   education attainment R614 -> years_school. Earnings NOT collected by SUSENAS.
# ------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(haven); library(dplyr); library(here); library(glue)
})
source(here::here("R","utils.R")); ensure_dirs()

IND <- here::here("data","Susenas 2024","ssn202403_kor_ind1.dta")
KP  <- here::here("data","Susenas 2024","ssn24_mar_kp_blok43.dta")
PROV_EASTJAVA <- 35L
AGE_MIN <- 15L; AGE_MAX <- 24L; SS_AGE <- c(16L,18L)
g <- function(d,v) if (v %in% names(d)) as.numeric(d[[v]]) else NA_real_

raw <- read_dta(IND) |> mutate(across(where(\(x) inherits(x,"haven_labelled")), as.numeric))
if ("R101" %in% names(raw)) raw <- raw |> filter(R101 == PROV_EASTJAVA)
message(glue("East Java rows: {nrow(raw)}"))

# weighted quintile helper (falls back to ntile if breaks collapse)
wq5 <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  br <- tryCatch(quantile(rep(x[ok], times = pmax(round(w[ok]/min(w[ok],na.rm=TRUE)),1)),
                          probs = seq(0,1,.2), na.rm = TRUE), error = function(e) NULL)
  if (is.null(br) || length(unique(br)) < 6)
    return(factor(dplyr::ntile(x,5), 1:5, paste0("Q",1:5)))
  cut(x, unique(br), include.lowest = TRUE, labels = paste0("Q",1:5))
}

dat <- tibble(
  prov    = g(raw,"R101"), kabkota = g(raw,"R102"),
  urban   = g(raw,"R105") == 1, rural = g(raw,"R105") == 2,
  female  = g(raw,"R405") == 2, age = g(raw,"R407"),
  marital = g(raw,"R404"),
  # NEET from R704 main activity (1=working,2=school,3=housekeeping,4=other):
  # R701/R702 invert the gender pattern and are NOT employment — do not use.
  main_act  = g(raw,"R704"),
  in_school = (g(raw,"R610") == 2) | (g(raw,"R704") == 2),   # attending OR main activity = school
  employed  = g(raw,"R704") == 1,                            # main activity = working
  married_u18 = { a <- g(raw,"R409"); !is.na(a) & a > 0 & a < 18 },
  w = g(raw,"FWT"), psu = g(raw,"PSU"), strata = g(raw,"STRATA"),
  hhid = if ("URUT" %in% names(raw)) raw$URUT else NA,
  pid  = if ("R401" %in% names(raw)) as.numeric(raw$R401) else NA  # person no. (roster), for Blok XV merge
) |>
  mutate(
    neet = case_when(is.na(in_school) | is.na(employed) ~ NA,
                     !in_school & !employed ~ TRUE, TRUE ~ FALSE),
    sex  = factor(if_else(female, "Girls", "Boys"), c("Girls","Boys")),
    # --- Digital (Blok VIII): R805 used internet 3mo; R802 owns phone ---
    internet = g(raw,"R805") == 1,
    mobile   = g(raw,"R802") == 1,
    net_learn = NA,   # R808 purpose-H (online learning) is multi-mark; wire if split cols exist
    # --- Disability (Blok X, Washington Group): severe = "a lot" or "cannot" ---
    disab_any = (g(raw,"R1002") %in% c(1,2)) | (g(raw,"R1003") %in% c(5,6)) |
                (g(raw,"R1004") %in% c(1,2)) | (g(raw,"R1005") %in% c(5,6)) |
                (g(raw,"R1006") %in% c(1,2)) | (g(raw,"R1007") %in% c(5,6)) |
                (g(raw,"R1008") %in% c(1,2)) | (g(raw,"R1009") %in% c(5,6)),
    # --- Education attainment (Blok VI): R614 highest certificate -> years ---
    edu_cert = g(raw,"R614"),
    years_school = dplyr::case_when(
      edu_cert %in% c(25) ~ 0,
      edu_cert %in% c(1,2,3,4,5)          ~ 6,   # primary
      edu_cert %in% c(6,7,8,9,10)         ~ 9,   # lower secondary
      edu_cert %in% c(11,12,13,14,15,16,17) ~ 12, # upper secondary
      edu_cert %in% c(18)                 ~ 14,  # D1/D2
      edu_cert %in% c(19)                 ~ 15,  # D3
      edu_cert %in% c(20,21)              ~ 16,  # D4/S1
      edu_cert %in% c(22)                 ~ 17,  # Profesi
      edu_cert %in% c(23)                 ~ 18,  # S2
      edu_cert %in% c(24)                 ~ 21,  # S3
      TRUE ~ NA_real_),
    completed_lsec = edu_cert %in% c(6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
  )
# guard: if disability block absent, set NA (avoid all-FALSE artefact)
if (!("R1002" %in% names(raw))) dat$disab_any <- NA
if (!("R805"  %in% names(raw))) dat$internet  <- NA
if (!("R802"  %in% names(raw))) dat$mobile    <- NA

# wealth quintile from KAPITA (per-capita expenditure), merged by household URUT
if (file.exists(KP) && "URUT" %in% names(raw)) {
  kp <- read_dta(KP) |> mutate(across(where(\(x) inherits(x,"haven_labelled")), as.numeric)) |>
        transmute(hhid = URUT, kapita = KAPITA) |> distinct(hhid, .keep_all = TRUE)
  dat <- dat |> left_join(kp, by = "hhid")
  dat$wealth_q  <- wq5(dat$kapita, dat$w)
  dat$poor_q1q2 <- dat$wealth_q %in% c("Q1","Q2")
  message(glue("KAPITA matched: {sum(!is.na(dat$kapita))}/{nrow(dat)}"))
} else { dat$wealth_q <- factor(NA, paste0("Q",1:5)); dat$poor_q1q2 <- NA }

dat <- dat |> mutate(
  adolescent = age >= AGE_MIN & age <= AGE_MAX,
  ss_age     = age >= SS_AGE[1] & age <= SS_AGE[2],
  girl             = as.integer(female & adolescent),
  girl_rural       = as.integer(girl == 1 & rural),
  girl_poor        = as.integer(girl == 1 & poor_q1q2 %in% TRUE),
  girl_no_internet = NA_integer_,
  girl_priority    = as.integer(girl == 1 & (rural | poor_q1q2 %in% TRUE))
)

attr(dat,"vintage") <- "SUSENAS Maret 2024 — East Java (prov 35)"
attr(dat,"is_real") <- TRUE
saveRDS(dat, here::here("output","analysis_ready_full.rds"))
ado <- dat |> filter(adolescent)
saveRDS(ado, here::here("output","analysis_ready.rds"))

# ---- validation: girls 15-24 NEET should be ~30.5% (ToR) ----------------
gv <- ado |> filter(female)
neet_g <- weighted.mean(gv$neet, gv$w, na.rm = TRUE)
message(glue("\nAdolescents 15-24: {nrow(ado)} | girls {nrow(gv)}"))
message(glue("Girls 15-24 NEET (weighted): {sprintf('%.1f%%', 100*neet_g)}  [ToR benchmark ~30.5%]"))
message(glue("Girls in-school: {sprintf('%.1f%%',100*weighted.mean(gv$in_school,gv$w,na.rm=TRUE))} | ",
             "employed: {sprintf('%.1f%%',100*weighted.mean(gv$employed,gv$w,na.rm=TRUE))} | ",
             "married<18: {sprintf('%.1f%%',100*weighted.mean(gv$married_u18,gv$w,na.rm=TRUE))}"))
message("Stage 1 (SUSENAS 2024) complete → output/analysis_ready.rds")
