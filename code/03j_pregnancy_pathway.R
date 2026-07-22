# 03j_pregnancy_pathway.R — EARLY PREGNANCY (<18) pathway, mirroring the child-marriage analysis.
# Blok XV (VSEN24.K): 1501B = age at first pregnancy, 1502B = age at first live birth,
# asked ONLY of ever-married women 10-54 -> stored in the ind2 (maternal) file.
# Merge into girls 15-24 by household (URUT) + person no. (R401).
# HONESTY NOTE: pregnancies among never-married girls are NOT measured by SUSENAS;
# we code never-married girls as no recorded early pregnancy -> estimates are a LOWER BOUND.
# Outputs: f12_pregnancy_by_wealth_en.png, f13_pregnancy_pathway_en.png,
#          output/models/pregnancy_tests.csv
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(survey); library(haven); library(here)
})
source(here::here("R","utils.R")); source(here::here("R","theme.R"))
if (!exists("GREY_MID"))  GREY_MID  <- "#7A8487"
if (!exists("GREY_DARK")) GREY_DARK <- "#374649"
if (!exists("ACCENT_GIRL")) ACCENT_GIRL <- "#E2007A"
if (!exists("UNICEF_DARK")) UNICEF_DARK <- "#00377C"
ensure_dirs(); options(survey.lonely.psu = "adjust")

DATA_DIR <- here::here("data","Susenas 2024")

# ---- 1. Locate the pregnancy variables ---------------------------------------
find_preg <- function(path){
  d <- read_dta(path, n_max = 200)
  nm <- names(d)
  list(nm = nm,
       preg  = grep("1501", nm, value = TRUE),
       birth = grep("1502", nm, value = TRUE),
       hh    = grep("^URUT$", nm, value = TRUE),
       pid   = intersect(c("R401","NO_URUT","NO_ART","NOART"), nm))
}
cand_files <- c("ssn202403_kor_ind2.dta", "SUSENAS_24.dta")
src <- NULL
for (f in cand_files){
  p <- file.path(DATA_DIR, f)
  if (!file.exists(p)) next
  info <- find_preg(p)
  cat("\n--", f, "--\n  1501*:", paste(info$preg, collapse=", "),
      "\n  1502*:", paste(info$birth, collapse=", "),
      "\n  ids  :", paste(c(info$hh, info$pid), collapse=", "), "\n")
  if (length(info$preg) > 0 && length(info$hh) == 1) { src <- list(path=p, info=info); break }
}
stopifnot("No file with Blok XV (1501*) variables found — paste the printout above." = !is.null(src))

# pick the AGE columns: prefer names ending in B (1501B/R1501B); else the numeric one in 10-45
pick_age_col <- function(d, cands){
  if (length(cands) == 0) return(NA_character_)
  bs <- grep("B$", cands, value = TRUE)
  if (length(bs) >= 1) return(bs[1])
  for (cc in cands){ v <- suppressWarnings(as.numeric(d[[cc]]))
    if (mean(v >= 8 & v <= 50, na.rm = TRUE) > .8) return(cc) }
  cands[1]
}
raw2 <- read_dta(src$path)
if ("R101" %in% names(raw2)) raw2 <- filter(raw2, as.numeric(R101) == 35)  # East Java only
preg_col  <- pick_age_col(raw2, src$info$preg)
birth_col <- pick_age_col(raw2, src$info$birth)
pid_col   <- if (length(src$info$pid) >= 1) src$info$pid[1] else NA
stopifnot("No person-id column in the pregnancy file." = !is.na(pid_col))
cat("\nUsing:", basename(src$path), "| pregnancy age:", preg_col,
    "| birth age:", birth_col, "| ids: URUT +", pid_col, "\n")

xv <- raw2 |>
  transmute(hhid = URUT, pid = as.numeric(.data[[pid_col]]),
            age_preg1  = { v <- suppressWarnings(as.numeric(.data[[preg_col]])); ifelse(v %in% 8:50, v, NA) },
            age_birth1 = if (!is.na(birth_col))
              { v <- suppressWarnings(as.numeric(.data[[birth_col]])); ifelse(v %in% 8:50, v, NA) } else NA_real_) |>
  distinct(hhid, pid, .keep_all = TRUE)

# ---- 2. Merge into girls 15-24 ----------------------------------------------
g <- readRDS(here::here("output","analysis_ready.rds")) |>
  filter(adolescent, female %in% c(TRUE,1))
stopifnot("Re-run 01c first: 'pid' missing from analysis_ready.rds" = "pid" %in% names(g))
g <- g |> left_join(xv, by = c("hhid","pid")) |>
  mutate(
    # ever-married girls with first pregnancy/birth <18; never-married coded 0 (unmeasured -> lower bound)
    preg_u18  = as.numeric(!is.na(age_preg1)  & age_preg1  < 18),
    birth_u18 = as.numeric(!is.na(age_birth1) & age_birth1 < 18),
    married = as.numeric(married_u18 %in% TRUE),
    NEET = as.numeric(neet), In_school = as.numeric(in_school), Employed = as.numeric(employed),
    poor = as.numeric(poor_q1q2 %in% TRUE), rural_n = as.numeric(rural %in% TRUE))
cat(sprintf("\nMerged Blok XV rows: %s of %s girls matched (ever-married subset expected small).\n",
    format(sum(!is.na(g$age_preg1)), big.mark=","), format(nrow(g), big.mark=",")))
des <- svydesign(ids=~psu, strata=~strata, weights=~w, data=g, nest=TRUE)

# ---- 3. f12: early pregnancy by wealth quintile (mirror of f11) --------------
bq <- svyby(~preg_u18, ~wealth_q, des, svymean, na.rm=TRUE) |> as.data.frame() |>
  setNames(c("wealth_q","rate","se")) |> filter(!is.na(wealth_q)) |>
  mutate(lo=pmax(0,rate-1.96*se), hi=rate+1.96*se)
f12 <- ggplot(bq, aes(wealth_q, rate)) +
  geom_col(fill=UNICEF_DARK, width=.68) +
  geom_errorbar(aes(ymin=lo,ymax=hi), width=.15, colour=GREY_DARK, linewidth=.4) +
  geom_text(aes(y=hi, label=scales::percent(rate,accuracy=.1)), vjust=-0.8, size=3.3, colour=GREY_DARK) +
  scale_y_continuous(labels=scales::percent_format(.1), expand=expansion(mult=c(0,.20))) +
  labs(x="Per-capita expenditure quintile (Q1 = poorest)", y=NULL,
       subtitle="Early pregnancy (first pregnancy before 18) is concentrated among the poorest girls",
       caption=paste0(attr(g,"vintage"), " · never-married girls unmeasured (lower bound)")) +
  theme_minimal(base_size=13) +
  theme(panel.grid.major.x=element_blank(), panel.grid.minor=element_blank(),
        plot.subtitle=element_text(colour=GREY_DARK, margin=margin(b=8)),
        plot.caption=element_text(colour=GREY_MID, size=8))
save_fig(f12, "f12_pregnancy_by_wealth_en", width=8.5, height=5.2)

# ---- 4. f13: outcomes by early pregnancy (mirror of f9) ----------------------
oc <- bind_rows(lapply(c("NEET","In_school","Employed"), function(v){
  svyby(as.formula(paste0("~",v)), ~preg_u18, des, svymean, na.rm=TRUE) |>
    as.data.frame() |> setNames(c("grp","rate","se")) |>
    mutate(outcome=gsub("_"," ",v)) })) |>
  mutate(grp = factor(ifelse(grp==1,"First pregnancy <18","No recorded pregnancy <18"),
                      c("First pregnancy <18","No recorded pregnancy <18")),
         lo=pmax(0,rate-1.96*se), hi=pmin(1,rate+1.96*se),
         outcome=factor(outcome, c("NEET","In school","Employed")))
f13 <- ggplot(oc, aes(outcome, rate, fill=grp)) +
  geom_col(position=position_dodge(.72), width=.62) +
  geom_errorbar(aes(ymin=lo,ymax=hi), position=position_dodge(.72), width=.15,
                colour=GREY_DARK, linewidth=.4) +
  geom_text(aes(y=hi, label=scales::percent(rate,accuracy=1)), position=position_dodge(.72),
            vjust=-0.8, size=3.2, colour=GREY_DARK) +
  scale_fill_manual(values=c("First pregnancy <18"=UNICEF_DARK,
                             "No recorded pregnancy <18"=GREY_MID), name=NULL) +
  scale_y_continuous(labels=scales::percent_format(1), expand=expansion(mult=c(0,.20))) +
  labs(x=NULL, y=NULL,
       subtitle="Girls with an early pregnancy are far likelier to be NEET and out of school",
       caption=paste0(attr(g,"vintage"), " · girls 15–24; 95% CIs; lower bound (see note)")) +
  theme_minimal(base_size=13) +
  theme(legend.position="top", panel.grid.major.x=element_blank(),
        panel.grid.minor=element_blank(),
        plot.subtitle=element_text(colour=GREY_DARK, margin=margin(b=8)),
        plot.caption=element_text(colour=GREY_MID, size=8))
save_fig(f13, "f13_pregnancy_pathway_en", width=9, height=5.4)

# ---- 5. Formal tests ---------------------------------------------------------
cf <- function(fit, term){ co<-summary(fit)$coefficients
  if(term %in% rownames(co)) co[term,c("Estimate","Std. Error","Pr(>|t|)")] else c(NA,NA,NA) }
row <- function(lbl, v, fit, controls) tibble(Hypothesis=lbl, `Effect (pp)`=round(100*v[1],1),
                               SE=round(100*v[2],1), p=round(v[3],4), N=nobs(fit), Controls=controls)
CTRL_BASE <- "age, rural"
CTRL_POV  <- "age, rural, poverty"
t1 <- svyglm(preg_u18  ~ poor + age + rural_n,            des)  # poverty -> early pregnancy
t2 <- svyglm(NEET      ~ preg_u18 + age + rural_n + poor, des)  # pregnancy -> NEET
t3 <- svyglm(In_school ~ preg_u18 + age + rural_n + poor, des)
t4 <- svyglm(Employed  ~ preg_u18 + age + rural_n + poor, des)
t5 <- svyglm(married   ~ preg_u18 + age + rural_n + poor, des)  # sequencing: pregnancy <-> child marriage
t6 <- svyglm(NEET      ~ preg_u18 * poor + age + rural_n, des)  # wealth moderation
tests <- bind_rows(
  row("Poverty (Q1–Q2) → early pregnancy",               cf(t1,"poor"),         t1, CTRL_BASE),
  row("Early pregnancy → NEET",                           cf(t2,"preg_u18"),     t2, CTRL_POV),
  row("Early pregnancy → in school",                      cf(t3,"preg_u18"),     t3, CTRL_POV),
  row("Early pregnancy → employment",                     cf(t4,"preg_u18"),     t4, CTRL_POV),
  row("Early pregnancy ↔ child marriage (association)",   cf(t5,"preg_u18"),     t5, CTRL_POV),
  row("Poverty × early pregnancy (NEET amplification)",   cf(t6,"preg_u18:poor"),t6, CTRL_BASE))
readr::write_csv(tests, here::here("output","models","pregnancy_tests.csv"))
cat("\n=== EARLY PREGNANCY: poverty link, outcomes, marriage sequencing (girls 15-24) ===\n")
print(as.data.frame(tests))
cat(sprintf("\nPrevalence: first pregnancy <18 = %.1f%% | first birth <18 = %.1f%% (weighted; lower bound).\n",
  100*svymean(~preg_u18, des, na.rm=TRUE)[1], 100*svymean(~birth_u18, des, na.rm=TRUE)[1]))
message("03j done → f12, f13, pregnancy_tests.csv")
