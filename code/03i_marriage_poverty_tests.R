# 03i_marriage_poverty_tests.R — poverty <-> child marriage, and formal tests of
# the marriage -> education/labour pathway, with a wealth-moderation test.
# All survey-weighted (SUSENAS Maret 2024), girls 15-24, PSU-clustered SEs.
# Motivates a Cash+ (cash-transfer) component: if poverty drives marriage and
# poverty amplifies the marriage penalty, transfers can break the chain.
# Outputs: output/figures/f11_marriage_by_wealth_en.png ; output/models/marriage_tests.csv
suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(survey); library(here)
})
source(here::here("R","utils.R")); source(here::here("R","theme.R"))
if (!exists("GREY_MID"))  GREY_MID  <- "#7A8487"
if (!exists("GREY_DARK")) GREY_DARK <- "#374649"
if (!exists("ACCENT_GIRL")) ACCENT_GIRL <- "#E2007A"
if (!exists("UNICEF_DARK")) UNICEF_DARK <- "#00377C"
ensure_dirs(); options(survey.lonely.psu = "adjust")

g <- readRDS(here::here("output","analysis_ready.rds")) |>
  filter(adolescent, female %in% c(TRUE,1)) |>
  mutate(NEET=as.numeric(neet), In_school=as.numeric(in_school), Employed=as.numeric(employed),
         married=as.numeric(married_u18 %in% TRUE),
         poor=as.numeric(poor_q1q2 %in% TRUE), rural_n=as.numeric(rural %in% TRUE))
des <- svydesign(ids=~psu, strata=~strata, weights=~w, data=g, nest=TRUE)

# ---- Plot f11: child-marriage rate by wealth quintile -----------------------
bq <- svyby(~married, ~wealth_q, des, svymean, na.rm=TRUE) |> as.data.frame() |>
  setNames(c("wealth_q","rate","se")) |> filter(!is.na(wealth_q)) |>
  mutate(lo=pmax(0,rate-1.96*se), hi=rate+1.96*se)
f11 <- ggplot(bq, aes(wealth_q, rate)) +
  geom_col(fill=ACCENT_GIRL, width=.68) +
  geom_errorbar(aes(ymin=lo,ymax=hi), width=.15, colour=GREY_DARK, linewidth=.4) +
  geom_text(aes(y=hi, label=scales::percent(rate,accuracy=.1)), vjust=-0.8, size=3.3, colour=GREY_DARK) +
  scale_y_continuous(labels=scales::percent_format(.1), expand=expansion(mult=c(0,.20))) +
  labs(x="Per-capita expenditure quintile (Q1 = poorest)", y=NULL,
       subtitle="Child marriage is concentrated among the poorest girls",
       caption=attr(g,"vintage")) +
  theme_minimal(base_size=13) +
  theme(panel.grid.major.x=element_blank(), panel.grid.minor=element_blank(),
        plot.subtitle=element_text(colour=GREY_DARK, margin=margin(b=8)),
        plot.caption=element_text(colour=GREY_MID, size=8))
save_fig(f11, "f11_marriage_by_wealth_en", width=8.5, height=5.2)

# ---- Formal tests (linear probability, survey-weighted) ---------------------
cf <- function(fit, term){ co<-summary(fit)$coefficients
  if(term %in% rownames(co)) co[term,c("Estimate","Std. Error","Pr(>|t|)")] else c(NA,NA,NA) }

t1 <- svyglm(married    ~ poor + age + rural_n,              des)   # H3 poverty -> marriage
t2 <- svyglm(NEET       ~ married + age + rural_n + poor,    des)   # H1 marriage -> NEET
t3 <- svyglm(In_school  ~ married + age + rural_n + poor,    des)   # marriage -> in school
t4 <- svyglm(Employed   ~ married + age + rural_n + poor,    des)   # marriage -> employment
t5 <- svyglm(NEET       ~ married * poor + age + rural_n,    des)   # H4 wealth moderation

# ---- Robustness: district fixed effects --------------------------------------
# With ~38 districts and n~6,200 girls (fewer still for the ever-married
# subsample used in 03j), district FE is a genuine robustness check but not
# the primary spec: it absorbs real local variation (labour markets, norms,
# programme rollout) at the cost of power, especially for rarer outcomes.
# Report alongside, don't replace, the base model.
t2_fe <- svyglm(NEET ~ married + age + rural_n + poor + factor(kabkota), des)
cat(sprintf("\nRobustness (district FE): Child marriage -> NEET = %.1fpp (base: %.1fpp)\n",
    100*cf(t2_fe,"married")[1], 100*cf(t2,"married")[1]))

row <- function(lbl, v, fit, controls){ tibble(Hypothesis=lbl, `Effect (pp)`=round(100*v[1],1),
                                SE=round(100*v[2],1), p=round(v[3],4),
                                N=nobs(fit), Controls=controls) }
CTRL_BASE <- "age, rural"
CTRL_POV  <- "age, rural, poverty"
tests <- dplyr::bind_rows(
  row("Poverty (Q1–Q2) → child marriage",              cf(t1,"poor"),         t1, CTRL_BASE),
  row("Child marriage → NEET",                          cf(t2,"married"),      t2, CTRL_POV),
  row("Child marriage → in school",                     cf(t3,"married"),      t3, CTRL_POV),
  row("Child marriage → employment",                    cf(t4,"married"),      t4, CTRL_POV),
  row("Poverty × child marriage (NEET amplification)",  cf(t5,"married:poor"), t5, CTRL_BASE)
)
readr::write_csv(tests, here::here("output","models","marriage_tests.csv"))

cat("\n=== POVERTY <-> CHILD MARRIAGE, and marriage -> outcomes (girls 15-24, weighted) ===\n")
print(as.data.frame(tests))
cat("\nRead: a positive 'Poverty × child marriage' term means the marriage penalty is LARGER for poor girls\n(=> addressing poverty via cash transfers can mitigate it).\n")
message("03i done → f11_marriage_by_wealth_en.png, marriage_tests.csv")
