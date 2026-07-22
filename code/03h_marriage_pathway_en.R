# 03h_marriage_pathway_en.R — child-marriage → schooling/exclusion pathway
# Descriptive (association, not causal), mirroring the Bolivia early-pregnancy
# pathway. Girls 15-24: contrast those married before 18 vs not, on
# in-school / NEET / employment, survey-weighted with 95% CIs.
suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(survey); library(here)
})
source(here::here("R","utils.R")); source(here::here("R","theme.R"))
# colour fallbacks (theme.R may not define all of these)
if (!exists("GREY_MID"))  GREY_MID  <- "#7A8487"
if (!exists("GREY_DARK")) GREY_DARK <- "#374649"
if (!exists("ACCENT_GIRL")) ACCENT_GIRL <- "#E2007A"
if (!exists("ACCENT_BOY"))  ACCENT_BOY  <- "#374EA2"
if (!exists("UNICEF_BLUE")) UNICEF_BLUE <- "#1CABE2"
ensure_dirs(); options(survey.lonely.psu = "adjust")

g <- readRDS(here::here("output","analysis_ready.rds")) |>
  filter(adolescent, female %in% c(TRUE,1)) |>
  mutate(
    grp = factor(ifelse(married_u18 %in% TRUE, "Married < 18", "Not married < 18"),
                 c("Not married < 18","Married < 18")),
    `In school` = as.numeric(in_school), NEET = as.numeric(neet),
    Employed = as.numeric(employed))

des <- svydesign(ids = ~psu, strata = ~strata, weights = ~w, data = g, nest = TRUE)

one <- function(v){
  svyby(as.formula(paste0("~`",v,"`")), ~grp, des, svymean, na.rm=TRUE) |>
    as.data.frame() |> setNames(c("grp","estimate","se")) |> mutate(outcome=v)
}
dat <- bind_rows(lapply(c("NEET","In school","Employed"), one)) |>
  mutate(outcome = factor(outcome, c("NEET","In school","Employed")),
         lo = pmax(0,estimate-1.96*se), hi = estimate+1.96*se)

pal <- c("Not married < 18" = UNICEF_BLUE, "Married < 18" = ACCENT_GIRL)
f <- ggplot(dat, aes(outcome, estimate, fill = grp)) +
  geom_col(position = position_dodge(.72), width = .64) +
  geom_errorbar(aes(ymin=lo,ymax=hi), position=position_dodge(.72),
                width=.15, colour=GREY_DARK, linewidth=.4) +
  geom_text(aes(y=hi, label=scales::percent(estimate,accuracy=1)),
            position=position_dodge(.72), vjust=-0.8, size=3.3, colour=GREY_DARK) +
  scale_fill_manual(values=pal, name=NULL) +
  scale_y_continuous(labels=scales::percent_format(1), expand=expansion(mult=c(0,.20))) +
  labs(x=NULL, y=NULL,
       subtitle="Girls married before 18 are far more likely to be NEET and out of school",
       caption=attr(g,"vintage")) +
  theme_minimal(base_size=13) +
  theme(panel.grid.major.x=element_blank(), panel.grid.minor=element_blank(),
        plot.subtitle=element_text(colour=GREY_DARK, margin=margin(b=8)),
        plot.caption=element_text(colour=GREY_MID, size=8), legend.position="top")
save_fig(f, "f9_marriage_pathway_en", width = 8.5, height = 5.4)

# survey-weighted association (linear prob.): NEET on married<18, adjusting age/area/poverty
adj <- tryCatch({
  m <- svyglm(NEET ~ married_u18 + age + rural + poor_q1q2, design = des)
  co <- summary(m)$coefficients
  co[grep("married", rownames(co)), , drop = FALSE]
}, error=function(e) NULL)

cat("\n=== NEET / in-school / employed by child-marriage status (weighted %) ===\n")
print(dat |> transmute(grp, outcome, pct=round(100*estimate,1)))
if(!is.null(adj)){ cat("\n=== Adjusted association: NEET ~ married<18 (+age,area,poverty) ===\n")
  cat(sprintf("Married<18 raises NEET probability by %.1f pp (SE %.1f), p=%.4f\n",
              100*adj[1,1], 100*adj[1,2], adj[1,4])) }
message("f9 marriage pathway saved.")
