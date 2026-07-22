# 08_bcr_estimation.R — Cost-benefit for Skills Development (East Java)
# -----------------------------------------------------------------------------
# Ported from the Bolivia Aula Conectada CBA (Grueso et al., 2026), adapted to
# the Indonesia ToR metrics: cost per NEET averted, BCR, NPV.
# Value chain:  φ-adjusted NEET reduction (pp) → extra years of effective
#   schooling → female Mincer return (IFLS5) × East-Java female wage × LFP ×
#   discounted working life → benefit PV per girl;  costs per ToC component
#   (incl. ancillary marriage-delay outreach + flexible learning per ToR).
# Every parameter is sourced and ranged (Low / Central / High); edit freely.
# Self-contained: does not require 05_projections.R.
# Outputs (read by slides): output/projections/bcr_{assumptions,costing,scenarios,ce}.csv
# -----------------------------------------------------------------------------
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(readr)
  library(tibble); library(here); library(glue) })
OUT <- here::here("output","projections"); dir.create(OUT, recursive=TRUE, showWarnings=FALSE)
IDR_PER_USD <- 15800   # ~2024 average; adjust as needed

# =============================================================================
# 1. ASSUMPTIONS — every number sourced; replace cost rows with Double Track actuals
# =============================================================================
A <- tribble(
  ~Parameter, ~Low, ~Central, ~High, ~Unit, ~Source,
  # --- EFFECT (φ-adjusted, girls-specific) ---
  "NEET reduction per exposed girl", 0.020, 0.050, 0.080, "share (pp/100)",
    "Anchored on Indonesian PIP/PKH (no transport discount, phi~1): PIP attendance +11.4pp (Ulfa & Rezki 2024); girls SHS enrolment +24.6pp (Caniago 2021). NEET reduction = attendance gain x ~0.45 pass-through (not all attendance gains come from would-be-NEET girls). Low keeps the conservative phi-discounted ELA value.",
  "Extra years schooling if retained", 2.0, 3.0, 4.0, "years",
    "Senior-secondary cycle a retained girl completes (ToR senior-secondary focus)",
  # --- BENEFIT side ---
  "Mincer return per year of edu", 0.054, 0.11, 0.12, "share of wage",  # Low = OWN SUSENAS 2024 consumption Mincer (welfare floor); High = IFLS5 wage return
    "IFLS5 East-Java female Mincer +12% = High (verified); Central 11% lightly selection-bounded; Low 7% (Danuza & Farah 2023 OLS ~5.7%, self-employment-adjusted)",
  "Female annual wage (East Java)", 14e6, 20e6, 30e6, "IDR/yr",
    "IFLS5 wage earners ×12; refine with Sakernas East Java female median",
  "Female labour force participation", 0.45, 0.52, 0.60, "share",
    "BPS/Sakernas female LFPR East Java ~50-55%; rural/urban range",
  "Working life", 30, 35, 40, "years", "Age ~20 (post-programme) to 50-60",
  "Discount rate", 0.05, 0.03, 0.02, "annual", "World Bank Education default 3%; 2-5% sensitivity",
  # --- COST side (USD/girl/yr) — PEDAGOGY-FIRST, no 1:1 hardware (OLPC caution) ---
  "Digital content & platform", 5, 10, 15, "USD/girl/yr", "IDB EdTech reviews 2018-22: licensing + curated content",
  "Teacher training (annualised)", 8, 15, 25, "USD/girl/yr", "Double Track PD days amortised; refine w/ provincial actuals",
  "Mentoring & socio-emotional", 5, 10, 15, "USD/girl/yr", "Skills4Girls mentoring; ELA-type facilitator cost",
  "School connectivity/devices (shared)", 8, 15, 25, "USD/girl/yr", "Shared lab/connectivity (not 1:1); rural premium in High",
  "Ancillary: marriage-delay + flexible learning", 4, 8, 14, "USD/girl/yr",
    "ToR (Ali Moechtar): community outreach to delay marriage + flexible options for caregivers",
  "Programme overhead", 0.10, 0.15, 0.20, "share of direct", "UNICEF typical 10-20%",
  "Years of programme exposure", 2, 3, 4, "years", "Mid-secondary uptake within Double Track"
)
write_csv(A, file.path(OUT,"bcr_assumptions.csv"))
get <- function(p,s) A[[s]][A$Parameter==p]

# override Mincer central with IFLS estimate if present
mp <- here::here("output","models","mincer_ifls.csv")
if (file.exists(mp)) {
  m <- read_csv(mp, show_col_types=FALSE) |> filter(sex=="Girls", term=="years_school")
  if (nrow(m)) message(glue("IFLS female Mincer on file: {sprintf('%.1f%%',100*m$estimate[1])} (Central kept at {get('Mincer return per year of edu','Central')})"))
}

# =============================================================================
# 2. BENEFIT — PV of lifetime earnings premium per exposed girl (USD)
# =============================================================================
wage_usd <- function(s) get("Female annual wage (East Java)", s) / IDR_PER_USD
years_edu <- function(s) get("NEET reduction per exposed girl", s) * get("Extra years schooling if retained", s)
benefit_pv <- function(s) {
  r <- get("Discount rate", s); T <- get("Working life", s)
  annuity <- (1 - (1+r)^(-T)) / r
  wage_usd(s) * get("Female labour force participation", s) *
    get("Mincer return per year of edu", s) * years_edu(s) * annuity
}

# =============================================================================
# 3. COST — sum of components × exposure × (1+overhead); USD/girl
# =============================================================================
cost_comp <- c("Digital content & platform","Teacher training (annualised)",
               "Mentoring & socio-emotional","School connectivity/devices (shared)",
               "Ancillary: marriage-delay + flexible learning")
costing <- A |> filter(Parameter %in% cost_comp) |>
  transmute(Component=Parameter, Low, Central, High, Source)
costing <- bind_rows(costing, tibble(Component="TOTAL per girl/year (USD)",
  Low=sum(costing$Low), Central=sum(costing$Central), High=sum(costing$High),
  Source="Sum of components"))
write_csv(costing, file.path(OUT,"bcr_costing_table.csv"))
tot <- costing |> filter(Component=="TOTAL per girl/year (USD)")
total_cost <- function(s){ ac <- switch(s, Low=tot$High, Central=tot$Central, High=tot$Low)
  yex <- switch(s, Low=get("Years of programme exposure","High"), Central=get("Years of programme exposure","Central"), High=get("Years of programme exposure","Low"))
  ov  <- switch(s, Low=get("Programme overhead","High"), Central=get("Programme overhead","Central"), High=get("Programme overhead","Low"))
  ac * yex * (1+ov) }

# =============================================================================
# 4. BCR + cost per NEET averted (Low / Central / High)
# =============================================================================
S <- c("Low","Central","High")
res <- tibble(
  Scenario = S,
  `Years edu gained` = sapply(S, function(s) sprintf("%.3f", years_edu(s))),
  `Benefit PV (USD/girl)` = sapply(S, function(s) round(benefit_pv(s))),
  `Cost (USD/girl)` = sapply(S, function(s) round(total_cost(s))),
  `NEET averted/girl` = sapply(S, function(s) get("NEET reduction per exposed girl", s)),
  BCR = sapply(S, function(s) benefit_pv(s)/total_cost(s)),
  `Cost per NEET averted (USD)` = sapply(S, function(s)
      if (get("NEET reduction per exposed girl", s) > 0) total_cost(s)/get("NEET reduction per exposed girl", s) else NA)
) |> mutate(
  `Benefit PV (USD/girl)` = formatC(`Benefit PV (USD/girl)`, format="d", big.mark=","),
  `Cost (USD/girl)` = formatC(`Cost (USD/girl)`, format="d", big.mark=","),
  BCR = ifelse(is.finite(BCR), sprintf("%.2f", BCR), "—"),
  `Cost per NEET averted (USD)` = ifelse(is.na(`Cost per NEET averted (USD)`), "n/a (null effect)",
       formatC(round(`Cost per NEET averted (USD)`), format="d", big.mark=","))
)
write_csv(res, file.path(OUT,"bcr_scenarios_table.csv"))

cat("\n================ COSTING (USD/girl/yr) ================\n"); print(costing |> select(-Source))
cat("\n================ BCR & COST-EFFECTIVENESS ================\n"); print(res)
cat(glue("\nHeadline (Central): BCR {res$BCR[2]} | cost per NEET averted {res$`Cost per NEET averted (USD)`[2]} USD | benefit PV {res$`Benefit PV (USD/girl)`[2]} USD/girl\n"))
cat("\nNOTE: effect & costs are sourced, ranged assumptions; Low scenario includes the null. Replace cost rows with Double Track actuals (MoF sub-national financing).\n")
message("Stage 8 complete → output/projections/bcr_*.csv")
