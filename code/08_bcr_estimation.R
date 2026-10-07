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
# 1. ASSUMPTIONS — ONE programme, ONE cost. Low/High are used ONLY for
#    one-at-a-time sensitivity (tornado); they are never stacked together.
#    Stacking every worst case against every best case is what produced the
#    earlier 0.03-18 BCR range, which no real programme would ever face.
# =============================================================================
# Return to schooling read from 04b: central = women, model (4) region + parents'
# education + selection; high = women, model (2) region only; low = SUSENAS consumption.
MINC_C <- 0.096; MINC_H <- 0.122
mc_path <- here::here("output","models","mincer_ifls_controls.csv")
if (file.exists(mc_path)) {
  mc <- read_csv(mc_path, show_col_types = FALSE) |> filter(Sex == "Women")
  v4 <- mc$`Return per year`[grepl("^\\(4\\)", mc$Model)]; v2 <- mc$`Return per year`[grepl("^\\(2\\)", mc$Model)]
  if (length(v4)) MINC_C <- v4[1]
  if (length(v2)) MINC_H <- max(v2[1], MINC_C)
}
message(sprintf("Return to schooling used: central %.1f%%, high %.1f%%, low 5.6%%", 100*MINC_C, 100*MINC_H))
A <- tribble(
  ~Parameter, ~Low, ~Central, ~High, ~Unit, ~Source,
  "NEET reduction per exposed girl", 0.025, 0.050, 0.075, "share (pp/100)",
    "Central: PIP attendance +11.4pp (Ulfa & Rezki 2024) x 0.45 pass-through to NEET. Band = 0.5x to 1.5x central.",
  "Extra years schooling if retained", 3, 3, 3, "years",
    "Full senior-secondary cycle (fixed by programme design)",
  "Mincer return per year of edu", 0.056, MINC_C, MINC_H, "share of earnings",
    "Central: IFLS5 female wage return with urban, province fixed effects, parents' education and Heckman selection (04b, model 4). High: region controls only (model 2). Low: SUSENAS 2024 consumption return (5.6%)",
  "Female annual wage (East Java)", 15e6, 20e6, 25e6, "IDR/yr",
    "Central Rp1.67m/month: below UMP Jawa Timur 2025 (~Rp2.31m) to reflect informal work; Sakernas national female mean Rp2.61m (Feb 2025)",
  "Female labour force participation", 0.45, 0.52, 0.60, "share", "BPS Sakernas, East Java female LFPR",
  "Real earnings growth", 0.00, 0.02, 0.03, "annual", "Experience/productivity growth over the career, below real GDP per capita growth",
  "Working life", 35, 35, 35, "years", "Age ~20 to ~55",
  "Years until earnings start", 5, 5, 5, "years", "3-year programme plus transition into work",
  "Discount rate", 0.05, 0.03, 0.02, "annual", "World Bank education default 3%",
  "Digital content & platform", 10, 10, 10, "USD/girl/yr", "Kemendikbudristek Merdeka Mengajar platform; INOVASI digital-learning unit costs",
  "Teacher training (annualised)", 15, 15, 15, "USD/girl/yr", "Double Track PD days, annualised",
  "Mentoring & socio-emotional", 10, 10, 10, "USD/girl/yr", "ELA/Skills4Girls facilitator costs",
  "School connectivity/devices (shared)", 15, 15, 15, "USD/girl/yr", "Shared labs, no 1:1 devices",
  "Ancillary: marriage-delay + flexible learning", 8, 8, 8, "USD/girl/yr", "ToR guidance: outreach to delay marriage, flexible options for caregivers",
  "Annual cost per girl (sensitivity)", 45, 58, 75, "USD/girl/yr", "Sum of components; band for cost uncertainty only",
  "Programme overhead", 0.15, 0.15, 0.15, "share of direct", "UNICEF typical 10-20%",
  "Years of programme exposure", 3, 3, 3, "years", "Senior-secondary cycle"
)
write_csv(A, file.path(OUT,"bcr_assumptions.csv"))
get <- function(p, s = "Central") A[[s]][A$Parameter == p]

# =============================================================================
# 2. CORE CALCULATION — any parameter can be overridden one at a time
# =============================================================================
calc <- function(over = list()) {
  p <- function(name) if (!is.null(over[[name]])) over[[name]] else get(name)
  w <- p("Female annual wage (East Java)") / IDR_PER_USD
  d <- p("Years until earnings start"); T <- p("Working life")
  r <- p("Discount rate"); g <- p("Real earnings growth")
  t <- d:(d + T - 1)
  per_year <- w * p("Female labour force participation") * p("Mincer return per year of edu") *
              p("NEET reduction per exposed girl") * p("Extra years schooling if retained")
  benefit <- sum(per_year * (1 + g)^(t - d) / (1 + r)^t)
  cost <- p("Annual cost per girl (sensitivity)") * p("Years of programme exposure") * (1 + p("Programme overhead"))
  list(benefit = benefit, cost = cost, bcr = benefit / cost,
       neet = p("NEET reduction per exposed girl"))
}

# =============================================================================
# 3. COSTING TABLE (single package)
# =============================================================================
cost_comp <- c("Digital content & platform","Teacher training (annualised)",
               "Mentoring & socio-emotional","School connectivity/devices (shared)",
               "Ancillary: marriage-delay + flexible learning")
costing <- A |> filter(Parameter %in% cost_comp) |> transmute(Component = Parameter, `USD/girl/yr` = Central, Source)
costing <- bind_rows(costing,
  tibble(Component = "Annual direct cost per girl", `USD/girl/yr` = sum(costing$`USD/girl/yr`), Source = "Sum"),
  tibble(Component = "Total per girl (3 years + 15% overhead)",
         `USD/girl/yr` = round(sum(costing$`USD/girl/yr`) * 3 * 1.15), Source = "Programme cost per girl"))
write_csv(costing, file.path(OUT,"bcr_costing_table.csv"))

# =============================================================================
# 4. HEADLINE: central + effect band, break-even, tornado
# =============================================================================
fmt <- function(x) format(round(x), big.mark = ",", scientific = FALSE)
scen <- list("Lower effect (2.5pp)" = list(`NEET reduction per exposed girl` = 0.025),
             "Central (5pp)" = list(),
             "Higher effect (7.5pp)" = list(`NEET reduction per exposed girl` = 0.075))
res <- bind_rows(lapply(names(scen), function(n) { x <- calc(scen[[n]])
  tibble(Scenario = n,
         `NEET cases averted per 1,000 girls` = round(1000 * x$neet),
         `Benefit PV (USD/girl)` = fmt(x$benefit),
         `Cost (USD/girl)` = fmt(x$cost),
         BCR = sprintf("%.2f", x$bcr),
         `Cost per NEET case averted (USD)` = fmt(x$cost / x$neet)) }))
write_csv(res, file.path(OUT,"bcr_scenarios_table.csv"))

base <- calc()
breakeven_pp <- 100 * base$cost / calc(list(`NEET reduction per exposed girl` = 1))$benefit
write_csv(tibble(`Break-even NEET reduction (pp)` = round(breakeven_pp, 1),
                 `Central assumed (pp)` = 5), file.path(OUT,"bcr_breakeven.csv"))

torn_params <- c("NEET reduction per exposed girl","Mincer return per year of edu",
                 "Female annual wage (East Java)","Female labour force participation",
                 "Real earnings growth","Discount rate","Annual cost per girl (sensitivity)")
tornado <- bind_rows(lapply(torn_params, function(p) {
  lo <- calc(setNames(list(get(p,"Low")),  p))$bcr
  hi <- calc(setNames(list(get(p,"High")), p))$bcr
  tibble(Parameter = p, `BCR at low value` = round(lo,2), `BCR at high value` = round(hi,2),
         spread = abs(hi - lo)) })) |> arrange(desc(spread))
write_csv(tornado, file.path(OUT,"bcr_tornado.csv"))

cat("\n================ COSTING (single package) ================\n"); print(costing |> select(-Source))
cat("\n================ BCR ================\n"); print(as.data.frame(res))
cat(sprintf("\nHeadline: every USD 1 invested returns about USD %.2f in lifetime earnings (central).\n", base$bcr))
cat(sprintf("Break-even: the programme pays for itself if it reduces girls' NEET by %.1fpp (central assumption: 5pp).\n", breakeven_pp))
cat("\n================ SENSITIVITY (one parameter at a time) ================\n"); print(as.data.frame(tornado |> select(-spread)))
message("Stage 8 complete -> output/projections/bcr_*.csv")
