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
er_path <- here::here("output","models","female_employment_rate.csv")
EMP_RATE <- if (file.exists(er_path)) read_csv(er_path, show_col_types = FALSE)$value[1] else 0.52
message(sprintf("Female employment rate used: %.1f%%", 100*EMP_RATE))
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
  "Female labour force participation", 0.45, EMP_RATE, 0.65, "share",
    "Employment rate of women 25-54 in East Java, SUSENAS Maret 2025 (14_targeting_followup.R); the probability of earning, not LFP",
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
  "Skills earnings premium (all exposed girls who work)", 0.00, 0.00, 0.056, "share of earnings",
    "Central = 0 (no causal evidence for an add-on skills course). High = 5.6%, the point estimate for vocational vs general schooling in Pritadrajati (2022), not statistically significant (SE 0.076)",
  "Years the skills premium lasts", 5, 10, 20, "years", "Conservative: well short of the working life",
  "Cash+ stipend cost (optional)", 0, 0, 46, "USD/girl/yr",
    "Central: girls linked to existing PKH/PIP, no new transfer. High: PIP senior-secondary rate (~Rp1.8m/yr, verify with Kemendikbudristek) for the poorest 40% of girls",
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
  benefit_ret <- sum(per_year * (1 + g)^(t - d) / (1 + r)^t)          # channel 1: retention -> NEET
  tp <- d:(d + p("Years the skills premium lasts") - 1)
  benefit_prem <- sum(w * p("Female labour force participation") * p("Skills earnings premium (all exposed girls who work)") *
                      (1 + g)^(tp - d) / (1 + r)^tp)                    # channel 2: skills premium, all who work
  cost <- (p("Annual cost per girl (sensitivity)") + p("Cash+ stipend cost (optional)")) *
          p("Years of programme exposure") * (1 + p("Programme overhead"))
  list(benefit = benefit_ret + benefit_prem, benefit_ret = benefit_ret, benefit_prem = benefit_prem,
       cost = cost, bcr = (benefit_ret + benefit_prem) / cost, neet = p("NEET reduction per exposed girl"),
       gain_per_switch = benefit_ret / p("NEET reduction per exposed girl"))
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
scen <- list("Central: retention channel (Indonesian causal evidence)" = list(),
             "Plus skills earnings premium of 5.6% (point estimate, not statistically significant)" = list(`Skills earnings premium (all exposed girls who work)` = 0.056),
             "Retention channel, earnings at the 2025 minimum wage" = list(`Female annual wage (East Java)` = 2.31e6 * 12),
             "Retention channel, with Cash+ stipend for the poorest 40%" = list(`Cash+ stipend cost (optional)` = 46))
res <- bind_rows(lapply(names(scen), function(n) { x <- calc(scen[[n]])
  tibble(Scenario = n,
         `Retention benefit (USD/girl)` = fmt(x$benefit_ret),
         `Skills premium benefit (USD/girl)` = fmt(x$benefit_prem),
         `Total benefit (USD/girl)` = fmt(x$benefit),
         `Cost (USD/girl)` = fmt(x$cost),
         BCR = sprintf("%.2f", x$bcr),
         `Cost per NEET case averted (USD)` = fmt(x$cost / x$neet)) }))
write_csv(res, file.path(OUT,"bcr_scenarios_table.csv"))

base <- calc()
floor0 <- list()
with_skills <- list(`Skills earnings premium (all exposed girls who work)` = 0.056)
breakeven_pp <- 100 * base$cost / calc(c(floor0, list(`NEET reduction per exposed girl` = 1)))$benefit_ret
write_csv(tibble(`Break-even NEET reduction (pp)` = round(breakeven_pp, 1), `Central assumed (pp)` = 5), file.path(OUT,"bcr_breakeven.csv"))
steps <- tibble(Step = c("Extra years of schooling for a girl kept in school", "Earnings gain per year of schooling",
                         "Earnings gain for that girl", "Female earnings (USD/yr) x labour force participation",
                         "Extra earnings per year (USD)", "Present value over the working life (USD per girl moved out of NEET)",
                         "Girls exposed per girl moved out of NEET (1 / NEET reduction)", "Cost per girl moved out of NEET (USD)",
                         "Return per USD 1, retention channel (central)", "Skills premium of 5.6% for all exposed girls who work, if it held (USD per exposed girl, PV)",
                         "Return per USD 1 if the skills premium held"),
  Value = c("3", sprintf("%.1f%%", 100*get("Mincer return per year of edu")), sprintf("%.0f%%", 100*3*get("Mincer return per year of edu")),
            sprintf("%s x %.0f%%", fmt(get("Female annual wage (East Java)")/IDR_PER_USD), 100*get("Female labour force participation")),
            fmt(get("Female annual wage (East Java)")/IDR_PER_USD * get("Female labour force participation") * 3 * get("Mincer return per year of edu")),
            fmt(base$gain_per_switch), sprintf("%.0f", 1/base$neet), fmt(base$cost / base$neet),
            sprintf("%.2f", base$bcr), fmt(calc(with_skills)$benefit_prem), sprintf("%.2f", calc(with_skills)$bcr)))
write_csv(steps, file.path(OUT,"bcr_steps.csv"))

torn_params <- c("NEET reduction per exposed girl","Mincer return per year of edu",
                 "Female annual wage (East Java)","Female labour force participation",
                 "Real earnings growth","Discount rate","Annual cost per girl (sensitivity)",
                 "Skills earnings premium (all exposed girls who work)","Years the skills premium lasts")
tornado <- bind_rows(lapply(torn_params, function(p) {
  lo <- calc(setNames(list(get(p,"Low")),  p))$bcr
  hi <- calc(setNames(list(get(p,"High")), p))$bcr
  tibble(Parameter = p, `BCR at low value` = round(lo,2), `BCR at high value` = round(hi,2),
         spread = abs(hi - lo)) })) |> arrange(desc(spread))
write_csv(tornado, file.path(OUT,"bcr_tornado.csv"))

cat("\n================ COSTING (single package) ================\n"); print(costing |> select(-Source))
cat("\n================ BCR ================\n"); print(as.data.frame(res))
cat(sprintf("\nHeadline: every USD 1 invested returns about USD %.2f (central, retention channel); USD %.2f if the 5.6%% skills premium held.\n", base$bcr, calc(with_skills)$bcr))
cat("\n================ HOW THE NUMBER IS BUILT ================\n"); print(as.data.frame(steps))
cat(sprintf("Break-even (retention channel only): pays for itself at a %.1fpp NEET reduction (central assumption: 5pp).\n", breakeven_pp))
cat("\n================ SENSITIVITY (one parameter at a time) ================\n"); print(as.data.frame(tornado |> select(-spread)))

# ---- ROI figures: cost vs stacked benefit; BCR by scenario with break-even line ----
suppressPackageStartupMessages(library(ggplot2)); source(here::here("R","utils.R"))
if (!exists("GREY_DARK")) GREY_DARK <- "#374649"; if (!exists("GREY_MID")) GREY_MID <- "#7A8487"
ws <- calc(with_skills)
bars <- tibble(bar = c("Cost","Cost","Benefit","Benefit"),
               part = c("Programme cost (3 years)","", "Earnings gain from staying in school", "Skills premium, if it held (uncertain)"),
               usd = c(ws$cost, 0, ws$benefit_ret, ws$benefit_prem)) |> filter(usd > 0)
f26 <- ggplot(bars, aes(x = factor(bar, c("Cost","Benefit")), y = usd, fill = part)) +
  geom_col(width = .55) +
  geom_text(aes(label = paste0("USD ", round(usd))), position = position_stack(vjust = .5), colour = "white", fontface = "bold", size = 4) +
  scale_fill_manual(values = c("Programme cost (3 years)" = "#E2007A", "Earnings gain from staying in school" = "#00377C",
                               "Skills premium, if it held (uncertain)" = "#9fc5e8"), name = NULL) +
  labs(x = NULL, y = "USD per participating girl (present value)",
       subtitle = "Per participating girl: cost against modelled lifetime benefits",
       caption = "Retention benefit rests on Indonesian causal evidence; the skills premium is a point estimate that is not statistically significant") +
  theme_minimal(base_size = 13) + theme(legend.position = "top", panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
                                        plot.subtitle = element_text(colour = GREY_DARK), plot.caption = element_text(colour = GREY_MID, size = 8))
save_fig(f26, "f26_roi_bars_en", width = 8.5, height = 5.4)
sb <- tibble(Scenario = c("Retention channel\n(central)", "With 5.6% skills premium\n(uncertain)", "Retention, earnings at\n2025 minimum wage", "Retention, plus Cash+\nstipend for poorest 40%"),
             BCR = c(base$bcr, ws$bcr, calc(list(`Female annual wage (East Java)` = 2.31e6*12))$bcr, calc(list(`Cash+ stipend cost (optional)` = 46))$bcr))
f27 <- ggplot(sb, aes(x = factor(Scenario, Scenario), y = BCR)) +
  geom_col(fill = "#00377C", width = .55) + geom_hline(yintercept = 1, linetype = "dashed", colour = "#E2007A") +
  annotate("text", x = 4.4, y = 1.06, label = "break-even", colour = "#E2007A", size = 3.4, hjust = 1) +
  geom_text(aes(label = sprintf("%.2f", BCR)), vjust = -0.6, size = 4, colour = GREY_DARK) +
  scale_y_continuous(expand = expansion(mult = c(0, .2))) +
  labs(x = NULL, y = "Benefit-cost ratio", subtitle = "Benefit-cost ratio by scenario") +
  theme_minimal(base_size = 13) + theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(), plot.subtitle = element_text(colour = GREY_DARK))
save_fig(f27, "f27_bcr_scenarios_en", width = 8.5, height = 5)
message("Stage 8 complete -> output/projections/bcr_*.csv, f26, f27")
