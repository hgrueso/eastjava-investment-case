# 09_lifetime_productivity_loss.R — "what East Java is losing right now"
# Distinct from the BCR (which values a FUTURE intervention's benefit): this
# is a snapshot of forgone lifetime earnings TODAY because of the CURRENT
# NEET gender gap — i.e. if NEET girls right now had the same employment
# rate as boys, valued at the female Mincer-implied wage over their
# discounted remaining working life. One headline number for policymakers.
#
# Method:
#   1. N_gap = (girls' NEET rate - boys' NEET rate) x female youth population
#      = the number of "excess" NEET girls attributable to the gender gap
#      (a conservative choice: only counts the GAP, not all NEET girls, so
#      the number isn't inflated by NEET drivers common to both sexes)
#   2. Annual wage each would earn if employed = East-Java female wage,
#      scaled by LFP among employed comparators (same inputs as 08_bcr)
#   3. PV of forgone earnings = annual wage x discounted annuity over
#      remaining expected working years (same annuity engine as 08_bcr, so
#      the two analyses are consistent with each other)
#   4. Reported per girl AND aggregated to the province
# Outputs: output/projections/lifetime_loss.csv, f15_lifetime_loss_en.png
suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(readr); library(here); library(glue)
})
source(here::here("R","utils.R")); source(here::here("R","theme.R"))
if (!exists("GREY_MID"))  GREY_MID  <- "#7A8487"
if (!exists("GREY_DARK")) GREY_DARK <- "#374649"
if (!exists("ACCENT_GIRL")) ACCENT_GIRL <- "#E2007A"
if (!exists("UNICEF_DARK")) UNICEF_DARK <- "#00377C"
ensure_dirs()

g <- readRDS(here::here("output","analysis_ready.rds")) |> filter(adolescent)
A <- tryCatch(read_csv(here::here("output","projections","bcr_assumptions.csv"), show_col_types = FALSE),
              error = function(e) NULL)
stopifnot("Run 08_bcr_estimation.R first (need output/projections/bcr_assumptions.csv)." = !is.null(A))
getp <- function(param, scen = "Central") A[[scen]][A$Parameter == param]

girls <- g |> filter(female %in% c(TRUE,1))
boys  <- g |> filter(!(female %in% c(TRUE,1)))
neet_girls <- weighted.mean(girls$neet, girls$w, na.rm = TRUE)
neet_boys  <- weighted.mean(boys$neet,  boys$w,  na.rm = TRUE)
gap_pp <- neet_girls - neet_boys

pop_girls <- sum(girls$w, na.rm = TRUE)          # weighted female youth population, 15-24
n_excess_neet <- pop_girls * gap_pp               # "excess" NEET girls attributable to the gender gap

annual_wage_idr <- getp("Female annual wage (East Java)")
lfp             <- getp("Female labour force participation")
disc_rate       <- getp("Discount rate")
work_years      <- getp("Working life")
idr_per_usd     <- 15800   # same constant as 08_bcr_estimation.R (IDR_PER_USD) — keep in sync if that changes
if (any(sapply(list(annual_wage_idr, lfp, disc_rate, work_years), length) == 0))
  stop("One or more assumption rows not found in bcr_assumptions.csv — check Parameter names match 08_bcr_estimation.R exactly.")

annuity <- (1 - (1 + disc_rate)^(-work_years)) / disc_rate
pv_per_girl_idr <- annual_wage_idr * lfp * annuity
pv_per_girl_usd <- pv_per_girl_idr / idr_per_usd

total_pv_usd <- n_excess_neet * pv_per_girl_usd
total_pv_idr <- total_pv_usd * idr_per_usd

out <- tibble(
  Metric = c("Girls' NEET rate", "Boys' NEET rate", "Gender gap (pp)",
             "Female youth population (15-24, weighted)", "Excess NEET girls (gap x population)",
             "PV of forgone lifetime earnings, per excess-NEET girl (USD)",
             "PV of forgone lifetime earnings, per excess-NEET girl (IDR)",
             "TOTAL forgone lifetime productivity, East Java (USD)",
             "TOTAL forgone lifetime productivity, East Java (IDR, billion)"),
  Value = c(sprintf("%.1f%%", 100*neet_girls), sprintf("%.1f%%", 100*neet_boys),
            sprintf("%.1f pp", 100*gap_pp),
            format(round(pop_girls), big.mark=","), format(round(n_excess_neet), big.mark=","),
            format(round(pv_per_girl_usd), big.mark=",", scientific=FALSE),
            format(round(pv_per_girl_idr), big.mark=",", scientific=FALSE),
            format(round(total_pv_usd), big.mark=",", scientific=FALSE),
            formatC(round(total_pv_idr/1e9, 1), format="f", digits=1, big.mark=","))
)
write_csv(out, here::here("output","projections","lifetime_loss.csv"))

cat("\n================ LIFETIME PRODUCTIVITY LOSS (current gender gap) ================\n")
print(out)
cat(glue("\nHeadline: the {sprintf('%.1f', 100*gap_pp)}pp NEET gender gap represents ",
         "~{format(round(n_excess_neet), big.mark=',')} excess NEET girls in East Java, ",
         "worth an estimated USD {format(round(total_pv_usd), big.mark=',', scientific=FALSE)} ",
         "(~IDR {formatC(round(total_pv_idr/1e9,1), format='f', digits=1)} billion) in forgone ",
         "lifetime productivity — TODAY, not a future benefit.\n"))

# --- One-number figure: per-girl loss vs. province-wide total, one ggplot, no extra deps ---
plot_df <- tibble(
  panel = factor(c("Per excess-NEET girl (USD)", "Province-wide, today (USD millions)"),
                  levels = c("Per excess-NEET girl (USD)", "Province-wide, today (USD millions)")),
  y = c(pv_per_girl_usd, total_pv_usd/1e6),
  lab = c(paste0("USD ", format(round(pv_per_girl_usd), big.mark=",", scientific=FALSE)),
          paste0("USD ", formatC(round(total_pv_usd/1e6,1), format="f", digits=1, big.mark=","), "M")),
  fill = c(ACCENT_GIRL, UNICEF_DARK)
)
f15 <- ggplot(plot_df, aes(x = 1, y = y, fill = panel)) +
  geom_col(width = .5, show.legend = FALSE) +
  geom_text(aes(label = lab), vjust = -0.6, size = 5.2, fontface = "bold", colour = GREY_DARK) +
  scale_fill_manual(values = setNames(plot_df$fill, plot_df$panel)) +
  facet_wrap(~panel, scales = "free_y") +
  scale_y_continuous(expand = expansion(mult = c(0,.28))) +
  labs(title = "Forgone lifetime productivity from the NEET gender gap",
       subtitle = sprintf("Excess NEET girls (%.1fpp gap above the boys' rate) x present value of lifetime earnings", 100*gap_pp),
       x = NULL, y = NULL, caption = attr(g, "vintage")) +
  theme_void(base_size = 13) +
  theme(strip.text = element_text(size = 11, colour = GREY_DARK, face = "bold"),
        plot.title = element_text(face = "bold", size = 15, margin = margin(b=4)),
        plot.subtitle = element_text(size = 10, colour = GREY_DARK, margin = margin(b=14)),
        plot.caption = element_text(size = 8, colour = GREY_MID),
        panel.spacing = unit(2, "lines"))
save_fig(f15, "f15_lifetime_loss_en", width = 9, height = 5.2)
message("09 done -> lifetime_loss.csv, f15_lifetime_loss_en.png")
