# 08b_roi_figures.R: ROI figures built from the finished CBA tables (run after 08).
#  f26: cost vs stacked benefit per participating girl
#  f27: benefit-cost ratio by scenario, with the break-even line
suppressPackageStartupMessages({ library(dplyr); library(ggplot2); library(readr); library(here) })
source(here::here("R","utils.R"))
GREY_DARK <- "#374649"; GREY_MID <- "#7A8487"
sc <- read_csv(here::here("output","projections","bcr_scenarios_table.csv"), col_types = cols(.default = "c"))
n <- function(x) parse_number(x)
sk <- sc[grepl("skills earnings premium", sc$Scenario), ][1, ]
bars <- tibble(bar = factor(c("Cost", "Benefit", "Benefit"), c("Cost", "Benefit")),
               part = factor(c("Programme cost (3 years)", "Earnings gain from staying in school", "Skills premium, if it held (uncertain)"),
                             c("Programme cost (3 years)", "Skills premium, if it held (uncertain)", "Earnings gain from staying in school")),
               usd = c(n(sk$`Cost (USD/girl)`), n(sk$`Retention benefit (USD/girl)`), n(sk$`Skills premium benefit (USD/girl)`)))
print(bars)
f26 <- ggplot(bars, aes(bar, usd, fill = part)) +
  geom_col(width = .55) +
  geom_text(aes(label = paste0("USD ", round(usd))), position = position_stack(vjust = .5),
            colour = "white", fontface = "bold", size = 4.2) +
  scale_fill_manual(values = c("Programme cost (3 years)" = "#E2007A", "Earnings gain from staying in school" = "#00377C",
                               "Skills premium, if it held (uncertain)" = "#7fb3e0"), name = NULL) +
  labs(x = NULL, y = "USD per participating girl (present value)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right", panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())
save_fig(f26, "f26_roi_bars_en", width = 9, height = 4.6)

sb <- tibble(Scenario = sc$Scenario, BCR = n(sc$BCR)) |>
  mutate(Scenario = factor(stringr::str_wrap(Scenario, 28), stringr::str_wrap(Scenario, 28)))
f27 <- ggplot(sb, aes(Scenario, BCR)) +
  geom_col(fill = "#00377C", width = .55) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "#E2007A") +
  geom_text(aes(label = sprintf("%.2f", BCR)), vjust = -0.6, size = 4, colour = GREY_DARK) +
  labs(x = NULL, y = "Benefit-cost ratio", caption = "Dashed line: break-even (benefit-cost ratio of 1)") +
  scale_y_continuous(expand = expansion(mult = c(0, .2))) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(), plot.caption = element_text(colour = GREY_MID))
save_fig(f27, "f27_bcr_scenarios_en", width = 9, height = 4.8)
message("08b done -> f26, f27")
