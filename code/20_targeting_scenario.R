# 20_targeting_scenario.R: inputs for a targeted version of the programme.
# A retention programme can only move girls who would otherwise leave school, so
# its effect is assumed to scale with the share of girls 16-18 who are out of
# school in the targeted group, relative to East Java as a whole. Each group also
# uses its own pass-through (share of school leavers without a senior-secondary
# certificate who are NEET), falling back to the East Java value when fewer than
# 40 such girls are in the sample. Poorest households: PKH recipients (2025 proxy,
# as the 2025 expenditure module is not available).
# Output: output/models/targeting_scenarios.csv (read by 08_bcr_estimation.R)
suppressPackageStartupMessages({ library(haven); library(dplyr); library(readr); library(here) })
num <- function(x) as.numeric(x)
cols <- c("R101","R102","R405","R407","R611","R615","R703_A","R703_B","R704","R2003A","WEIND")
r <- read_dta(here::here("data","Susenas 2025","SUSENAS_25-ALL-Merged.dta"), col_select = all_of(cols))
g <- tibble(prov = num(r$R101), kab = num(r$R102) %% 100, female = num(r$R405) == 2, age = num(r$R407),
            w = num(r$WEIND), cert = num(r$R615), pkh = num(r$R2003A) %in% 1,
            in_school = num(r$R611) %in% 2 | as.character(r$R703_B) == "B",
            working = as.character(r$R703_A) == "A" | num(r$R704) %in% 1) |>
  filter(prov == 35, female, !is.na(w)) |>
  mutate(neet = !in_school & !working, madura = kab %in% 26:29)
rm(r)

stats <- function(dd, label) {
  oos <- dd |> filter(age >= 16, age <= 18)
  lv  <- dd |> filter(age >= 16, age <= 20, !in_school, !(cert %in% 11:24))
  tibble(Group = label,
         `Out of school, girls 16-18 (%)` = round(100 * weighted.mean(!oos$in_school, oos$w), 1),
         `Pass-through (%)` = round(100 * weighted.mean(lv$neet, lv$w), 1),
         `School leavers in sample` = nrow(lv))
}
out <- bind_rows(stats(g, "East Java"), stats(g |> filter(madura), "Madura"), stats(g |> filter(pkh), "Poorest households (PKH)"))
ej <- out[out$Group == "East Java", ]
out <- out |> mutate(
  `Pass-through used (%)` = ifelse(`School leavers in sample` >= 40, `Pass-through (%)`, ej$`Pass-through (%)`),
  `Effect scale` = round(`Out of school, girls 16-18 (%)` / ej$`Out of school, girls 16-18 (%)`, 2),
  `NEET reduction (pp)` = round(11.4 * `Effect scale` * `Pass-through used (%)` / 100, 1))
write_csv(out, here::here("output","models","targeting_scenarios.csv"))
cat("\n=== Targeting scenario inputs (girls, East Java, SUSENAS 2025) ===\n"); print(as.data.frame(out))
message("20 done")
