# 00d_inventory.R — what is in every SUSENAS file we hold?
# For each .dta in data/Susenas 2024 and data/Susenas 2025: rows, columns,
# whether it is national or East-Java-only, and which key covariates exist.
suppressPackageStartupMessages(library(haven))

folders <- c("data/Susenas 2024", "data/Susenas 2025")

key_vars <- list(
  "Design / weights"         = c("PSU","SSU","STRATA","FWT","WEIND","WERT"),
  "Geography"                = c("R101","R102","R105"),
  "Demographics"             = c("R401","R403","R404","R405","R407","R409"),
  "Schooling"                = c("R610","R611","R612","R613","R614","R615"),
  "Education support"        = c("R616","R617","R618","R619","R620"),
  "Activity / employment"    = c("R703_A","R703_B","R704","R705","R706","R707","R708"),
  "Digital"                  = c("R802","R805","R808"),
  "Disability"               = c("R1001","R1002","R1003","R1004","R1005","R1006","R1007","R1008","R1009","R1010"),
  "Pregnancy (Blok XV/XIV)"  = c("R1401B","R1402B","R1501B","R1502B"),
  "Household size"           = c("R301","R302"),
  "Social protection"        = c("R2002","R2003A","R2005","R2202","R2203"),
  "Expenditure"              = c("KAPITA","EXPEND","PENGELUARAN")
)

for (fd in folders) {
  cat("\n", strrep("=", 78), "\n FOLDER: ", fd, "\n", strrep("=", 78), "\n", sep = "")
  if (!dir.exists(fd)) { cat("  (folder not found)\n"); next }
  files <- list.files(fd, full.names = TRUE)
  for (f in files) {
    cat(sprintf("\n-- %s  (%.1f MB)\n", basename(f), file.size(f) / 1e6))
    if (!grepl("\\.dta$", f, ignore.case = TRUE)) next
    nm <- names(read_dta(f, n_max = 0))
    cat(sprintf("   columns: %d\n", length(nm)))
    if ("R101" %in% nm) {
      prov <- as.numeric(read_dta(f, col_select = "R101")$R101)
      cat(sprintf("   rows: %s | provinces present: %d %s\n",
          format(length(prov), big.mark = ","), length(unique(prov)),
          ifelse(length(unique(prov)) > 1, "-> NATIONAL file", "-> single province (filtered)")))
      if (35 %in% prov) cat(sprintf("   East Java (35) rows: %s\n", format(sum(prov == 35), big.mark = ",")))
    } else {
      cat("   no R101 column (likely a household-level or module file; check join key)\n")
      cat("   first columns:", paste(head(nm, 12), collapse = ", "), "\n")
    }
    for (grp in names(key_vars)) {
      have <- intersect(key_vars[[grp]], nm)
      if (length(have)) cat(sprintf("   %-26s %s\n", grp, paste(have, collapse = ", ")))
    }
    expn <- grep("KAPITA|EXP|PENGE|KONS", nm, value = TRUE, ignore.case = TRUE)
    if (length(expn)) cat("   expenditure-like columns:", paste(expn, collapse = ", "), "\n")
  }
}
cat("\nDone. Paste this whole output back.\n")
