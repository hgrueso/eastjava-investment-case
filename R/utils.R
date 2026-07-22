# R/utils.R — shared helpers used across all East Java analysis scripts.
suppressPackageStartupMessages({ library(here); library(ggplot2) })

ensure_dirs <- function() {
  for (d in c("output", "output/figures", "output/tables",
              "output/models", "output/projections")) {
    dir.create(here::here(d), showWarnings = FALSE, recursive = TRUE)
  }
  message(here::here("output"))
}

save_fig <- function(plot, name, width = 8, height = 5, dpi = 220) {
  path <- here::here("output", "figures", paste0(name, ".png"))
  ggsave(path, plot, width = width, height = height, dpi = dpi, bg = "white")
  message("  saved figure: ", basename(path))
  invisible(path)
}

write_table <- function(df, name) {
  path <- here::here("output", "tables", paste0(name, ".csv"))
  readr::write_csv(df, path)
  message("  saved table: ", basename(path))
  invisible(path)
}

wquantile <- function(x, w, probs = seq(0, 1, .2), na.rm = TRUE) {
  ok <- if (na.rm) !is.na(x) & !is.na(w) else rep(TRUE, length(x))
  x <- x[ok]; w <- w[ok]
  ord <- order(x)
  x <- x[ord]; w <- w[ord]
  cw <- cumsum(w) / sum(w)
  sapply(probs, function(p) x[which(cw >= p)[1]])
}

wq5 <- function(x, w) {
  brks <- wquantile(x, w, probs = seq(0, 1, .2))
  brks[1] <- -Inf; brks[length(brks)] <- Inf
  cut(x, breaks = unique(brks), labels = paste0("Q", seq_len(length(unique(brks)) - 1)),
      include.lowest = TRUE)
}

g <- function(raw, col) {
  if (col %in% names(raw)) as.numeric(raw[[col]]) else rep(NA_real_, nrow(raw))
}
