big_mark <- function(x, ...) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x, big.mark = mark, ...)
}

cat_line_subtle <- function(...) {
  cli::cat_line(pillar::style_subtle(paste0(...)))
}
