#' Wrap functions for step-by-step data processing
#'
#' `r lifecycle::badge("experimental")`
#'
#' @description
#' `step_by_step()` creates a step-by-step object that allows you to apply
#' functions to data in a sequence.
#'
#' @param fns A list of functions to be applied step by step.
#' @param descriptions A character vector of step descriptions.
#' @param ... Additional arguments for attributes.
#'
#' @return A function that takes `data` and returns an object that inherits from
#' `adverbial_object_step_by_step`.
#'
#' @export
step_by_step <- function(fns, descriptions = NULL, ...) {
  steps <- get_steps(
    fns = fns,
    descriptions = descriptions
  )
  steps$state <- "todo"
  steps$state[[1]] <- "doing"

  function(data) {
    structure(
      list(
        steps = steps,
        data = data
      ),
      class = "adverbial_object_step_by_step",
      ...
    )
  }
}

get_steps <- function(fns, descriptions) {
  descriptions <- descriptions %||% purrr::map_chr(fns, pillar::obj_sum)
  vctrs::data_frame(
    step = names(fns),
    fn = fns,
    description = descriptions
  )
}

#' @export
print.adverbial_object_step_by_step <- function(x, ...) {
  print_steps(x)
  print_step_data(x)
}

print_steps <- function(x) {
  cat_line_subtle("# Steps:")
  print_steps_state(x)
  print_steps_info(x)
  cat_line_subtle("#")
}

print_steps_state <- function(x) {
  symbol_state <- c(
    todo = cli::symbol$checkbox_off,
    doing = cli::symbol$checkbox_off,
    done = cli::symbol$checkbox_on
  )
  steps <- paste0(
    vctrs::vec_seq_along(x$steps),
    ". ",
    x$steps$step
  )
  descriptions <- x$steps$description %||%
    purrr::map_chr(x$steps$fn, pillar::obj_sum)

  cat_line_subtle(
    "# ",
    pillar::align(paste0(
      symbol_state[x$steps$state],
      " ",
      steps,
      ": "
    )),
    pillar::align(descriptions)
  )
}

print_steps_info <- function(x) {
  loc_next_step <- vctrs::vec_match("doing", x$steps$state)
  if (is.na(loc_next_step)) {
    cat_line_subtle(
      "# ",
      cli::symbol$info,
      ' All steps are done. Please call `purrr::chuck("data")` to get the data.'
    )
  } else {
    next_step <- encodeString(x$steps$step[[loc_next_step]], quote = "\"")
    cat_line_subtle(
      "# ",
      cli::symbol$info,
      " Please call `next_step(.step = ",
      next_step,
      ")` to continue."
    )
  }
}

print_step_data <- function(x) {
  cat_line_subtle("# Data:")
  print(x$data)
}
