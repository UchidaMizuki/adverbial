step_by_step <- function(fns, steps = names(fns), descriptions = NULL, ...) {
  steps <- get_steps(
    fns = fns,
    steps = steps,
    descriptions = descriptions
  )
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

get_steps <- function(fns, steps, descriptions) {
  fns <- vctrs::vec_cast(fns, list())
  steps <- vctrs::vec_cast(steps, character())
  descriptions <- descriptions %||% purrr::map_chr(fns, pillar::obj_sum)
  vctrs::data_frame(
    step = steps,
    fn = fns,
    description = descriptions,
    state = "todo"
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
    x$steps$step,
    " "
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
    cat_line_subtle("# ", cli::symbol$info, " All steps are done.")
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
