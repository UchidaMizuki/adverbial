next_step <- function(.object, .step, ...) {
  step <- vctrs::vec_cast(.step, character())
  vctrs::vec_check_size(step, size = 1)

  loc_next_step <- vctrs::vec_match("doing", .object$steps$state)
  next_step <- .object$steps$step[[loc_next_step]]
  if (is.na(next_step)) {
    cli::cli_abort(c(
      "All steps are done.",
      "i" = "If you want to add a step, first use {.code insert_step(.step = {.val {step}})}."
    ))
  } else if (step != next_step) {
    cli::cli_abort(c(
      "{.arg .step} is not equal to {.val {next_step}}.",
      "i" = "You can use {.code next_step(.step = {.val {next_step}})}.",
      "i" = "If you want to change steps, first use {.code insert_step(.step = {.val {step}})} or {.code delete_step(.step = {.val {next_step}})}."
    ))
  }

  .object$data <- .object$steps$fn[[loc_next_step]](.object$data, ...)

  .object$steps$state[[loc_next_step]] <- "done"
  if (loc_next_step < vctrs::vec_size(.object$steps)) {
    .object$steps$state[[loc_next_step + 1]] <- "doing"
  }

  .object
}

insert_step <- function(
  object,
  fns,
  steps = names(fns),
  descriptions = NULL,
  before = NULL,
  after = NULL
) {
  check_step_no_done(object)

  if (!xor(is.null(before), is.null(after))) {
    cli::cli_abort(
      "You must specify either {.arg before} or {.arg after}, but not both."
    )
  } else {
    loc <- vctrs::vec_as_location2(
      before %||% after,
      vctrs::vec_size(object$steps),
      names = object$steps$step
    )
    if (!is.null(before)) {
      loc <- loc - 1
    }
  }
  sizes <- c(loc, vctrs::vec_size(object$steps) - loc)
  object_steps <- vctrs::vec_chop(object$steps, sizes = sizes)

  fns_steps <- get_steps(
    fns = fns,
    steps = steps,
    descriptions = descriptions
  )

  steps <- vctrs::vec_rbind(
    object_steps[[1]],
    fns_steps,
    object_steps[[2]]
  )
  steps$state[[1]] <- "doing"

  object$steps <- steps
  object
}

update_step <- function(object, step, f = NULL, description = NULL) {
  check_step_no_done(object)

  loc <- vctrs::vec_as_location2(
    step,
    vctrs::vec_size(object$steps),
    names = object$steps$step
  )

  object$steps$fn[[loc]] <- f %||% object$steps$fn[[loc]]
  object$steps$description[[loc]] <- description %||%
    object$steps$description[[loc]]
  object
}

delete_step <- function(object, step) {
  check_step_no_done(object)

  loc <- vctrs::vec_as_location(
    step,
    vctrs::vec_size(object$steps),
    names = object$steps$step
  )
  object$steps <- vctrs::vec_slice(object$steps, -loc)
  object
}

check_step_no_done <- function(object) {
  if (any(object$steps$state == "done")) {
    cli::cli_abort("It must not contain any completed steps.")
  }
}
