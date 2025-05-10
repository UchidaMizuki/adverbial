new_step_by_step <- function(
  object,
  steps,
  current_step,
  ...,
  class = character()
) {
  structure(
    object,
    adverbial_object_step_by_step = list(
      steps = steps,
      current_step = current_step
    ),
    ...,
    class = c(
      class,
      "adverbial_object_step_by_step",
      setdiff(class(object), "adverbial_object_step_by_step")
    )
  )
}

#' Create a step-by-step object
#'
#' `r lifecycle::badge("experimental")`
#'
#' `step_by_step()` creates a step-by-step object that can be used to track the
#' progress of a process.
#' It is useful for long-running processes where you want to keep track of
#' the steps that have been completed and the steps that are still to be done.
#'
#' @param steps A named vector of steps to be completed. The names of the
#' vector are the names of the steps, and the values are the descriptions of
#' the steps.
#'
#' @return A function that takes an object and returns a step-by-step object.
#'
#' @export
step_by_step <- function(steps) {
  steps <- vctrs::vec_cast(steps, character())
  if (!rlang::is_named(steps)) {
    cli::cli_abort("{.arg steps} must be a named vector.")
  }

  function(object, ...) {
    new_step_by_step(
      object,
      steps = steps,
      current_step = 0,
      ...
    )
  }
}

#' Wrap a function to be used as a step
#'
#' `r lifecycle::badge("experimental")`
#'
#' `as_step()` wraps a function to be used as a step in a step-by-step
#' process.
#'
#' @param f A function to be wrapped.
#' @param name The name of the step. If `NULL`, the step does not proceed but
#' the function is applied.
#'
#' @return A function that takes a step-by-step object and additional
#' arguments, and returns the updated step-by-step object.
#'
#' @export
as_step <- function(f, name = NULL) {
  name <- vctrs::vec_cast(name, character())

  structure(
    function(object, ...) {
      wrap_step(f, name)(object, ...)
    },
    adverbial_function_step_by_step = list(
      f = f,
      name = name
    ),
    class = "adverbial_function_step_by_step"
  )
}

#' End a step-by-step process
#'
#' `r lifecycle::badge("experimental")`
#'
#' `end_step()` ends the step-by-step process and removes the step-by-step
#' attributes from the object.
#'
#' @param object The object to end the step-by-step process for.
#'
#' @return The object with the step-by-step attributes removed.
#'
#' @export
end_step <- function(object) {
  attr(object, "adverbial_object_step_by_step") <- NULL
  class(object) <- setdiff(class(object), "adverbial_object_step_by_step")
  object
}

wrap_step <- function(f, name) {
  function(object, ...) {
    if (inherits(object, "adverbial_object_step_by_step")) {
      attr_object <- attr(object, "adverbial_object_step_by_step")
      steps <- attr_object$steps
      current_step <- attr_object$current_step

      if (is.null(name)) {
        new_step_by_step(
          f(object, ...),
          steps = steps,
          current_step = current_step
        )
      } else {
        if (current_step == vctrs::vec_size(steps)) {
          cli::cli_abort("No more steps to do.")
        }

        next_step_name <- names(steps)[[current_step + 1]]
        if (name != next_step_name) {
          cli::cli_abort("Next step must be {.fn {next_step_name}}.")
        }

        new_step_by_step(
          f(object, ...),
          steps = steps,
          current_step = current_step + 1
        )
      }
    } else {
      f(object, ...)
    }
  }
}

#' @export
print.adverbial_object_step_by_step <- function(x, ...) {
  print_steps(x)
  print(end_step(x))
}

print_steps <- function(x) {
  cat_line_subtle("# Steps:")
  print_steps_state(x)
  print_steps_info(x)
  cat_line_subtle("#")
}

print_steps_state <- function(object) {
  attr_object <- attr(object, "adverbial_object_step_by_step")
  steps <- attr_object$steps
  current_step <- attr_object$current_step

  size_steps <- vctrs::vec_size(steps)
  seq_along_steps <- vctrs::vec_seq_along(steps)

  symbol_state <- vctrs::vec_rep(cli::symbol$checkbox_off, size_steps)
  symbol_state[1:current_step] <- cli::symbol$checkbox_on

  descriptions <- unname(steps)
  steps <- paste0(symbol_state, " ", seq_along_steps, ". ", names(steps), ": ")
  cat_line_subtle(
    "# ",
    pillar::align(steps),
    descriptions
  )
}

print_steps_info <- function(object) {
  attr_object <- attr(object, "adverbial_object_step_by_step")
  steps <- attr_object$steps
  current_step <- attr_object$current_step

  if (current_step == vctrs::vec_size(steps)) {
    cat_line_subtle(
      "# ",
      cli::symbol$info,
      " All steps are done. Please call `end_step()`."
    )
  } else {
    next_step <- names(steps)[[current_step + 1]]
    cat_line_subtle(
      "# ",
      cli::symbol$info,
      " Please call `",
      next_step,
      "()` to continue."
    )
  }
}

#' @export
print.adverbial_function_step_by_step <- function(x, ...) {
  cli::cat_line(paste0("<", pillar::obj_sum(x), ">"))

  print(attr(x, "adverbial_function_step_by_step")$f)
  invisible(x)
}

#' @export
type_sum.adverbial_function_step_by_step <- function(x) {
  "step"
}

#' @export
obj_sum.adverbial_function_step_by_step <- function(x) {
  name <- attr(x, "adverbial_function_step_by_step")$name

  if (is.null(name)) {
    pillar::type_sum(x)
  } else {
    paste0(pillar::type_sum(x), ": ", name)
  }
}
