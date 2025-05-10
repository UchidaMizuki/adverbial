#' Create composed functions
#'
#' @param fns A list of functions to compose.
#' @param dir Direction of composition, either `"forward"` or `"backward"`.
#' By default, the functions are composed in the forward direction.
#' Passed to [purrr::compose()].
#' @param ... Additional arguments for attributes.
#' @param class Name of subclass.
#'
#' @return A composed function that inherits from `adverbial_function_compose`.
#'
#' @seealso [purrr::compose()]
#'
#' @examples
#' square <- function(x) x ^ 2
#' cdist <- new_composed(list(square = square, sum = sum, sqrt = sqrt))
#' cdist(1:10)
#'
#' cdist$sum <- new_partialised(sum, list(na.rm = TRUE))
#' cdist(c(1:10, NA))
#'
#' @export
new_composed <- function(fns, dir = NULL, ..., class = character()) {
  vctrs::obj_check_list(fns)

  if (is.null(dir)) {
    dir <- "forward"
    cli::cli_alert_info(
      "No direction specified, using {.code dir = {.val {dir}}}."
    )
  }

  attrs <- rlang::list2(...)
  attrs <- attrs[!names(attrs) %in% c("first_fn", "fns", "fn_names")]

  data <- purrr::compose(!!!fns, .dir = dir)
  rlang::exec(
    structure,
    data,
    fn_names = rlang::names2(fns),
    !!!attrs,
    class = c(class, "adverbial_function_compose", class(data))
  )
}

functions <- function(x) {
  first_fn <- attr(x, "first_fn")
  fns <- attr(x, "fns")
  fn_names <- attr(x, "fn_names")

  fns <- rlang::list2(first_fn, !!!fns)
  rlang::set_names(fns, fn_names)
}

`functions<-` <- function(x, value) {
  attrs <- attributes(x)
  attrs <- attrs[!names(attrs) %in% c("first_fn", "fns", "fn_names")]

  data <- purrr::compose(!!!value, .dir = "forward")
  rlang::exec(
    structure,
    data,
    fn_names = rlang::names2(value),
    !!!attrs,
    class = class(x)
  )
}

#' @export
`[.adverbial_function_compose` <- function(x, i, ...) {
  functions(x)[i, ...]
}

#' @export
`[<-.adverbial_function_compose` <- function(x, i, value) {
  functions(x)[i] <- value
  x
}

#' @export
`[[.adverbial_function_compose` <- function(x, i, ...) {
  functions(x)[[i, ...]]
}

#' @export
`[[<-.adverbial_function_compose` <- function(x, i, value) {
  functions(x)[[i]] <- value
  x
}

#' @export
`$.adverbial_function_compose` <- function(x, i) {
  x[[i]]
}

#' @export
`$<-.adverbial_function_compose` <- function(x, i, value) {
  x[[i]] <- value
  x
}

#' @export
names.adverbial_function_compose <- function(x) {
  names(functions(x))
}

#' @export
print.adverbial_function_compose <- function(x, ...) {
  cli::cat_line(paste0("<", pillar::obj_sum(x), ">"))

  fns <- functions(x)
  for (i in seq_along(fns)) {
    cli::cat_line(paste0(i, ". ", names(fns)[[i]]))
    print(fns[[i]], ...)
    cli::cat_line()
  }
  invisible(x)
}

#' @export
type_sum.adverbial_function_compose <- function(x) {
  "composed"
}

#' @export
obj_sum.adverbial_function_compose <- function(x) {
  paste0(pillar::type_sum(x), "(", big_mark(vctrs::vec_size(functions(x))), ")")
}
