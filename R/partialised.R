#' Create partialised functions
#'
#' @param f A function.
#' @param args A list of default arguments.
#' @param ... Additional arguments for attributes.
#' @param class Name of subclass.
#'
#' @return A `adverbial_function_partial` function.
#'
#' @seealso [purrr::partial()]
#'
#' @examples
#' dist <- function(x, y) {
#'   sqrt(x ^ 2 + y ^ 2)
#' }
#' pdist <- new_partialised(dist, list(x = 3))
#' pdist(y = 4)
#'
#' @export
new_partialised <- function(f, args = list(), ..., class = character()) {
  vctrs::vec_assert(args, list())

  attrs <- rlang::list2(...)
  attrs <- attrs[!names(attrs) %in% c("body", "fn")]

  data <- purrr::partial(rlang::as_function(f), !!!args)
  rlang::exec(
    structure,
    data,
    fn = f,
    !!!attrs,
    class = c(class, "adverbial_function_partial", class(data))
  )
}

arguments <- function(x) {
  out <- rlang::call_args(attr(x, "body"))
  out[-vctrs::vec_size(out)]
}

`arguments<-` <- function(x, value) {
  attrs <- attributes(x)
  attrs <- attrs[!names(attrs) %in% c("body", "fn")]

  f <- attr(x, "fn")
  data <- purrr::partial(f, !!!value)
  rlang::exec(structure, data, fn = f, !!!attrs, class = class(x))
}

#' @export
`[.adverbial_function_partial` <- function(x, i, ...) {
  arguments(x)[i, ...]
}

#' @export
`[<-.adverbial_function_partial` <- function(x, i, value) {
  arguments(x)[i] <- value
  x
}

#' @export
`[[.adverbial_function_partial` <- function(x, i, ...) {
  arguments(x)[[i, ...]]
}

#' @export
`[[<-.adverbial_function_partial` <- function(x, i, value) {
  arguments(x)[[i]] <- value
  x
}

#' @export
`$.adverbial_function_partial` <- function(x, i) {
  x[[i]]
}

#' @export
`$<-.adverbial_function_partial` <- function(x, i, value) {
  x[[i]] <- value
  x
}

#' @export
names.adverbial_function_partial <- function(x) {
  names(arguments(x))
}

#' @export
print.adverbial_function_partial <- function(x, ...) {
  cli::cat_line(paste0("<", pillar::obj_sum(x), ">"))

  print_fn(x)

  cli::cat_line("(")
  print_args(arguments(x))
  cli::cat_line(strrep(" ", 2L), "...")
  cli::cat_line(")")

  invisible(x)
}

print_fn <- function(x) {
  print(attr(x, "fn"))
}

print_args <- function(x) {
  if (!vctrs::vec_is_empty(x)) {
    nms <- pillar::align(rlang::names2(x))
    nms <- paste0(strrep(" ", 2L), nms, " = ")

    opts <- options()
    on.exit(options(opts))

    width <- max(pillar::get_extent(nms))
    options(width = pmax(0, opts$width - width))

    spaces <- strrep(" ", width)
    out <- purrr::map2(unname(x), nms, function(x, nm) {
      if (rlang::is_scalar_atomic(x) && !rlang::is_named(x)) {
        out <- as.character(x)
      } else {
        out <- utils::capture.output(x)
      }

      names(out)[[1L]] <- nm
      names(out)[-1L] <- spaces

      out
    })
    out <- vctrs::vec_c(!!!out)
    cli::cat_line(names(out), out)
  }
}

#' @export
type_sum.adverbial_function_partial <- function(x) {
  "partialised"
}

#' @export
obj_sum.adverbial_function_partial <- function(x) {
  paste0(pillar::type_sum(x), "(", big_mark(vctrs::vec_size(arguments(x))), ")")
}
