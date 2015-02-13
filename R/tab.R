
tab_options <- list(
  max_width = Inf,
  min_width = 0,
  column_splitter = " ",
  truncate = FALSE,
  truncate_marker = "...",
  preserve_newlines = FALSE,
  padding_chr = " ",
  show_headers = TRUE,
  heading_transform = "toupper",
  data_transform = "format",
  table_width = "auto",
  align = "left"
)

#' @importFrom magrittr %>%
NULL

#' Pretty tables in the R terminal
#'
#' @section Features:
#' \itemize{
#'   \item ANSI colored text via the crayon package.
#'   \item Cell wrapping.
#'   \item Minimum and maximum cell width.
#'   \item Custom column spliter string.
#'   \item Custom padding string.
#'   \item Preservation of newlines in cells.
#'   \item Headers.
#'   \item Transformation functions for headers and data. By
#'      defaults headers are in all caps, and data is formatted
#'      with \code{base::format}.
#'   \item Maximum table width (truncated here), defaults to
#'      terminal width.
#'   \item Align left, right or center.
#' }
#'
#' It prints the table to the console via \code{base::cat}, and
#' also returns the lines of the table invisibly.
#'
#' @param x The object to print. It will be coerced to a data frame.
#' @param ... Extra configuration obtions, see them below.
#' @return The lines of the formatted table, invisibly.
#'
#' @export
#' @examples
#' x <- data.frame(
#'   stringsAsFactors = FALSE,
#'   pkg = c("igraph", "crayon", "tab"),
#'   version = c("0.7.1", "1.1.0", "1.0.0"),
#'   description = c(
#'     paste("Routines for simple graphs and network analysis.",
#'       "igraph can handle large graphs very well and provides",
#'       "functions for generating random and regular graphs,",
#'       "graph visualization, centrality indices and much more."),
#'     paste("Crayon adds support for colored terminal output on",
#'       "terminals that support ANSI color and highlight codes.",
#'       "ANSI color support is automatically detected.",
#'       "Colors and highlighting can be combined and nested. New styles",
#'       "can also be created easily. This package was inspired by",
#'       "the chalk JavaScript project"),
#'     paste("Pretty Tables in the terminal. Pretty print a data frame",
#'        "in the R terminal. Supports word wrapping,\ncell truncation,",
#'        "ANSI colors, column names, alignments, padding")
#'   )
#' )
#' tab(x, max_width = 40)
#' tab(x, min_width = 10)
#' tab(x, max_width = 50, column_splitter = " | ")
#'
#' x$pkg <- crayon::red(x$pkg)
#' tab(x, max_width = 50)
#' tab(x, align = "right", max_width = 50, column_splitter = " | ")
#' tab(x, max_width = 50, padding_chr = ".")

tab <- function(x, ...) {

  x <- as.data.frame(x, stringsAsFactors = FALSE) %>%
    as.list()

  opts <- list(...)
  if (length(opts) && (
    is.null(names(opts)) || any(names(opts) == ""))) {
    stop("Unnamed arguments are not allowed")
  }

  unknown_opts <- setdiff(names(opts), names(tab_options))
  if (length(unknown_opts)) {
    stop("Unknown option", if (length(unknown_opts) - 1) "s" else "",
         paste(unknown_opts, collapse = ", "))
  }

  opts <- modifyList(tab_options, opts)

  if (opts$table_width == "auto") opts$table_width <- getOption("width")

  ## transform data cells
  if (is.character(opts$data_transform)) {
    opts$data_transform <- getFunction(opts$data_transform)
  }
  x <- lapply(x, opts$data_transform)

  ## sanitize data, remove newlines if requested
  x <- if (opts$preserve_newlines) {
    lapply(x, gsub, pattern = "\\h+", replacement = " ", perl = TRUE)
  } else {
    lapply(x, gsub, pattern = "\\s+", replacement = " ")
  }

  ## Add headers
  if (opts$show_headers) {
    if (is.character(opts$heading_transform)) {
      opts$heading_transform <- getFunction(opts$heading_transform)
    }
    headers <- names(x) %>%
      opts$heading_transform()
    x <- mapply(headers, x, FUN = c, SIMPLIFY = FALSE)
  }

  ## get actual max-width between min & max
  ## based on length of data in columns
  width <- vapply(x, FUN.VALUE = 1, function(cur) {
    col_nchar(cur) %>%
      max(opts$min_width) %>%
      min(opts$max_width)
  })

  ## split long words so they can break onto multiple lines
  x <- mapply(x, width, opts$truncate_marker, FUN = split_long_words,
              SIMPLIFY = FALSE)

  ## wrap long lines. each item is now an array of lines.
  x <- mapply(x, width, FUN = split_into_lines, SIMPLIFY = FALSE)

  ## if truncating required, only include first line + add truncation char
  ## TODO

  ## recalculate column widths from truncated output/lines
  width <- vapply(x, FUN.VALUE = 1, function(cur) {
    col_nchar(cur) %>%
      max(opts$min_width) %>%
      min(opts$max_width)
  })

  ## merge lines into rows
  rows <- create_rows(x, width, opts)

  cat(rows, sep = "\n")

  invisible(rows)
}

create_rows <- function(x, width, opts) {

  num_lines <- vapply(x, function(xx) vapply(xx, length, 1L),
      integer(length(x[[1]]))) %>%
    apply(1, max)

  ## combine matching lines of each rows
  lapply(seq_along(num_lines), FUN = function(i) {
    max_lines <- num_lines[i]
    lapply(seq_len(length(x)), FUN = function(c) {
      c(x[[c]][[i]], rep("", max_lines - length(x[[c]][[i]]))) %>%
        pad(width[c], opts$padding_chr, opts$align)
    }) %>%
      do_call(f = paste, .args = list(sep = opts$column_splitter))
  }) %>%
    unlist() %>%
    truncate_string(opts$table_width)
}
