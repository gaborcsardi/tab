
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

#' Pretty tables in the R terminal
#'
#' Pretty print a data frame.
#'
#' @docType package
#' @name tab
#' @importFrom magrittr %>%
NULL

#' @export

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
