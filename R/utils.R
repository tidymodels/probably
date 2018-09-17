quote_collapse <- function(x, quote = "`", collapse = ", ") {
  paste(encodeString(x, quote = quote), collapse = collapse)
}

#' @importFrom rlang abort
abort_default <- function(x, fn) {
  cls <- quote_collapse(class(x))
  msg <- paste0("No implementation of `", fn, "()` for object of class ", cls, ".")
  abort(msg)
}

# Adapted from print.factor
# Smart enough to truncate the levels if they get too long
cat_levels <- function(x, width = getOption("width")) {

  lev <- levels(x)
  n_lev <- length(lev)
  colsep <- " "

  header <- "Levels: "

  maxl <- {
    width <- width - (nchar(header, "w") + 3L + 1L + 3L)
    lenl <- cumsum(nchar(lev, "w") + nchar(colsep, "w"))

    if (n_lev <= 1L || lenl[n_lev] <= width) {
      n_lev
    }
    else {
      max(1L, which.max(lenl > width) - 1L)
    }
  }

  # do we need to drop levels?
  drop <- n_lev > maxl

  cat(

    # Print number of levels if we had to drop some
    if (drop) {
      paste(format(n_lev), "")
    },

    # Print `Levels: `
    header,

    paste(

      # `first levels ... last levels`
      if (drop) {
        c(lev[1L:max(1, maxl - 1)], "...", if (maxl > 1) lev[n_lev])
      }

      # print all levels
      else {
        lev
      },

      collapse = colsep
    ),

    # Newline
    "\n",

    sep = ""
  )
}

cat_eq_count <- function(x) {
  eq_count <- sum(is_equivocal(x))
  cat_eq <- "EQ Count: "
  cat_eq <- paste0(cat_eq, eq_count)
  cat(cat_eq)
  cat("\n")
}
