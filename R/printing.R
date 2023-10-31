cat_class_pred <- function(x) {
  if (length(x) == 0) {
    cat("class_pred(0)", "\n")
  } else {
    print(format(x), quote = FALSE)
  }
}

# Adapted from print.factor
# Smart enough to truncate the levels if they get too long
cat_levels <- function(x, width = getOption("width")) {
  ord <- is_ordered_class_pred(x)

  if (ord) {
    colsep <- " < "
  } else {
    colsep <- " "
  }

  lev <- levels(x)
  n_lev <- length(lev)

  header <- "Levels: "

  maxl <- {
    width <- width - (nchar(header, "w") + 3L + 1L + 3L)
    lenl <- cumsum(nchar(lev, "w") + nchar(colsep, "w"))

    if (n_lev <= 1L || lenl[n_lev] <= width) {
      n_lev
    } else {
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

cat_reportable <- function(x) {
  reportable <- 100 * reportable_rate(x)

  if (rlang::is_scalar_integerish(reportable)) {
    reportable <- vec_cast(reportable, integer())
  }

  digits <- function(x) {
    if (is.integer(x)) {
      0
    } else {
      1
    }
  }

  reportable <- paste0(
    formatC(reportable, format = "f", digits = digits(reportable)),
    "%"
  )

  cat_report <- "Reportable: "
  cat_report <- paste0(cat_report, reportable)
  cat(cat_report)
  cat("\n")
}
