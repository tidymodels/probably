# is there a forcats for this?
recode_data <- function(obs, prob, threshold) {
  lvl <- levels(obs)
  if (getOption("yardstick.event_first")) {
    pred <- ifelse(prob >= threshold, lvl[1], lvl[2])
  } else {
    pred <- ifelse(prob >= threshold, lvl[2], lvl[1])
  }
  factor(pred, levels = lvl)
}

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

  ord <- is_ordered_class_pred(x)

  if(ord) {
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

# Check if a class_pred object came from an ordered factor
is_ordered_class_pred <- function(x) {
  attr(x, "ordered")
}
