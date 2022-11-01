

probability_bins <- function(.data, truth, estimate, no_bins = 10) {

  bin_exprs <- map(
    seq_len(no_bins),
    ~ expr(!! truth <= !! .x / !! no_bins ~ !! .x)
  )

  dplyr::mutate(
    .data,
    bins = case_when(!!! bin_exprs)
  )
}
