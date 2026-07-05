#' Pareto-optimal (non-dominated) sorting
#'
#' Ranks rows by Pareto dominance across several objectives. Row A dominates
#' row B when A is at least as good as B on every objective and strictly
#' better on at least one. Front 1 is the non-dominated set - the rows that no
#' other row beats on all objectives simultaneously; front 2 is what remains
#' non-dominated after removing front 1, and so on.
#'
#' Useful for trade shortlists: "which targets improve my team most, cost the
#' least, and hold their value best" is a three-objective frontier, and the
#' front-1 players are the ones with no strictly-better alternative.
#'
#' @param objectives a data.frame or matrix, one column per objective. Rows
#'   with any `NA` are placed on a final, worst front.
#' @param maximize logical vector (length = number of columns): `TRUE` to
#'   maximise a column, `FALSE` to minimise it. Defaults to all `TRUE`.
#'
#' @return integer vector of Pareto front numbers (1 = best), one per row.
#'
#' @examples
#' d <- data.frame(gain = c(2, 1, 3, 1), cost = c(1, 1, 3, 2))
#' # want high gain, low cost
#' ffs_pareto_front(d, maximize = c(TRUE, FALSE))
#'
#' @export
ffs_pareto_front <- function(objectives, maximize = NULL) {
  M <- as.matrix(objectives)
  checkmate::assert_matrix(M, mode = "numeric", min.cols = 1, min.rows = 1)
  if (is.null(maximize)) maximize <- rep(TRUE, ncol(M))
  checkmate::assert_logical(maximize, len = ncol(M))

  # orient every objective so larger is better
  M[, !maximize] <- -M[, !maximize]

  n <- nrow(M)
  front <- integer(n)
  ok <- stats::complete.cases(M)

  assigned <- !ok # NA rows are never assigned a real front until the end
  f <- 0L
  while (!all(assigned)) {
    f <- f + 1L
    idx <- which(!assigned)
    sub <- M[idx, , drop = FALSE]
    dominated <- vapply(seq_along(idx), function(i) {
      x <- sub[i, ]
      ge_all <- rowSums(sweep(sub, 2, x, ">=")) == ncol(sub)
      gt_any <- rowSums(sweep(sub, 2, x, ">")) > 0
      ge_all[i] <- FALSE # never dominated by self
      any(ge_all & gt_any)
    }, logical(1))
    front[idx[!dominated]] <- f
    assigned[idx[!dominated]] <- TRUE
  }
  if (any(!ok)) front[!ok] <- f + 1L # incomparable rows to the back
  front
}
