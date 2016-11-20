fit_surface <- function(df) {
  # do some modelling
  df$fStatRec <- factor(df$StatRec, levels = statrec$StatRec)

  # HACK!!
  df <- subset(df, is.finite(df$weight))

  # substitute zero with half minumum observed catch weight
  df$adj_weight <- df$weight
  if (all(df$weight == 0)) {
    # no observations
    return(NULL)
  } else
    if (any(df$weight == 0)) {
      min_weight <- min(df$weight[df$weight>0], na.rm = TRUE)
      df$adj_weight[df$adj_weight == 0] <- min_weight/2
    }

  # check if there is enough data?
  # or wrap in a try to get it going:
  # set the smoothing to be related to the number of non-zero statsquares observed
  k <- max(3, min(20, floor(length(unique(df$StatRec[df$weight>0])) / 5)))
  out <-
    try(
      gam(log(adj_weight) ~ s(fStatRec, bs = "mrf", xt = list(penalty = Q), k = k),
          data = df,
          drop.unused.levels = FALSE)
    )

  if (inherits(out, "try-error")) {
    NULL
  } else {
    out
  }
}
