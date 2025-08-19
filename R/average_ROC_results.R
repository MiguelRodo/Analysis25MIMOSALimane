# Function to plot ROC curves
# It combines the results from MIMOSA and Fisher's test and plots them
plot_curves <- function(roc_result, roc_fisher, n_samples) {
  ROC <- rbind(
    data.frame(roc_result, Method = "MIMOSA"),
    data.frame(roc_fisher, Method = "Fisher")
  )
  roc_p <- ggplot(ROC) +
    geom_line(aes(x = FPR, y = TPR, color = Method), lwd = 1.5) +
    theme_bw()
  return(roc_p)
}

# Function to average multiple ROC curves
# It combines the results from multiple runs and averages the TPR and FPR

average_roc_curves <- function(roc_list, fpr_grid = seq(0, 1, by = 0.001)) {
  # Interpolate each curve's TPR onto the common FPR grid
  tpr_mat <- sapply(roc_list, function(df) {
    stopifnot(all(c("FPR", "TPR") %in% names(df)))
    df2 <- df[is.finite(df$FPR) & is.finite(df$TPR), c("FPR", "TPR")]
    df2 <- df2[order(df2$FPR), , drop = FALSE]
    df2 <- df2[!duplicated(df2$FPR), , drop = FALSE]
    approx(x = df2$FPR, y = df2$TPR, xout = fpr_grid, rule = 2)$y
  })

  if (is.null(dim(tpr_mat))) tpr_mat <- matrix(tpr_mat, ncol = 1)

  data.frame(
    FPR    = fpr_grid,
    TPR    = rowMeans(tpr_mat, na.rm = TRUE),
    TPR_sd = apply(tpr_mat, 1, stats::sd, na.rm = TRUE),
    n      = ncol(tpr_mat)
  )
}