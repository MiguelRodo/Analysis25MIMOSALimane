# Function to average multiple ROC curves
# It combines the results from multiple runs and averages the TPR and FPR

average_roc_curves <- function(roc_list, fpr_grid = seq(0, 1, 0.001)) {
  tpr_mat <- sapply(roc_list, function(x) {
    df <- x$roc_data
    df <- df[order(df$FPR), ]
    df <- df[!duplicated(df$FPR), ]
    approx(df$FPR, df$TPR, xout = fpr_grid, rule = 2)$y
  })
  if (is.null(dim(tpr_mat))) tpr_mat <- matrix(tpr_mat, ncol = 1)
  avg_roc <- data.frame(
    FPR = fpr_grid,
    TPR = rowMeans(tpr_mat),
    TPR_sd = if (ncol(tpr_mat) > 1) apply(tpr_mat, 1, sd) else NA_real_
  )
  list(
    avg_roc = avg_roc,
    auc_mean = mean(vapply(roc_list, function(x) x$auc, numeric(1)))
  )
}

# Minimal plotting: mimic simple example but adapt to new list formats
plot_curves <- function(MIMOSA, Fisher, n_samples) {
  ROC <- rbind(
    data.frame(MIMOSA$avg_roc, Method = sprintf("MIMOSA (AUC=%.3f)", MIMOSA$auc_mean)),
    data.frame(Fisher$avg_roc, Method = sprintf("Fisher (AUC=%.3f)", Fisher$auc_mean))
  )
  roc_p <- ggplot(ROC) +
    geom_line(aes(x = FPR, y = TPR, color = Method), linewidth = 1.2) +
    theme_bw()+
    scale_y_continuous(limits = c(0, 1))
  roc_p
}
