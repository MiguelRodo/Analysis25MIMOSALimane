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

plot_curves <- function(MIMOSA, Fisher, title) {
  ROC <- rbind(
    data.frame(MIMOSA$avg_roc, Method = "MIMOSA"),
    data.frame(Fisher$avg_roc, Method = "Fisher")
  )
  
  # Create AUC text annotations
  auc_data <- data.frame(
    x = c(0.7, 0.7),
    y = c(0.15, 0.05),
    label = c(sprintf("AUC = %.3f", MIMOSA$auc_mean),
              sprintf("AUC = %.3f", Fisher$auc_mean)),
    Method = c("MIMOSA", "Fisher"),
    n_samples = title
  )
  
  roc_p <- ggplot(ROC) +
    geom_line(aes(x = FPR, y = TPR, color = Method), linewidth = 2) +
    geom_text(data = auc_data, aes(x = x, y = y, label = label, color = Method), 
              size = 6, hjust = 0, vjust = 0, show.legend = FALSE) + 
    theme_bw() +
    scale_y_continuous(limits = c(0, 1)) +
    scale_color_discrete(name = "Method") +  # Explicitly set legend title
    facet_wrap(~ paste(title)) +
    theme(
      # Strip (facet labels) styling
      strip.background = element_rect(fill = "grey90", color = "black"),
      strip.text = element_text(size = 20, face = "bold"),
      
      # Axis titles
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold"),
      
      # Axis tick labels
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      
      # Legend styling
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 12),
      legend.key.size = unit(1.2, "cm"),
      
      # Panel spacing
      panel.spacing = unit(0.8, "cm")
    )
  
  return(roc_p)
}
