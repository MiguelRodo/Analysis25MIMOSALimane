lrt_curve <- function(data) {
df <- data$data
  resp_vec <- data$responder_status
  # Add responder info
  subj_idx <- as.integer(gsub("Subject_", "", df$SUBJECTID))
  df$responder <- resp_vec[subj_idx]
  subjects <- unique(df$SUBJECTID)
  results <- data.frame(subject = character(), p_value = numeric(), stringsAsFactors = FALSE)
  for (subj in subjects) {
    sd <- df[df$SUBJECTID == subj, ]
    # Skip if not exactly one stim + one unstim
    if (nrow(sd[sd$STIMULATION == "Unstimulated", ]) != 1 ||
        nrow(sd[sd$STIMULATION == "Stimulated", ]) != 1) next
    full <- glm(cbind(CYTNUM, NSUB) ~ STIMULATION, family = binomial(), data = sd)
    null <- glm(cbind(CYTNUM, NSUB) ~ 1, family = binomial(), data = sd)
    p <- anova(null, full, test = "Chisq")$`Pr(>Chi)`[2]
    results <- rbind(results, data.frame(subject = subj, p_value = p))
  }
  results$fdr <- p.adjust(results$p_value, method = "BH")
  subj_num <- as.integer(gsub("Subject_", "", results$subject))
  results$true_responder <- resp_vec[subj_num]
  # ROC data from FDR
  thresholds <- seq(0, 1, 0.01)
  roc_data <- do.call(rbind, lapply(thresholds, function(t) {
    pred <- as.integer(results$fdr < t)
    tp <- sum(pred == 1 & results$true_responder == 1)
    fp <- sum(pred == 1 & results$true_responder == 0)
    tn <- sum(pred == 0 & results$true_responder == 0)
    fn <- sum(pred == 0 & results$true_responder == 1)
    data.frame(
      threshold = t,
      TPR = ifelse(tp + fn > 0, tp / (tp + fn), 0),
      FPR = ifelse(fp + tn > 0, fp / (fp + tn), 0)
    )
  }))
  auc_val <- if (length(unique(results$true_responder)) == 2) {
  as.numeric(pROC::auc(pROC::roc(results$true_responder, -results$p_value)))
} else NA
  return(list(roc_data = roc_data, auc = auc_val))
  }
