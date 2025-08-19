lrt_curve <- function(data, alpha = 0.05) {
  # Expect: data is a list with $data (long df) and $responder_status (0/1)
  df_raw   <- data$data
  resp_vec <- data$responder_status

  # Map responder truth to rows
  subject_idx <- as.integer(sub("Subject_", "", df_raw$SUBJECTID))
  df <- dplyr::mutate(
    df_raw,
    responder  = as.integer(resp_vec[subject_idx]),
    proportion = CYTNUM / (CYTNUM + NSUB)
  )

  # Per-subject LRT (binomial GLM: Stimulated vs Unstimulated)
  subjects <- unique(df$SUBJECTID)
  lrt_results <- data.frame(
    subject  = character(),
    p_value  = numeric(),
    stringsAsFactors = FALSE
  )

  for (subject in subjects) {
    subj_data <- df[df$SUBJECTID == subject, , drop = FALSE]

    # Need exactly one row per condition
    unstim <- subj_data[subj_data$STIMULATION == "Unstimulated", , drop = FALSE]
    stim   <- subj_data[subj_data$STIMULATION == "Stimulated",   , drop = FALSE]
    if (nrow(unstim) != 1 || nrow(stim) != 1) next

    # Binomial GLM with counts: successes = CYTNUM, failures = NSUB
    # Total trials = CYTNUM + NSUB
    full <- stats::glm(
      cbind(CYTNUM, NSUB) ~ STIMULATION,
      family = stats::binomial(),
      data = subj_data
    )
    null <- stats::glm(
      cbind(CYTNUM, NSSUB = NSUB) ~ 1, # NSSUB name is ignored; cbind position matters
      family = stats::binomial(),
      data = subj_data
    )
    # Likelihood ratio test comparing null vs full
    a <- stats::anova(null, full, test = "Chisq")
    p <- as.numeric(a$`Pr(>Chi)`[2])

    lrt_results <- rbind(lrt_results, data.frame(subject = subject, p_value = p))
  }

  # BH FDR and truth
  lrt_results$fdr <- p.adjust(lrt_results$p_value, method = "BH")
  subject_numbers <- as.integer(sub("Subject_", "", lrt_results$subject))
  lrt_results$true_responder <- as.integer(resp_vec[subject_numbers])

  # Build ROC-like curve (sweep FDR thresholds) and return only TPR/FPR
  thresholds <- seq(0, 1, by = 0.01)
  roc_data <- do.call(rbind, lapply(thresholds, function(thresh) {
    pred <- ifelse(lrt_results$fdr < thresh, 1, 0)
    tp <- sum(pred == 1 & lrt_results$true_responder == 1)
    fp <- sum(pred == 1 & lrt_results$true_responder == 0)
    tn <- sum(pred == 0 & lrt_results$true_responder == 0)
    fn <- sum(pred == 0 & lrt_results$true_responder == 1)

    tpr <- ifelse(tp + fn > 0, tp / (tp + fn), 0)
    fpr <- ifelse(fp + tn > 0, fp / (fp + tn), 0)

    data.frame(TPR = tpr, FPR = fpr)
  }))

  return(roc_data)
}