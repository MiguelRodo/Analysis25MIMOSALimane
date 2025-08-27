fishers_curve<- function(data, alpha = 0.05) {
  df_raw   <- data$data
  resp_vec <- data$responder_status
  subjects <- unique(df_raw$SUBJECTID)
  
  results_list <- lapply(subjects, function(subj) {
    sub_data <- df_raw[df_raw$SUBJECTID == subj, , drop = FALSE]
    
    unstim <- sub_data[sub_data$STIMULATION == "Unstimulated", , drop = FALSE]
    stim   <- sub_data[sub_data$STIMULATION == "Stimulated",   , drop = FALSE]
    
    cont_table <- matrix(
      c(unstim$CYTNUM, unstim$NSUB,
        stim$CYTNUM,   stim$NSUB),
      nrow = 2, byrow = TRUE
    )
    
    ft <- stats::fisher.test(cont_table, alternative = "less")
    data.frame(SUBJECTID = as.character(subj), p_value = as.numeric(ft$p.value))
  })
  
  results_df <- do.call(rbind, results_list)
  results_df$fdr <- stats::p.adjust(results_df$p_value, method = "BH")
  
  subj_idx <- as.integer(gsub("Subject_", "", results_df$SUBJECTID))
  results_df$true_responder <- as.integer(resp_vec[subj_idx])
  results_df$score <- 1 - results_df$p_value
  
  roc_obj <- pROC::roc(
    response  = results_df$true_responder,
    predictor = results_df$score,
    levels    = c(0, 1),
    direction = "<"
  )
  
  roc_data <- data.frame(
    FPR = 1 - roc_obj$specificities,
    TPR = roc_obj$sensitivities
  )
  
  auc_value <- as.numeric(pROC::auc(roc_obj))
  
  return(list(
    roc_data = roc_data,   
    auc = auc_value
  ))
}
