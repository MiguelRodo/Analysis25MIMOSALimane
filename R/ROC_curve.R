ROC_curve <- function(fit, n_samples, data) {
 # Extract MIMOSA results
  mimosa_obj <- fit$IL2
  mimosa_result <- mimosa_obj@result
  prob_mat <- mimosa_result@z
  ind_mat <- mimosa_result@phenoData@data
  
  # Create results table with predictions and truth
  results_tbl <- ind_mat |>
    dplyr::bind_cols(tibble::as_tibble(prob_mat)) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      pid_ind = as.numeric(gsub("Subject_", "", SUBJECTID))
    ) |>
    dplyr::arrange(pid_ind) |>
    dplyr::mutate(
      resp_status = as.numeric(data$responder_status),
      mimosa_prob = V2  # MIMOSA probability of being a responder
    )
  
  # Calculate AUC using pROC
  roc_obj <- pROC::roc(results_tbl$resp_status, results_tbl$mimosa_prob, quiet = TRUE)
  auc_value <- as.numeric(pROC::auc(roc_obj))
  
  # Extract ROC curve coordinates for ggplot
  roc_coords <- pROC::coords(roc_obj, "all", ret = c("threshold", "specificity", "sensitivity"))
  
  # Create ROC data frame for ggplot
  roc_data <- data.frame(
    FPR = 1 - roc_coords$specificity,  # FPR = 1 - Specificity
    TPR = roc_coords$sensitivity,      # TPR = Sensitivity
    Threshold = roc_coords$threshold
  )

  
  return(roc_data)
}
