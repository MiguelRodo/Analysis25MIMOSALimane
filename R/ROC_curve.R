ROC_curve <- function(fit, data) {
  mimosa_obj <- fit$IL2
<<<<<<< HEAD
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
=======
  res <- mimosa_obj@result
  prob_mat <- res@z
  pheno <- res@phenoData@data

  df <- dplyr::bind_cols(pheno, tibble::as_tibble(prob_mat)) |>
    dplyr::mutate(pid_ind = as.numeric(sub("Subject_", "", SUBJECTID))) |>
    dplyr::arrange(pid_ind)

  # Ensure responder_status aligned by pid_ind
  resp_df <- tibble::tibble(
    pid_ind = seq_along(data$responder_status),
    resp_status = as.numeric(data$responder_status)
  )
  df <- dplyr::left_join(df, resp_df, by = "pid_ind") |>
    dplyr::mutate(mimosa_prob = V2)

  roc_obj <- pROC::roc(df$resp_status, df$mimosa_prob, quiet = TRUE)
  auc_value <- as.numeric(pROC::auc(roc_obj))
  coords <- pROC::coords(roc_obj, "all", ret = c("specificity","sensitivity"))

  roc_data <- data.frame(FPR = 1 - coords$specificity,
                         TPR = coords$sensitivity)
  list(roc_data = roc_data, auc = auc_value)
>>>>>>> refs/remotes/origin/main
}
