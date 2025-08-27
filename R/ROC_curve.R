ROC_curve <- function(fit, n_samples, data) {
 mimosa_obj <- fit$IL2
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
}
