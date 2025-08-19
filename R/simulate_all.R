simulate_all <- function(n_samples_list, N_list, w_list, f_list, c_list,
                         mu_u_NR, mu_u_R, n_runs = 10) {
  all_sim_data <- list()
  for (n_samples in n_samples_list) {
    for (N in N_list) {
      for (w in w_list) {
        for (f in f_list) {
          for (c in c_list) {
            print("=== Running Simulation with Parameters ===")
            print(paste("Parameters: n_samples =", n_samples, ", N =",
                        N, ", w =", w, ", f =", f, ", c =", c))
            # Store results from multiple runs
            all_roc_mimosa <- list()
            all_roc_fisher <- list()
            for (run in 1:n_runs) {
              # Compute parameters
              params <- calculated_params(mu_u_NR, mu_u_R, f, c)
              # Simulate data
              sim_res <- get_sim_data(n_samples, w, params, N)
              sim_data <- sim_res$data
              # MAP responder_status to long-format data (CHANGE #2)
              n_rows_per <- nrow(sim_data) / length(sim_res$responder_status)
              sim_data$responder_status <- rep(sim_res$responder_status,
                                               each = n_rows_per)
              sim_data$N <- N
              sim_data$n_samples <- n_samples
              sim_data$w <- w
              sim_data$f <- f
              sim_data$c <- c
              sim_data$run <- run   #  keep track of run number
              # Save in list
              all_sim_data[[length(all_sim_data) + 1]] <- sim_data
              # Prepare for MIMOSA and fit
              E <- prepare_for_mimosa(sim_data)
              message("MIMOSA input object E:")
              if (is.null(E)) {
                message("E is NULL – skipping this combination")
                next
              }
              print(exists("E"))
              fit <- fit_MIMOSA(E)
              if (is.null(fit) || is.atomic(fit[[1]]) || !inherits(fit[[1]],
                                                                    "BayesMIMOSA")
              ) {
                message("Invalid fit object – skipping ROC calculation")
                next
              }

              # Calculate ROC results
              roc_result <- ROC_curve(fit, n_samples, list(data = sim_data,
                                        responder_status = sim_res$responder_status))
              roc_fisher <- fishers_curve(sim_data)
              all_roc_mimosa[[run]] <- roc_result
              all_roc_fisher[[run]] <- roc_fisher
            }
            #Average the curves
            avg_roc_mimosa <- average_roc_curves(all_roc_mimosa)
            avg_roc_fisher <- average_roc_curves(all_roc_fisher)
            # Plot averaged curves
            p <- plot_curves(avg_roc_mimosa, avg_roc_fisher, n_samples)
            filename <- sprintf("
                      plots/ROC_avg_ns%d_N%.0f_w%.2f_f%.0f_c%.0f_runs%d.png",
                                n_samples, N, w, f, c, n_runs)
            ggsave(filename, plot = p, width = 8, height = 6, dpi = 300)
          }
        }
      }
    }
  }
  combined_sim_data <- do.call(rbind, all_sim_data)
  write.csv(combined_sim_data, "combined_sim_data.csv", row.names = FALSE)
  return(combined_sim_data)
}
