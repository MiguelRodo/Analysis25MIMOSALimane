source("R/Roc_fisher_clean.R")
simulate_all <- function(n_samples_list, N_list, w_list, f_list, c_list,
                         mu_u_NR, mu_u_R) {
  for (n_samples in n_samples_list) {
    for (N in N_list) {
      for (w in w_list) {
        for (f in f_list) {
          for (c in c_list) {
            # Log the current simulation parameters
            print("=== Running Simulation with Parameters ===")
            print(list(n_samples = n_samples, N = N, w = w, f = f, c = c))
            # Compute parameters
            params <- calculated_params(mu_u_NR, mu_u_R, f, c)
            print("Computed parameters:")
            print(params)
            # Simulate data
            sim_data <- get_sim_data(n_samples, w, params, N)
            print("First few rows of simulated data:")
            print(head(sim_data))
            # Prepare for MIMOSA and fit
            E <- prepare_for_mimosa(sim_data)
            print("MIMOSA input object E:")
            print(E)
            if (is.null(E)) {
              print("E is NULL – skipping this combination")
              next
            }

            fit <- fit_MIMOSA(E)
            print("Fit object (first item):")
            print(fit[[1]])
            roc_result <- ROC_results(fit, n_samples)
            roc_fisher <- ROC_fisher(fit, n_samples)
            ROC <- rbind(
              data.frame(roc_result, Method = "MIMOSA"),
              data.frame(roc_fisher, Method = "Fisher")
            )
            library(ggplot2)
            roc_p <- ggplot(ROC) +
              geom_line(aes(x = FPR, y = TPR, color = Method), lwd = 1.5) +
              facet_wrap(~Ntot) +
              theme_bw()
            roc_plot <- paste0("ROC_n", n_samples, "_N", N, "_w", w, 
                               "_f", f, "_c", c, ".png")
            ggsave(roc_plot, plot = roc_p, width = 8, height = 6, dpi = 300)
          }
        }
      }
    }
  }
}
