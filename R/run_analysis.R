# Function to run the analysis with different parameters
# It simulates data, fits the MIMOSA model, calculates ROC results, and plots the curves

run_analysis <- function(n_samples_list, N_list, w_list, f_list, c_list, mu_u_NR, mu_u_R, n_runs = 10) {
    if (!dir.exists("plots")) dir.create("plots")

    for (n_samples in n_samples_list) {
        for (N in N_list) {
            for (w in w_list) {
                for (f in f_list) {
                    for (c in c_list) {
                        message("=== Running Simulation with Parameters ===")
                        message(paste("Parameters: n_samples =", n_samples, ", N =", N, ", w =", w, ", f =", f, ", c =", c))

                        all_roc_mimosa <- list()
                        all_roc_fisher <- list()

                        for (run in 1:n_runs) {
                            params <- calculate_params(mu_u_NR, mu_u_R, f, c)
                            sim <- simulate_data(n_samples, w, params, N, seed = 2025 + run)

                            E <- prepare_for_mimosa(sim$data)
                            if (is.null(E)) {
                                message("E is NULL – skipping this combination")
                                next
                            }
                            fit <- fit_MIMOSA(E)

                            roc_result <- ROC_curve(fit, n_samples, sim)
                            roc_fisher <- fishers_curve(sim)

                            all_roc_mimosa[[run]] <- roc_result
                            all_roc_fisher[[run]] <- roc_fisher
                        }

                        avg_roc_mimosa <- average_roc_curves(all_roc_mimosa)
                        avg_roc_fisher <- average_roc_curves(all_roc_fisher)

                        p <- plot_curves(avg_roc_mimosa, avg_roc_fisher, n_samples)
                        filename <- sprintf("plots/ROC_avg_ns%d_N%.0f_w%.2f_f%.0f_c%.0f_runs%d.png", n_samples, N, w, f, c, n_runs)
                        ggsave(filename, plot = p, width = 8, height = 6, dpi = 300)
                    }
                }
            }
        }
    }
}
