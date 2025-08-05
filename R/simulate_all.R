simulate_all <- function(n_samples_list, N_list, w_list, f_list, c_list,
                         mu_u_NR, mu_u_R) {
  
  for (n_samples in n_samples_list) {
    for (N in N_list) {
      for (w in w_list) {
        for (f in f_list) {
          for (c in c_list) {
            
            # Compute parameters
            params <- calculated_params(mu_u_NR, mu_u_R, f, c)
            
            # Simulate data
            sim_data <- get_sim_data(n_samples, w, params, N)
            
            # Prepare for MIMOSA and fit
            E <- prepare_for_mimosa(sim_data)
            fit <- fit_MIMOSA(E)
            
            # You could do something with `fit` here,
            # e.g., collect results, save, plot, etc.
          }
        }
      }
    }
  }
}

