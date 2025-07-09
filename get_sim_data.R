
get_sim_data <- function(n_samples, w, params, N ) {
  set.seed(2025)
# Extract parameter values from the list
  a_s    <- params$a_s
  b_s    <- params$b_s 
  a_u_R  <- params$a_u_R 
  b_u_R  <- params$b_u_R
  a_u_NR <- params$a_u_NR 
  b_u_NR <- params$b_u_NR 
  
# Storage vectors
responder_status <- rbinom(n_samples, 1, w)# who is a responder
p_u <- rep(NA, n_samples)
p_s <- rep(NA, n_samples)
n_s <- rep(NA, n_samples) #no of functional cells
n_u <- rep(NA, n_samples) #no of functional cells
p_hat_s <-  rep(NA, n_samples)
p_hat_u <- rep(NA, n_samples)
p_posterior_s <- rep(NA, n_samples)
p_posterior_u <- rep(NA, n_samples)

for (i in seq_len(n_samples)) {
  # Stimulated condition 
  p_s[i] <- rbeta(1, a_s, b_s)
  n_s[i] <- rbinom(1, N, p_s[i])
  p_hat_s[i] <- n_s[i] / N
  p_posterior_s[i] <- (n_s[i] + a_s) / (N + a_s + b_s)
 # Unstimulated condition — DIFFERENT depending on responder status
  if (responder_status[i] == 1) {
    p_u[i] <- rbeta(1, a_u_R, b_u_R)
  } else {
    p_u[i] <- rbeta(1, a_u_NR, b_u_NR)
  }
  n_u[i] <- rbinom(1, N, p_u[i])
  p_hat_u[i] <- n_u[i] / N
  p_posterior_u[i] <- (n_u[i] + (ifelse(responder_status[i] == 1, a_u_R, a_u_NR))) /(N + (ifelse(responder_status[i] == 1, a_u_R + b_u_R, a_u_NR + b_u_NR)))
}
  
# Return a list 
  return(list(
    responder_status = responder_status,
    p_s = p_s, 
    p_u = p_u,
    n_s = n_s, 
    n_u = n_u,
    p_hat_s = p_hat_s,
    p_hat_u = p_hat_u,
    p_posterior_s = p_posterior_s,
    p_posterior_u = p_posterior_u
  ))
  }

