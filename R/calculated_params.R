
calculated_params <- function(mu_u_NR, mu_u_R, f, c){
  mu_s <- mu_u_NR * (1 + f)# Stimulated mean (fixed for all) used f = 1
  # Convert means for beta distribution
  a_s <- c * mu_s
  b_s <- c * (1 - mu_s)

  a_u_R <- c * mu_u_R
  b_u_R <- c * (1 - mu_u_R)

  a_u_NR <- c * mu_u_NR
  b_u_NR <- c * (1 - mu_u_NR)

  # return as a named list
  return(list(
    a_s    = a_s,
    b_s    = b_s,
    a_u_R  = a_u_R,
    b_u_R  = b_u_R,
    a_u_NR = a_u_NR,
    b_u_NR = b_u_NR
  ))
}
