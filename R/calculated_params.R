
calculated_params <- function(mu_u_NR, mu_u_R, f, c_R, c_NR){
  mu_s <- mu_u_NR * (1 + f)# Stimulated mean (fixed for all) used f = 1
  # Convert means for beta distribution
  a_s_R  <- c_R  * mu_s
  b_s_R  <- c_R  * (1 - mu_s)

  a_s_NR <- c_NR * mu_s
  b_s_NR <- c_NR * (1 - mu_s)

  # Unstimulated
  a_u_R  <- c_R  * mu_u_R
  b_u_R  <- c_R  * (1 - mu_u_R)

  a_u_NR <- c_NR * mu_u_NR
  b_u_NR <- c_NR * (1 - mu_u_NR)

  # return as a named list
  return(list(
    a_s_R    = a_s_R,
    b_s_R    = b_s_R,
    a_s_NR   = a_s_NR,
    b_s_NR = b_s_NR,
    a_u_R  = a_u_R,
    b_u_R  = b_u_R,
    a_u_NR = a_u_NR,
    b_u_NR = b_u_NR
  ))
}
