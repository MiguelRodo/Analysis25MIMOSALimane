calculate_params <- function(mu_u_NR, mu_u_R, f, c_u, c_s) {
  
  # Stimulated condition 
  mu_s <- mu_u_R * (1 + f)  
  a_s <- c_s * mu_s
  b_s <- c_s * (1 - mu_s)
  
  # Unstimulated responders 
  a_u_R <- c_u * mu_u_R
  b_u_R <- c_u * (1 - mu_u_R)
  
  # Unstimulated non-responders 
  a_u_NR <- c_u * mu_u_NR
  b_u_NR <- c_u * (1 - mu_u_NR)

  return(list(
    a_s = a_s, b_s = b_s,
    a_u_R = a_u_R, b_u_R = b_u_R,
    a_u_NR = a_u_NR, b_u_NR = b_u_NR,
    mu_s = mu_s, mu_u_R = mu_u_R, mu_u_NR = mu_u_NR
  ))
}


