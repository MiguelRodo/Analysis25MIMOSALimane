
simulate_data <- function(n_samples, w, params, N) {
  set.seed(2025)
  # Extract parameter values from the list
  a_s_R  <- params$a_s_R
  b_s_R  <- params$b_s_R
  a_s_NR <- params$a_s_NR
  b_s_NR <- params$b_s_NR

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
    # Unstimulated condition - DIFFERENT depending on responder status
    if (responder_status[i] == 1) {
      # Responders: LOW unstimulated response 
      p_u[i] <- rbeta(1, a_u_R, b_u_R)
    } else {
      # Non-responders: HIGH unstimulated response (closer to stimulated level)
      p_u[i] <- rbeta(1, a_u_NR, b_u_NR)
    }
    n_u[i] <- rbinom(1, N, p_u[i])
    # Stimulated condition - difference based on (responders and non-responders)
    if (responder_status[i] == 1) {
      p_s[i] <- rbeta(1, a_s_R, b_s_R)
    } else {
      p_s[i] <- rbeta(1, a_s_NR, b_s_NR)
    }
    n_s[i] <- rbinom(1, N, p_s[i])
  }
stim_data <- data.frame(
    SUBJECTID = paste0("Subject_", 1:n_samples),
    CYTOKINE = "IL2",
    TCELL = "CD4",
    STIMULATION = "Stimulated",
    CYTNUM = n_s,
    NSUB = N - n_s,
    RefTreat = "Treatment",
    stringsAsFactors = FALSE
  )

unstim_data <- data.frame(
    SUBJECTID = paste0("Subject_", 1:n_samples),
    CYTOKINE = "IL2",
    TCELL = "CD4",
    STIMULATION = "Unstimulated",
    CYTNUM = n_u,
    NSUB = N - n_u,
    RefTreat = "Reference",
    stringsAsFactors = FALSE
  )
  # Combine both into one long-format data frame
  sim_data <- rbind(unstim_data, stim_data)
  # Add responder status to the data
return(list(
  data = sim_data,
  responder_status = responder_status
))
}
# Function to plot the cell count trends for responders and non-responders
# It visualizes the change of functional cell counts
#across conditions and responder status
plot_responder_comparison <- function(sim_result) {
  # Get data and responder status
  data <- sim_result$data
  responder_status <- sim_result$responder_status
  # Add responder label to data
  data$RESPONDER <- rep(ifelse(responder_status == 1,
                               "Responder", "Non-Responder"), 2)
  data$STIMULATION <- factor(data$STIMULATION,
                            levels = c("Unstimulated", "Stimulated"))
 # Plot trend in cell counts 
 p1 <- ggplot(data, aes(x = STIMULATION, y = CYTNUM, color = RESPONDER)) +
   geom_line(aes(group = SUBJECTID), alpha = 0.6, size = 0.5) +
    geom_point(size = 2, alpha = 0.8) +
    facet_wrap(~RESPONDER) +
    labs(
      title = "Cell Count Distribution by Responder Status",
      x = "Condition",
      y = "Number of Functional Cells",
      color = "Responder Status"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
    return(p1)
}