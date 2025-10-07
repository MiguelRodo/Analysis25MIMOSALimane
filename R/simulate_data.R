# Function to simulate data for MIMOSA analysis
# It generates data based on specified parameters and returns a data frame
# and responder status

simulate_data <- function(n_samples, w, params, N, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  # Extract parameter values from the list
  a_s <- params$a_s
  b_s <- params$b_s
  a_u_R <- params$a_u_R
  b_u_R <- params$b_u_R
  a_u_NR <- params$a_u_NR
  b_u_NR <- params$b_u_NR

  # Storage vectors
  responder_status <- rbinom(n_samples, 1, w) # who is a responder
  p_u <- rep(NA, n_samples)
  p_s <- rep(NA, n_samples)
  n_s <- rep(NA, n_samples) # no of functional cells
  n_u <- rep(NA, n_samples) # no of functional cells

  for (i in seq_len(n_samples)) {
    # Unstimulated condition - DIFFERENT depending on responder status
    if (responder_status[i] == 1) {
      # Responders: LOW unstimulated response (to create big difference with stimulated)
      p_u[i] <- rbeta(1, a_u_R, b_u_R)
      p_s[i] <- rbeta(1, a_s, b_s)
    } else {
      # Non-responders: HIGH unstimulated response (closer to stimulated level)
      p_u[i] <- rbeta(1, a_u_NR, b_u_NR)
      p_s[i] <- rbeta(1, a_u_NR, b_u_NR)
    }
    n_u[i] <- rbinom(1, N, p_u[i])
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

# Combining columns
sim_data <- rbind(unstim_data, stim_data)

  # Add responder status to the data
  return(list(
    data = sim_data,
    responder_status = responder_status
  ))
}

# Function to plot the cell count trends for responders and non-responders
# It visualizes the change of functional cell counts across conditions and responder status
plot_responder_comparison <- function(sim_result) {
   
  # Extract data and responder status
  data <- sim_result$data
  responder_status <- sim_result$responder_status
  
  # Create a lookup for SUBJECTID → Responder label
  subj_ids <- unique(data$SUBJECTID)
  responder_map <- data.frame(
    SUBJECTID = subj_ids,
    RESPONDER = ifelse(responder_status == 1, "Responder", "Non-Responder")
  )
  
  # Merge into data
  data <- merge(data, responder_map, by = "SUBJECTID")
  
  # Ensure correct order of stimulation
  data$STIMULATION <- factor(data$STIMULATION, levels = c("Unstimulated", "Stimulated"))
  
  # Plot
  p1 <- ggplot(data, aes(x = STIMULATION, y = CYTNUM, color = RESPONDER)) +
    geom_line(aes(group = SUBJECTID), alpha = 0.6, linewidth = 0.5) +
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
