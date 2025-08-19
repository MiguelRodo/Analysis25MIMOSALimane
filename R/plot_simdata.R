plot_simulated_counts <- function(sim_data, n_samples, N, w, f, c) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  sim_data <- sim_data %>%
    mutate(ResponseGroup = ifelse(RESPONDER == 1, "Responder", "Non-Responder"))
  plot_data <- sim_data %>%
    group_by(SUBJECTID, STIMULATION, ResponseGroup) %>%
    summarise(CYTNUM = sum(CYTNUM), .groups = "drop") %>%
    pivot_wider(names_from = STIMULATION, values_from = CYTNUM)
  responder_plot <- ggplot(filter(plot_data, ResponseGroup == "Responder")) +
    geom_point(aes(x = Unstimulated, y = Stimulated)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
    geom_segment(aes(x = Unstimulated, xend = Unstimulated,
                     y = Unstimulated, yend = Stimulated), alpha = 0.4) +
    labs(title = "Responders: Stimulated vs Unstimulated",
         x = "Unstimulated CYTNUM", y = "Stimulated CYTNUM") +
    theme_bw()
  nonresponder_plot <- ggplot(filter(plot_data,
                                  ResponseGroup == "Non-Responder")) +
    geom_point(aes(x = Unstimulated, y = Stimulated)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
    geom_segment(aes(x = Unstimulated, xend = Unstimulated,
                     y = Unstimulated, yend = Stimulated), alpha = 0.4) +
    labs(title = "Non-Responders: Stimulated vs Unstimulated",
         x = "Unstimulated CYTNUM", y = "Stimulated CYTNUM") +
    theme_bw()
  plot_base <- paste0("SimData_n", n_samples, "_N", N,
                      "_w", w, "_f", f, "_c", c)
  ggsave(paste0(plot_base, "_Responders.png"), plot = responder_plot,
         width = 6, height = 5, dpi = 300)
  ggsave(paste0(plot_base, "_NonResponders.png"),
         plot = nonresponder_plot, width = 6, height = 5, dpi = 300)
}
