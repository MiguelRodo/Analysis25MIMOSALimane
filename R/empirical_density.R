empirical_density <- function() {
  
# mode: (alpha - 1) / (alpha + beta - 2)
# their mode:
# 4.5 / 85 * 4e-4 = 2.12e-5
# Parameters
mode_u <- 2.12e-5
con <- 2.3e4
alpha_unstim <- 1 + mode_u * (con - 2)
beta_unstim <- con - alpha_unstim
mean_uns <- alpha_unstim / con

# mu_u_R  <- 4e-4
# con     <- 3e4
# alpha_unstim <- con * mu_u_R
# beta_unstim  <- con * (1 - mu_u_R) 

# Simulate Beta data
set.seed(2025)
df <- data.frame(empirical = rbeta(200, alpha_unstim, beta_unstim))

y_height <- 1.2e4

# Plot
plot <- ggplot(df, aes(x = empirical)) +
  # histogram with white fill, black outline
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 2.3e-05, fill = "lightgrey", color = "darkgrey") +

  # blue dashed-dotted curve
  stat_function(fun = dbeta,
                args = list(shape1 = alpha_unstim, shape2 = beta_unstim),
                color = "blue", linetype = "dotdash", linewidth = 1) +
  
  # labels
  labs(x = expression("Empirical " * hat(rho)[u]),
       y = "Density") +  
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey90", color = "black"),
    strip.text = element_text(size = 12, face = "bold"),
    plot.margin = margin(10, 10, 10, 40),   
    plot.tag = element_text(size = 14, face = "bold")
  ) +
  
  # set axis ranges & breaks
  coord_cartesian(xlim = c(0, 6e-04), ylim = c(0, 12000)) +
  scale_x_continuous(breaks = seq(0, 6e-04, by = 2e-04),
                     labels = scales::scientific_format(digits = 1)) +
  scale_y_continuous(breaks = seq(0, 12000, by = 3000)) 

# save plot
filename <- sprintf("plots/empirical_density.png")
ggsave(
  filename,
  plot = plot,
  width = 12, height = 12, units = "cm", dpi = 300
)
}