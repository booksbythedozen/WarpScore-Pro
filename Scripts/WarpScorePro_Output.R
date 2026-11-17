# OBN_Output.R
# Ottawa Bayesian Neuropsychology (OBN) protocol.
# Bryce P Mulligan, PhD, CPsych
# 4 July 2023
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# This script performs output functions (i.e. generating and saving graphs, 
# tables, reports, etc).
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Print output values table
# print(BTDdf)
# ------------------------------------------------------------------------------
# Transform TestScore column into a factor with order matching BTDdf
BTDdf$TestScore <- factor(BTDdf$TestScore, levels=unique(BTDdf$TestScore))
# Plot the posterior probabilities and 95% PIs
p1 <- ggplot(BTDdf, aes(TestScore, p.value)) +   
   ylab("Posterior Probability") +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  ) +
  annotate("rect",                         # gold band to denote exceptionally-high range
           xmin = -Inf, xmax = Inf,
           ymin = 98, ymax = 100,
           fill = "violet", alpha = 0.3, color = NA) +
  annotate("rect",                         # light grey band to denote high-average range
           xmin = -Inf, xmax = Inf,
           ymin = 90, ymax = 98,
           fill = "#4B0082", alpha = 0.3, color = NA) +
    annotate("rect",                         # light grey band to denote high-average range
           xmin = -Inf, xmax = Inf,
           ymin = 75, ymax = 90,
           fill = "blue", alpha = 0.3, color = NA) +
  annotate("rect",                         # green band to denote average range
           xmin = -Inf, xmax = Inf,
           ymin = 25, ymax = 75,
           fill = "green", alpha = 0.2, color = NA) +
  annotate("rect",                         # light blue band to denote low-average range
           xmin = -Inf, xmax = Inf,
           ymin = 9, ymax = 25,
           fill = "yellow", alpha = 0.4, color = NA) +
    annotate("rect",                         # yellow band to denote below-average range
           xmin = -Inf, xmax = Inf,
           ymin = 2, ymax = 9,
           fill = "orange", alpha = 0.5, color = NA) +
  annotate("rect",                         # red band to denote exceptionally-low range
           xmin = -Inf, xmax = Inf,
           ymin = 0, ymax = 2,
           fill = "red", alpha = 0.5, color = NA) +
   geom_col() +  # default columns
   scale_x_discrete(guide = guide_axis(angle = 90)) +
   geom_errorbar(aes(ymin=L.PI, ymax=H.PI)) +
   geom_label(aes(label = ceiling(p.value)),
             vjust = -0.1,
             size = 7,
             fontface = "bold",
             label.size = 0,   # set >0 to add a border
             fill = "white", alpha = 0.5,
             color = "black") +
   theme(
     text = element_text(size = 20),          # “all” text baseline
     axis.title = element_text(size = 20),
     axis.title.x = element_blank(),
     axis.text  = element_text(size = 18),
     plot.title = element_text(size = 22),
     legend.text = element_text(size = 18),
     panel.grid.major.x = element_blank(),
     panel.grid.minor.x = element_blank(),
     panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
     panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.25)
  )

# ------------------------------------------------------------------------------
# Display plots
print(p1) # show plot
# ------------------------------------------------------------------------------
# Display tables
# ------------------------------------------------------------------------------
# Save plots/tables