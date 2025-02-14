# Figure 5b
# biotransformation plot prep
lit_comp_biotransf = lit_comp_summ %>%
  left_join(pharmuse,by="Pharmaceutical") %>%
  filter(!is.na(Readily.Biodegrades))

lit_comp_biotransf$Readily.Biodegrades <- factor(
  lit_comp_biotransf$Readily.Biodegrades,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

# NOTE: below y=x line, lit-reported concentration > predicted; above line is
# predicted > lit_reported
biotransf = ggplot(lit_comp_biotransf, aes(x = Mean,
                                           y = Average_Predicted_Concentration,
                                           fill = Readily.Biodegrades)) +
  scale_x_log10() +
  scale_y_log10() +
  geom_point(shape = 21, size = 4, color = "black") +  
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "black") + 
  scale_fill_manual(values = c("Yes" = "blue", "No" = "red")) +  
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x=expression("Measured Concentration ("*mu*"g/L)"),
       y = expression("Predicted Concentration ("*mu*"g/L)"),
       fill = "Readily biodegrades?") +
  theme(axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        axis.text.x = element_text(size = 12),   
        axis.text.y = element_text(size = 12)) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 12))
ggsave("biotransf.svg", plot = biotransf, width = 5, height = 5,
       device="svg")

# Figure S1:
# Henry's law constant plot prep
lit_comp_HL = lit_comp_summ %>%
  left_join(pharmuse,by="Pharmaceutical") %>%
  filter(!is.na(Henrys.Law)) %>%
  mutate(henry_law_category = case_when(Henrys.Law > 1E-03 & Molar_Mass < 200 ~
                                          "High",
                                        Henrys.Law <= 1E-03 & Henrys.Law > 1E-06 &
                                          Molar_Mass ~ "Intermediate",
                                        Henrys.Law <= 1E-06 & Molar_Mass ~ "Low"))

# NOTE: below y=x line, lit-reported concentration > predicted; above line is
# predicted > lit_reported
HL = ggplot(lit_comp_HL, aes(x = Mean, y = Average_Predicted_Concentration,
                             fill = henry_law_category)) +
  scale_x_log10() +
  scale_y_log10() +
  geom_point(shape = 21, size = 4, color = "black") +  
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "black") +
  scale_fill_manual(values = c("Low" = "blue", "Intermediate" = "red", 
                               "High" = "violet")) +   
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x=expression("Measured Concentration ("*mu*"g/L)"),
       y = expression("Predicted Concentration ("*mu*"g/L)"),
       fill = "Henry's law constant") +
  theme(axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        axis.text.x = element_text(size = 12),   
        axis.text.y = element_text(size = 12)) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 12))
ggsave("HL.svg", plot = HL, width = 5, height = 5,
       device="svg")

# Figure 5a
# LogKow plot prep
lit_comp_logKow = lit_comp_summ %>%
  left_join(pharmuse,by="Pharmaceutical") %>%
  filter(!is.na(LogKow.Octanol.Water)) %>%
  mutate(logKow_category = case_when(LogKow.Octanol.Water >= 5 ~ "High",
                                     LogKow.Octanol.Water < 5 & LogKow.Octanol.Water >=2 ~
                                       "Intermediate",
                                     LogKow.Octanol.Water < 2 ~ "Low"))

# NOTE: below y=x line, lit-reported concentration > predicted; above line is
# predicted > lit_reported
logKow = ggplot(lit_comp_logKow, aes(x = Mean, y = Average_Predicted_Concentration,
                                     fill = logKow_category)) +
  scale_x_log10() +
  scale_y_log10() +
  geom_point(shape = 21, size = 4, color = "black") +  
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "black") + 
  scale_fill_manual(values = c("Low" = "blue", "Intermediate" = "violet",
                               "High" = "red")) +   
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x=expression("Measured Concentration ("*mu*"g/L)"),
       y = expression("Predicted Concentration ("*mu*"g/L)"),
       fill = "Log Kow") +
  theme(axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 12))
ggsave("logKow.svg", plot = logKow, width = 5, height = 5,
       device="svg")

# OLD Figure 2 code: Average predicted concentration across 100 ensembles vs average
# number of prescriptions 
median_val = median(average_profile$Average_Predicted_Concentration)
labels = c("Median = 0.04 \U00B5g/L",
           "4e-04 \U00B5g/L",
           "4 \U00B5g/L")
annotations = data.frame(
  x = c(6700,7250,7500),  
  y = c(median_val, 4e-04, 4),  
  label = labels
)
avg_plot = ggplot(average_profile, aes(x=Average_Number_Prescriptions,
                                       y=Average_Predicted_Concentration)) + 
  scale_y_log10() + 
  annotate("segment", x = 0, y = median_val, xend = 8000, yend = median_val,
           color = "black",
           linewidth=1) + 
  annotate("segment", x = 0, y = 4e-04, xend = 8000, yend = 4e-04, 
           color = "black",
           linewidth=1) + 
  annotate("segment", x = 0, y = 4, xend = 8000, yend = 4, color = "black",
           linewidth=1) + 
  geom_text(data = annotations, aes(x = x, y = y, label = label), vjust = -0.5,
            color="black") +
  geom_point(color = "darkgray",alpha=0.5) + 
  guides(color = "none") +
  labs(x="Number of prescriptions",
       y=expression("Log Predicted Concentration ("*mu*"g/L)"),
       color="Pharmaceuticals") +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        axis.text.x = element_text(size = 12),   
        axis.text.y = element_text(size = 12)) 
ggsave("average_drug_conc_plot.svg", plot = avg_plot, width = 5, height = 5,
       device="svg")

# old inset in new figure 2
inset_hist = ggplot(average_profile, aes(x = Average_Predicted_Concentration)) +
  geom_histogram(binwidth = 0.001, fill = "gray", color = "gray", alpha=0.3) +
  geom_vline(xintercept = 4, color = "black", linetype = "dashed", size = 0.1) +
  geom_vline(xintercept = 4e-2, color = "black", linetype = "dashed", size = 0.7) +
  geom_vline(xintercept = 4e-4, color = "black", linetype = "dashed", size = 0.7) +
  annotate("text", x = 0.02, y =22 , label=bquote(4 ~ x ~ 10^-4 ~ mu * "g/L"),
           color = "black", vjust = -0.5,
           size = 3) +
  annotate("text", x = 0.06, y =22 , label=bquote(4 ~ x ~ 10^-2 ~ mu * "g/L"),
           color = "black", vjust = -0.5,
           size = 3) +
  coord_cartesian(xlim = c(0, 0.1)) +  
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),  
    axis.ticks = element_line(color = "black") 
  )

combined_hist = ggdraw() +
  draw_plot(main_hist) +
  draw_plot(inset_hist, x = 0.55, y = 0.55, width = 0.45, height = 0.45)