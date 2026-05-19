#~~~
# Functions and values to use for aesthetics across figures
#
# By: R. Johnson
#~~~


#- Values for figures

# pH treatment color for Halodule (blues)
ph_col_hw <- c('ambient' = "#4988C4", 'OA' = "#0F2854")  # ambient - light; OA - dark
# pH treatment color for Thalassia (greens)
ph_col_tt <- c('ambient' = "#00B7B5", 'OA' = "#005461")  # ambient - light; OA - dark

# pH treatment color for pot-level variables (e.g., porewater) (purples)
ph_col_env <- c('ambient' = "#9A0680", 'OA' = "#4C0070")  # ambient - light; OA - dark



#- Function for figure aesthetics
fig_theme <- function (.fig) {
   
   .fig +
      theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
            axis.ticks = element_line(color='black'), 
            axis.text = element_text(color='black', size=10),
            axis.text.x = element_text(hjust=0.3, margin = margin(t=2, 'line')),
            axis.title = element_text(color="black", size=12), 
            axis.title.x = element_text(margin = margin(t=5, 'line')),
            axis.title.y = element_text(margin = margin(r=7, 'line')))
}



