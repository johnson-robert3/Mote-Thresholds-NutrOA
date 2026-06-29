#~~~
# Nutrient amendment figures
# 
# View changes in nutrient pack masses through experiment
#
# By: R. Johnson
#~~~


# Create 'np_mass' df from 'data_import-and-process' script first


#- Individual nutrient packs ----

# Total mass loss (units = g)
ggplot(np_mass %>% mutate(treatment_nutrients = factor(treatment_nutrients, levels = c('pulsed', '2g', '4g', '7g', '10g')))) +
   #
   geom_point(aes(x = treatment_nutrients, y = mass_loss_g, color = treatment_ph), 
              position = position_dodge(width=0.3), size=2, alpha=0.6) +
   #
   scale_color_manual(name = 'pH', values = ph_col_env) +
   #
   theme_classic() %>%
   fig_theme()


# Daily rate of mass loss (units = g/d)
ggplot(np_mass %>% mutate(treatment_nutrients = factor(treatment_nutrients, levels = c('pulsed', '2g', '4g', '7g', '10g')))) +
   #
   geom_point(aes(x = treatment_nutrients, y = mass_loss_rate, color = treatment_ph), 
              position = position_dodge(width=0.3), size=2, alpha=0.6) +
   #
   scale_color_manual(name = 'pH', values = ph_col_env) +
   #
   theme_classic() %>%
   fig_theme()


# Percent mass loss (units = %)
ggplot(np_mass %>% mutate(treatment_nutrients = factor(treatment_nutrients, levels = c('pulsed', '2g', '4g', '7g', '10g')))) +
   #
   geom_point(aes(x = treatment_nutrients, y = perc_mass_loss, color = treatment_ph), 
              position = position_dodge(width=0.3), size=2, alpha=0.6) +
   #
   scale_color_manual(name = 'pH', values = ph_col_env) +
   #
   theme_classic() %>%
   fig_theme()


# Daily rate of percent mass loss (units = %/d)
ggplot(np_mass %>% mutate(treatment_nutrients = factor(treatment_nutrients, levels = c('pulsed', '2g', '4g', '7g', '10g')))) +
   #
   geom_point(aes(x = treatment_nutrients, y = perc_mass_loss_rate, color = treatment_ph), 
              position = position_dodge(width=0.3), size=2, alpha=0.6) +
   #
   scale_color_manual(name = 'pH', values = ph_col_env) +
   #
   theme_classic() %>%
   fig_theme()




#- Treatment means ----

# Calculate means and SE by treatment
np_mass_trt <- np_mass %>%
   summarize(across(c(days_buried, mass_loss_g, perc_mass_loss, mass_loss_rate, perc_mass_loss_rate),
             list(mean = ~mean(., na.rm=TRUE), se = se), .names = "{.fn}_{.col}"), 
             .by = c(treatment_nutrients, treatment_ph)) 



# Total mass loss by treatment means (g)
ggplot(np_mass_trt %>% mutate(treatment_nutrients = factor(treatment_nutrients, levels = c('pulsed', '2g', '4g', '7g', '10g')))) +
   #
   geom_point(aes(x = treatment_nutrients, y = mean_mass_loss_g, color = treatment_ph), 
              position = position_dodge(width=0.3), size=2, alpha=0.6) +
   geom_errorbar(aes(x = treatment_nutrients, 
                     ymin = mean_mass_loss_g - se_mass_loss_g, 
                     ymax = mean_mass_loss_g + se_mass_loss_g, 
                     color = treatment_ph), 
              position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   #
   scale_color_manual(name = 'pH', values = ph_col_env) +
   #
   theme_classic() %>%
   fig_theme()


# Percent mass loss by treatment means (%)
ggplot(np_mass_trt %>% mutate(treatment_nutrients = factor(treatment_nutrients, levels = c('pulsed', '2g', '4g', '7g', '10g')))) +
   #
   geom_point(aes(x = treatment_nutrients, y = mean_perc_mass_loss, color = treatment_ph), 
              position = position_dodge(width=0.3), size=2, alpha=0.6) +
   geom_errorbar(aes(x = treatment_nutrients, 
                     ymin = mean_perc_mass_loss - se_perc_mass_loss, 
                     ymax = mean_perc_mass_loss + se_perc_mass_loss, 
                     color = treatment_ph), 
              position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   #
   scale_color_manual(name = 'pH', values = ph_col_env) +
   #
   theme_classic() %>%
   fig_theme()


# Daily rate of mass loss by treatment means (g/d)
ggplot(np_mass_trt %>% mutate(treatment_nutrients = factor(treatment_nutrients, levels = c('pulsed', '2g', '4g', '7g', '10g')))) +
   #
   geom_point(aes(x = treatment_nutrients, y = mean_mass_loss_rate, color = treatment_ph), 
              position = position_dodge(width=0.3), size=2, alpha=0.6) +
   geom_errorbar(aes(x = treatment_nutrients, 
                     ymin = mean_mass_loss_rate - se_mass_loss_rate, 
                     ymax = mean_mass_loss_rate + se_mass_loss_rate, 
                     color = treatment_ph), 
              position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   #
   scale_color_manual(name = 'pH', values = ph_col_env) +
   #
   theme_classic() %>%
   fig_theme()


# Daily rate of percent mass loss by treatment means (%/d)
ggplot(np_mass_trt %>% mutate(treatment_nutrients = factor(treatment_nutrients, levels = c('pulsed', '2g', '4g', '7g', '10g')))) +
   #
   geom_point(aes(x = treatment_nutrients, y = mean_perc_mass_loss_rate, color = treatment_ph), 
              position = position_dodge(width=0.3), size=2, alpha=0.6) +
   geom_errorbar(aes(x = treatment_nutrients, 
                     ymin = mean_perc_mass_loss_rate - se_perc_mass_loss_rate, 
                     ymax = mean_perc_mass_loss_rate + se_perc_mass_loss_rate, 
                     color = treatment_ph), 
              position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   #
   scale_color_manual(name = 'pH', values = ph_col_env) +
   #
   theme_classic() %>%
   fig_theme()





#- Mean and ind. points together ----

# total mass loss (units = g)
ggplot(np_mass_trt %>% mutate(treatment_nutrients = factor(treatment_nutrients, levels = c('pulsed', '2g', '4g', '7g', '10g')))) +
   #
   geom_point(aes(x = treatment_nutrients, y = mean_mass_loss_g, color = treatment_ph), 
              position = position_dodge(width=0.3), size=2.5, alpha=0.9) +
   geom_errorbar(aes(x = treatment_nutrients, 
                     ymin = mean_mass_loss_g - se_mass_loss_g, 
                     ymax = mean_mass_loss_g + se_mass_loss_g, 
                     color = treatment_ph), 
              position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   # ind points
   geom_point(data = np_mass %>% mutate(treatment_nutrients = factor(treatment_nutrients, levels = c('pulsed', '2g', '4g', '7g', '10g'))),
              aes(x = treatment_nutrients, y = mass_loss_g, color = treatment_ph), 
              position = position_dodge(width=0.3), size=1, alpha=0.3) +
   #
   scale_color_manual(name = 'pH', values = ph_col_env) +
   labs(x = "Nutrient treatment",
        y = "Total nutrient amendment (g)") +
   #
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/nutrient_amendment_total.png", height=4, width=5, units="in", dpi=300)



# total percent mass loss (units = %)
ggplot(np_mass_trt %>% mutate(treatment_nutrients = factor(treatment_nutrients, levels = c('pulsed', '2g', '4g', '7g', '10g')))) +
   #
   geom_point(aes(x = treatment_nutrients, y = mean_perc_mass_loss, color = treatment_ph), 
              position = position_dodge(width=0.3), size=2.5, alpha=0.9) +
   geom_errorbar(aes(x = treatment_nutrients, 
                     ymin = mean_perc_mass_loss - se_perc_mass_loss, 
                     ymax = mean_perc_mass_loss + se_perc_mass_loss, 
                     color = treatment_ph), 
              position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   # ind points
   geom_point(data = np_mass %>% mutate(treatment_nutrients = factor(treatment_nutrients, levels = c('pulsed', '2g', '4g', '7g', '10g'))),
              aes(x = treatment_nutrients, y = perc_mass_loss, color = treatment_ph), 
              position = position_dodge(width=0.3), size=1, alpha=0.3) +
   #
   scale_color_manual(name = 'pH', values = ph_col_env) +
   labs(x = "Nutrient treatment",
        y = "Change in nutrient mass (%)") +
   #
   theme_classic() %>%
   fig_theme()



# rate of nutrient mass loss (units = g/d)
ggplot(np_mass_trt %>% mutate(treatment_nutrients = factor(treatment_nutrients, levels = c('pulsed', '2g', '4g', '7g', '10g')))) +
   #
   geom_point(aes(x = treatment_nutrients, y = mean_mass_loss_rate, color = treatment_ph), 
              position = position_dodge(width=0.3), size=2.5, alpha=0.9) +
   geom_errorbar(aes(x = treatment_nutrients, 
                     ymin = mean_mass_loss_rate - se_mass_loss_rate, 
                     ymax = mean_mass_loss_rate + se_mass_loss_rate, 
                     color = treatment_ph), 
              position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   # ind points
   geom_point(data = np_mass %>% mutate(treatment_nutrients = factor(treatment_nutrients, levels = c('pulsed', '2g', '4g', '7g', '10g'))),
              aes(x = treatment_nutrients, y = mass_loss_rate, color = treatment_ph), 
              position = position_dodge(width=0.3), size=1, alpha=0.3) +
   #
   scale_color_manual(name = 'pH', values = ph_col_env) +
   labs(x = "Nutrient treatment",
        y = "Rate of nutrient amendment (g / day)") +
   #
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/nutrient_amendment_rate.png", height=4, width=5, units="in", dpi=300)




