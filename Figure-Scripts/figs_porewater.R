#~~~
# Figures for porewater
#
# By: R. Johnson
#~~~



#--
# pH
#--

# week 2
ggplot(porewater_trt %>% filter(parameter=="pH" & week == "w2" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), width=0.2) +
   labs(title = "Porewater pH at week 2",
        x = "Nutrient treatment",
        y = "pH")


# week 6
ggplot(porewater_trt %>% filter(parameter=="pH" & week == "w6" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(name = 'pH', values = ph_col_env) +
   labs(title = "Porewater pH - Stress (wk 6)",
        x = "Nutrient treatment (g)",
        y = "pH") +
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/pw_pH_w6.png", height=4, width=5, units="in", dpi=300)


# week 9
ggplot(porewater_trt %>% filter(parameter=="pH" & week == "w9" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(name = 'pH', values = ph_col_env) +
   labs(title = "Porewater pH - Recovery (wk 9)",
        x = "Nutrient treatment (g)",
        y = "pH") +
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/pw_pH_w9.png", height=4, width=5, units="in", dpi=300)


# ph - acclimation, stress, recovery periods
ggplot(porewater_trt %>% filter(parameter=="pH" & week %in% c('w2', 'w6', 'w9') & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor(),
                 period = recode_values(week,
                                        'w2' ~ "Acclimation",
                                        'w6' ~ "Stress",
                                        'w9' ~ "Recovery"),
                 period = factor(period, levels = c("Acclimation", "Stress", "Recovery")))) +
   #
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(name = 'pH', values = ph_col_env) +
   labs(title = "Porewater pH",
        x = "Nutrient treatment (g)",
        y = "pH") +
   facet_wrap(facets = vars(period)) +
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/pw_pH_periods.png", height=3, width=8, units="in", dpi=300)



#--
# Total dissolved sulfide
#--

# porewater DS between treatments at wk 2, mean + SE
ggplot(porewater_trt %>% filter(parameter == "porewater_S_uM" & week == "w2" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, 
                     ymin = mean - se, ymax = mean + se, color = treatment_ph), width=0.2) +
   labs(title = "Porewater sulfide at week 2",
        x = "Nutrient treatment",
        y = "Total dissolved sulfide (uM)")


# porewater DS between treatments at wk 6, mean + SE
ggplot(porewater_trt %>% filter(parameter == "porewater_S_uM" & week == "w6" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, 
                     ymin = mean - se, ymax = mean + se, color = treatment_ph), width=0.2) +
   labs(title = "Porewater sulfide at week 6",
        x = "Nutrient treatment",
        y = "Total dissolved sulfide (uM)")

   # sulfide is very low across treatments at 6 weeks; maybe the nutrients stimulated denitrification? so denitrifiers outcompeted sulfate reducers?


# porewater DS between treatments at wk 9, mean + SE
ggplot(porewater_trt %>% filter(parameter == "porewater_S_uM" & week == "w9" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, 
                     ymin = mean - se, ymax = mean + se, color = treatment_ph), width=0.2) +
   labs(title = "Porewater sulfide at week 9",
        x = "Nutrient treatment",
        y = "Total dissolved sulfide (uM)")


# Sulfide - acclimation, stress, recovery periods
ggplot(porewater_trt %>% filter(parameter == "porewater_S_uM" & week %in% c('w2', 'w6', 'w9') & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor(),
                 period = recode_values(week,
                                        'w2' ~ "Acclimation",
                                        'w6' ~ "Stress",
                                        'w9' ~ "Recovery"),
                 period = factor(period, levels = c("Acclimation", "Stress", "Recovery")))) +
   #
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(name = 'pH', values = ph_col_env) +
   labs(title = "Porewater sulfide",
        x = "Nutrient treatment (g)",
        y = "Total dissolved sulfide (uM)") +
   facet_wrap(facets = vars(period)) +
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/pw_sulfide_periods.png", height=3, width=8, units="in", dpi=300)


   # Sulfide - split panels by nutrient treatment instead of sample period
   ggplot(porewater_trt %>% filter(parameter == "porewater_S_uM" & week %in% c('w2', 'w6', 'w9') & treatment_nutrients!="pulsed") %>%
             mutate(treatment_nutrients = factor(treatment_nutrients, levels = c('0g', '2g', '4g', '7g', '10g'))) %>%
             mutate(week = parse_number(week) %>% as.factor())) +
      #
      geom_line(aes(x = week, y = mean, color = treatment_ph, group = treatment_ph), 
                position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
      geom_errorbar(aes(x = week, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                    position = position_dodge(width=0.3), width=0, linewidth=0.67) +
      geom_point(aes(x = week, y = mean, color = treatment_ph), 
                 size=3.5, shape=19, position = position_dodge(width=0.3)) +
      scale_color_manual(name = 'pH', values = ph_col_env) +
      labs(title = "Porewater sulfide",
           x = "Week",
           y = "Total dissolved sulfide (uM)") +
      facet_wrap(facets = vars(treatment_nutrients), ncol=1) +
      theme_classic() %>%
      fig_theme()
   
   ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/pw_sulfide_treatments.png", height=8, width=4, units="in", dpi=300)



#--
# Hydrogen sulfide
#--

# H2S between treatments at wk 2, mean + SE
ggplot(porewater_trt %>% filter(parameter == "pw_h2s_uM" & week == "w2" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, 
                     ymin = mean - se, ymax = mean + se, color = treatment_ph), width=0.2) +
   labs(title = "Hydrogen sulfide at week 2",
        x = "Nutrient treatment",
        y = "Hydrogen sulfide (uM)")


# H2S between treatments at wk 6, mean + SE
ggplot(porewater_trt %>% filter(parameter == "pw_h2s_uM" & week == "w6" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, 
                     ymin = mean - se, ymax = mean + se, color = treatment_ph), width=0.2) +
   labs(title = "Hydrogen sulfide at week 6",
        x = "Nutrient treatment",
        y = "Hydrogen sulfide (uM)")


# H2S between treatments at wk 9, mean + SE
ggplot(porewater_trt %>% filter(parameter == "pw_h2s_uM" & week == "w9" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, 
                     ymin = mean - se, ymax = mean + se, color = treatment_ph), width=0.2) +
   labs(title = "Hydrogen sulfide at week 9",
        x = "Nutrient treatment",
        y = "Hydrogen sulfide (uM)")


# H2S - acclimation, stress, recovery periods
ggplot(porewater_trt %>% filter(parameter == "pw_h2s_uM" & week %in% c('w2', 'w6', 'w9') & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor(),
                 period = recode_values(week,
                                        'w2' ~ "Acclimation",
                                        'w6' ~ "Stress",
                                        'w9' ~ "Recovery"),
                 period = factor(period, levels = c("Acclimation", "Stress", "Recovery")))) +
   #
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(name = 'pH', values = ph_col_env) +
   labs(title = "Hydrogen sulfide (H2S)",
        x = "Nutrient treatment (g)",
        y = "Hydrogen sulfide (uM)") +
   facet_wrap(facets = vars(period)) +
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/pw_h2s_periods.png", height=3, width=8, units="in", dpi=300)


   # H2S - split panels by nutrient treatment instead of sample period
   ggplot(porewater_trt %>% filter(parameter == "pw_h2s_uM" & week %in% c('w2', 'w6', 'w9') & treatment_nutrients!="pulsed") %>%
             mutate(treatment_nutrients = factor(treatment_nutrients, levels = c('0g', '2g', '4g', '7g', '10g'))) %>%
             mutate(week = parse_number(week) %>% as.factor())) +
      #
      geom_line(aes(x = week, y = mean, color = treatment_ph, group = treatment_ph), 
                position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
      geom_errorbar(aes(x = week, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                    position = position_dodge(width=0.3), width=0, linewidth=0.67) +
      geom_point(aes(x = week, y = mean, color = treatment_ph), 
                 size=3.5, shape=19, position = position_dodge(width=0.3)) +
      scale_color_manual(name = 'pH', values = ph_col_env) +
      labs(title = "Hydrogen sulfide (H2S)",
           x = "Week",
           y = "Hydrogen sulfide (uM)") +
      facet_wrap(facets = vars(treatment_nutrients), ncol=1) +
      theme_classic() %>%
      fig_theme()
   
   ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/pw_h2s_treatments.png", height=8, width=4, units="in", dpi=300)







# some exploratory comparisons (not done yet)
   

# total sulfide vs pH
ggplot(porewater_plant %>% filter_out(is.na(porewater_S_uM)), aes(x = pH, y = log10(porewater_S_uM))) +
   geom_point(aes(color = treatment_nutrients, shape = week), size=2, alpha=0.6) +
   theme_classic()


# h2s vs pH
ggplot(porewater_plant %>% filter_out(is.na(pw_h2s_uM)), aes(x = pH, y = log10(pw_h2s_uM))) +
   geom_point(aes(color = treatment_nutrients, shape = week), size=2, alpha=0.6) +
   theme_classic()


# h2s vs total sulfide
ggplot(porewater_plant %>% filter_out(is.na(pw_h2s_uM)), aes(x = porewater_S_uM, y = pw_h2s_uM)) +
   geom_point(aes(color = treatment_nutrients, shape = week), size=2, alpha=0.6) +
   theme_classic()



#ranks

# ph
ggplot(porewater_plant) + geom_point(aes(x = rank(pH), y = pH, color=treatment_nutrients))
ggplot(porewater_plant %>% filter(week== "w6")) + geom_point(aes(x = rank(pH), y = pH, color=treatment_nutrients))

# ph vs h2s
ggplot(porewater_plant %>% filter(week== "w6")) + geom_point(aes(x = rank(pH), y = pw_h2s_uM, color=treatment_nutrients))
ggplot(porewater_plant %>% filter(week== "w2")) + geom_point(aes(x = rank(pH), y = pw_h2s_uM, color=treatment_nutrients))
ggplot(porewater_plant %>% filter(week== "w9")) + geom_point(aes(x = rank(pH), y = pw_h2s_uM, color=treatment_nutrients))
ggplot(porewater_plant %>% filter(week== "w9")) + geom_point(aes(x = rank(pH), y = pw_h2s_uM, color=treatment_nutrients)) + facet_wrap(~treatment_nutrients)
# regular ph (not ranked)
ggplot(porewater_plant %>% filter(week== "w9")) + geom_point(aes(x = pH, y = pw_h2s_uM, color=treatment_nutrients)) + facet_wrap(~treatment_nutrients)





