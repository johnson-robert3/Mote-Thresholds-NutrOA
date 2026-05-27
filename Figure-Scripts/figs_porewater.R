#~~~
# Figures for porewater
#
# By: R. Johnson
#~~~



#--
# pH
#--

# week 2
ggplot(porewater_trt %>% filter(week == "w2" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_point(aes(x = treatment_nutrients, y = mean_pH, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean_pH, ymin = mean_pH - se_pH, ymax = mean_pH + se_pH, color = treatment_ph), width=0.2) +
   labs(title = "Porewater pH at week 2",
        x = "Nutrient treatment",
        y = "pH")


# week 6
ggplot(porewater_plant %>% filter(week == "w6" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(pH, na.rm=TRUE),
                    se = se(pH),
                    .by = c(treatment_ph, treatment_nutrients))) +
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
ggplot(porewater_plant %>% filter(week == "w9" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(pH, na.rm=TRUE),
                    se = se(pH),
                    .by = c(treatment_ph, treatment_nutrients))) +
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
ggplot(porewater_plant %>% filter(week %in% c('w2', 'w6', 'w9') & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(pH, na.rm=TRUE),
                    se = se(pH),
                    .by = c(treatment_ph, treatment_nutrients, week))) +
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
   facet_wrap(facets = vars(week)) +
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/pw_pH_periods.png", height=3, width=8, units="in", dpi=300)



#--
# Sulfide
#--

# porewater DS between treatments at wk 2, mean + SE
ggplot(porewater_trt %>% filter(week == "w2" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_point(aes(x = treatment_nutrients, y = mean_porewater_S_uM, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean_porewater_S_uM, 
                     ymin = mean_porewater_S_uM - se_porewater_S_uM, ymax = mean_porewater_S_uM + se_porewater_S_uM, color = treatment_ph), width=0.2) +
   labs(title = "Porewater sulfide at week 2",
        x = "Nutrient treatment",
        y = "Total dissolved sulfide (uM)")


# porewater DS between treatments at wk 6, mean + SE
ggplot(porewater_trt %>% filter(week == "w6" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_point(aes(x = treatment_nutrients, y = mean_porewater_S_uM, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean_porewater_S_uM, 
                     ymin = mean_porewater_S_uM - se_porewater_S_uM, ymax = mean_porewater_S_uM + se_porewater_S_uM, color = treatment_ph), width=0.2) +
   labs(title = "Porewater sulfide at week 6",
        x = "Nutrient treatment",
        y = "Total dissolved sulfide (uM)")

   # sulfide is very low across treatments at 6 weeks; maybe the nutrients stimulated denitrification? so denitrifiers outcompeted sulfate reducers?


# porewater DS between treatments at wk 9, mean + SE
ggplot(porewater_trt %>% filter(week == "w9" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_point(aes(x = treatment_nutrients, y = mean_porewater_S_uM, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean_porewater_S_uM, 
                     ymin = mean_porewater_S_uM - se_porewater_S_uM, ymax = mean_porewater_S_uM + se_porewater_S_uM, color = treatment_ph), width=0.2) +
   labs(title = "Porewater sulfide at week 9",
        x = "Nutrient treatment",
        y = "Total dissolved sulfide (uM)")


# DS - acclimation, stress, recovery periods
ggplot(porewater_plant %>% filter(week %in% c('w2', 'w6', 'w9') & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(porewater_S_uM, na.rm=TRUE),
                    se = se(porewater_S_uM),
                    .by = c(treatment_ph, treatment_nutrients, week))) +
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
   facet_wrap(facets = vars(week)) +
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/pw_sulfide_periods.png", height=3, width=8, units="in", dpi=300)



