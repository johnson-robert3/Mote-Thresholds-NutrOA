#~~~
# Figures to view leaf and shoot variables
#
# By: R. Johnson
#~~~



#--
# Shoot Structure
#--

#-- Number of shoots ----

# Hw shoots - week 9
ggplot(shoots_plant %>% 
          filter(species == "Hw" & week=="w9" & treatment_nutrients!="pulsed" & shoot_count>0) %>%  # only including plants w/ living Hw
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(shoot_count, na.rm=TRUE),
                    se = se(shoot_count),
                    .by = c(treatment_ph, treatment_nutrients))) +
   #
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   scale_color_manual(name = 'pH', values = ph_col_hw) +
   labs(title = "Halodule shoots - Recovery (wk 9)",
        x = "Nutrient treatment (g)",
        y = "Number of shoots (per pot)") +
   theme_classic() +
   theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/hw_shoots_w9.png", height=4, width=5, units="in", dpi=300)


# Hw shoots - acclimation, stress, recovery periods
ggplot(shoots_plant %>% 
          filter(species == "Hw" & week %in% c('w2', 'w6', 'w9') & treatment_nutrients!="pulsed" & shoot_count>0) %>%  # only including plants w/ living Hw
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(shoot_count, na.rm=TRUE),
                    se = se(shoot_count),
                    .by = c(treatment_ph, treatment_nutrients, week))) +
   #
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(name = 'pH', values = ph_col_hw) +
   labs(title = "Halodule shoot count",
        x = "Nutrient treatment (g)",
        y = "Number of shoots (per pot)") +
   facet_wrap(facets = vars(week)) +
   theme_classic() +
   theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/hw_shoots_periods.png", height=3, width=8, units="in", dpi=300)



# Tt shoots - week 9
ggplot(shoots_plant %>% 
          filter(species == "Tt" & week=="w9" & treatment_nutrients!="pulsed" & shoot_count>0) %>%  # only including plants w/ living Hw
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(shoot_count, na.rm=TRUE),
                    se = se(shoot_count),
                    .by = c(treatment_ph, treatment_nutrients))) +
   #
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   scale_color_manual(name = 'pH', values = ph_col_tt) +
   labs(title = "Thalassia shoots - Recovery (wk 9)",
        x = "Nutrient treatment (g)",
        y = "Number of shoots (per pot)") +
   theme_classic() +
   theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/tt_shoots_w9.png", height=4, width=5, units="in", dpi=300)


# Tt shoots - acclimation, stress, recovery periods
ggplot(shoots_plant %>% 
          filter(species == "Tt" & week %in% c('w2', 'w6', 'w9') & treatment_nutrients!="pulsed" & shoot_count>0) %>%  # only including plants w/ living Hw
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(shoot_count, na.rm=TRUE),
                    se = se(shoot_count),
                    .by = c(treatment_ph, treatment_nutrients, week))) +
   #
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(name = 'pH', values = ph_col_tt) +
   labs(title = "Thalassia shoot count",
        x = "Nutrient treatment (g)",
        y = "Number of shoots (per pot)") +
   facet_wrap(facets = vars(week)) +
   theme_classic() +
   theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/tt_shoots_periods.png", height=3, width=8, units="in", dpi=300)



#-- Number of leaves ----

# Hw - week 9
ggplot(shoots_plant %>% 
          filter(species == "Hw" & week=="w9" & treatment_nutrients!="pulsed" & leaf_count>0) %>%  # only including plants w/ living Hw
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(leaf_count, na.rm=TRUE),
                    se = se(leaf_count),
                    .by = c(treatment_ph, treatment_nutrients))) +
   
   # will still need to somehow relate this to the number of leaves at the start (wk2) (b/c plants started with different numbers of shoots)
   
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   scale_color_manual(name = 'pH', values = ph_col_hw) +
   labs(title = "Halodule number leaves - Recovery (wk 9)",
        x = "Nutrient treatment (g)",
        y = "Number of leaves (per pot)") +
   theme_classic() +
   theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/hw_leaves_w9.png", height=4, width=5, units="in", dpi=300)


# Tt - week 9
ggplot(shoots_plant %>% 
          filter(species == "Tt" & week=="w9" & treatment_nutrients!="pulsed" & leaf_count>0) %>%  # only including plants w/ living Tt
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(leaf_count, na.rm=TRUE),
                    se = se(leaf_count),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   scale_color_manual(name = 'pH', values = ph_col_tt) +
   labs(title = "Thalassia number leaves - Recovery (wk 9)",
        x = "Nutrient treatment (g)",
        y = "Number of leaves (per pot)") +
   theme_classic() +
   theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/tt_leaves_w9.png", height=4, width=5, units="in", dpi=300)


#-- Blades per shoot ----

# Hw bps - week 9
ggplot(shoots_plant %>% 
          filter(species == "Hw" & week=="w9" & treatment_nutrients!="pulsed" & bps>0) %>%  # only including plants w/ living Hw
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(bps, na.rm=TRUE),
                    se = se(bps),
                    .by = c(treatment_ph, treatment_nutrients))) +
   #
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(name = 'pH', values = ph_col_hw) +
   labs(title = "Halodule BPS - Recovery (wk 9)",
        x = "Nutrient treatment (g)",
        y = "Blades per shoot") +
   theme_classic() +
   theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/hw_bps_w9.png", height=4, width=5, units="in", dpi=300)


# Hw bps - acclimation, stress, recovery periods
ggplot(shoots_plant %>% 
          filter(species == "Hw" & week %in% c('w2', 'w6', 'w9') & treatment_nutrients!="pulsed" & bps>0) %>%  # only including plants w/ living Tt
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(bps, na.rm=TRUE),
                    se = se(bps),
                    .by = c(treatment_ph, treatment_nutrients, week))) +
   #
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(name = 'pH', values = ph_col_hw) +
   labs(title = "Halodule BPS",
        x = "Nutrient treatment (g)",
        y = "Blades per shoot") +
   facet_wrap(facets = vars(week)) +
   theme_classic() +
   theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/hw_bps_periods.png", height=3, width=8, units="in", dpi=300)


   # Hw bps - split panels by nutrient treatment instead of sample period
   ggplot(shoots_plant %>% 
             filter(species == "Hw" & week %in% c('w2', 'w6', 'w9') & treatment_nutrients!="pulsed" & bps>0) %>%  # only including plants w/ living Hw
             mutate(treatment_nutrients = factor(treatment_nutrients, levels = c('0g', '2g', '4g', '7g', '10g'))) %>%
             mutate(week = parse_number(week) %>% as.factor()) %>%
             summarize(mean = mean(bps, na.rm=TRUE),
                       se = se(bps),
                       .by = c(treatment_ph, treatment_nutrients, week))) +
      geom_line(aes(x = week, y = mean, color = treatment_ph, group = treatment_ph), 
                position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
      geom_errorbar(aes(x = week, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                    position = position_dodge(width=0.3), width=0, linewidth=0.67) +
      geom_point(aes(x = week, y = mean, color = treatment_ph), 
                 size=3.5, shape=19, position = position_dodge(width=0.3)) +
      scale_color_manual(values = ph_col_hw) +
      labs(title = "Halodule BPS",
           x = "Week",
           y = "Blades per shoot") +
      facet_wrap(facets = vars(treatment_nutrients), ncol=1) +
      theme_classic() %>%
      fig_theme()
   
   ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/hw_bps_treatments.png", height=8, width=4, units="in", dpi=300)



# Tt bps - week 9
ggplot(shoots_plant %>% 
          filter(species == "Tt" & week=="w9" & treatment_nutrients!="pulsed" & bps>0) %>%  # only including plants w/ living Tt
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(bps, na.rm=TRUE),
                    se = se(bps),
                    .by = c(treatment_ph, treatment_nutrients))) +
   #
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(name = 'pH', values = ph_col_tt) +
   labs(title = "Thalassia BPS - Recovery (wk 9)",
        x = "Nutrient treatment (g)",
        y = "Blades per shoot") +
   theme_classic() +
   theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/tt_bps_w9.png", height=4, width=5, units="in", dpi=300)


# Tt bps - acclimation, stress, recovery periods
ggplot(shoots_plant %>% 
          filter(species == "Tt" & week %in% c('w2', 'w6', 'w9') & treatment_nutrients!="pulsed" & bps>0) %>%  # only including plants w/ living Tt
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(bps, na.rm=TRUE),
                    se = se(bps),
                    .by = c(treatment_ph, treatment_nutrients, week))) +
   #
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(name = 'pH', values = ph_col_tt) +
   labs(title = "Thalassia BPS",
        x = "Nutrient treatment (g)",
        y = "Blades per shoot") +
   facet_wrap(facets = vars(week)) +
   theme_classic() +
   theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/tt_bps_periods.png", height=3, width=8, units="in", dpi=300)


   # Tt bps - split panels by nutrient treatment instead of sample period
   ggplot(shoots_plant %>% 
             filter(species == "Tt" & week %in% c('w2', 'w6', 'w9') & treatment_nutrients!="pulsed" & bps>0) %>%  # only including plants w/ living Tt
             mutate(treatment_nutrients = factor(treatment_nutrients, levels = c('0g', '2g', '4g', '7g', '10g'))) %>%
             mutate(week = parse_number(week) %>% as.factor()) %>%
             summarize(mean = mean(bps, na.rm=TRUE),
                       se = se(bps),
                       .by = c(treatment_ph, treatment_nutrients, week))) +
      geom_line(aes(x = week, y = mean, color = treatment_ph, group = treatment_ph), 
                position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
      geom_errorbar(aes(x = week, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                    position = position_dodge(width=0.3), width=0, linewidth=0.67) +
      geom_point(aes(x = week, y = mean, color = treatment_ph), 
                 size=3.5, shape=19, position = position_dodge(width=0.3)) +
      scale_color_manual(values = ph_col_tt) +
      labs(title = "Thalassia BPS",
           x = "Week",
           y = "Blades per shoot") +
      facet_wrap(facets = vars(treatment_nutrients), ncol=1) +
      theme_classic() %>%
      fig_theme()
   
   ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/tt_bps_treatments.png", height=8, width=4, units="in", dpi=300)



# Box and whisker plots with jittered data points
{
# Halodule - number of shoots at wk 9, as a box and whisker plot with jittered data points
ggplot(shoots_plant %>% filter(species == "Hw" & week=="w9" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_boxplot(aes(x = treatment_nutrients, y = shoot_count, color = treatment_ph), outliers = FALSE) +
   geom_jitter(aes(x = treatment_nutrients, y = shoot_count, color = treatment_ph, shape = site), 
               position = position_jitterdodge(jitter.width=0.2, jitter.height=0.2)) +
   labs(title = "Hw number of shoots at week 9",
        x = "Nutrient treatment",
        y = "Number of Hw shoots (per pot)")


# Halodule - number of leaves at wk 9, as a box and whisker plot with jittered data points
ggplot(shoots_plant %>% filter(species == "Hw" & week=="w9" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_boxplot(aes(x = treatment_nutrients, y = leaf_count, color = treatment_ph), outliers = FALSE) +
   geom_jitter(aes(x = treatment_nutrients, y = leaf_count, color = treatment_ph, shape = site), 
               position = position_jitterdodge(jitter.width=0.2, jitter.height=0.2)) +
   labs(title = "Hw number of leaves at week 9",
        x = "Nutrient treatment",
        y = "Number of Hw leaves (per pot)")


# Thalassia - number of shoots at wk 9, as a box and whisker plot with jittered data points
ggplot(shoots_plant %>% filter(species == "Tt" & week=="w9" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_boxplot(aes(x = treatment_nutrients, y = shoot_count, color = treatment_ph), outliers = FALSE) +
   geom_jitter(aes(x = treatment_nutrients, y = shoot_count, color = treatment_ph, shape = site), 
               position = position_jitterdodge(jitter.width=0.2, jitter.height=0.2)) +
   labs(title = "Tt number of shoots at week 9",
        x = "Nutrient treatment",
        y = "Number of Tt shoots (per pot)")


# Thalassia - total number of leaves at wk 9, as a box and whisker plot with jittered data points
ggplot(shoots_plant %>% filter(species == "Tt" & week=="w9" & treatment_nutrients!="pulsed" & tt_blades>0) %>%  # only including plants w/ living Tt
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_boxplot(aes(x = treatment_nutrients, y = leaf_count, color = treatment_ph), outliers = FALSE) +
   geom_jitter(aes(x = treatment_nutrients, y = leaf_count, color = treatment_ph, shape = site), 
               position = position_jitterdodge(jitter.width=0.2, jitter.height=0.2)) +
   labs(title = "Tt number of leaves at week 9",
        x = "Nutrient treatment",
        y = "Number of Tt leaves (per pot)")
}



#--
# Leaf Morphometry
#--

#-- Blade Length ----

# Hw BL - week 9
windows(height=4, width=5)
ggplot(morph_plant %>% filter(week=="w9" & species=="Hw" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(blade_length, na.rm=TRUE),
                    se = se(blade_length),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(name = 'pH', values = ph_col_hw) +
   labs(title = "Halodule size - Recovery (wk 9)",
        x = "Nutrient treatment (g)",
        y = "Blade length (cm)") +
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/hw_BL_w9.png", height=4, width=5, units="in", dpi=300)


# Hw BL - acclimation, stress, recovery periods
ggplot(morph_plant %>% filter(species=="Hw" & week %in% c('w2', 'w6', 'w9') & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(blade_length, na.rm=TRUE),
                    se = se(blade_length),
                    .by = c(treatment_ph, treatment_nutrients, week))) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(name = 'pH', values = ph_col_hw) +
   labs(title = "Halodule size",
        x = "Nutrient treatment (g)",
        y = "Blade length (cm)") +
   facet_wrap(facets = vars(week)) +
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/hw_BL_periods.png", height=3, width=8, units="in", dpi=300)


# Hw weeks 6 and 9 together (example code for all symbols together)
{
ggplot(morph_plant %>% filter(week %in% c('w6','w9') & species=="Hw" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(blade_length, na.rm=TRUE),
                    se = se(blade_length),
                    .by = c(treatment_ph, treatment_nutrients, week))) +
   # both (open data points for wk 6, closed data points for wk 9)
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph, shape=week), 
              size=3.5, position = position_dodge(width=0.3)) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, linetype=week, group = interaction(week, treatment_ph)), 
             position = position_dodge(width=0.3), linewidth=0.75) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, 
                     color = treatment_ph, group = interaction(week, treatment_ph)), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   #
   scale_shape_manual(values = c('w6' = 1, 'w9' = 19), name = "Week") +
   scale_linetype_manual(values = c('w6' = 2, 'w9' = 1), name = "Week") +
   scale_color_manual(name = 'pH', values = ph_col_hw) +
   #
   labs(title = "Halodule height",
        x = "Nutrient treatment (g)",
        y = "Blade length (cm)") +
   theme_classic() +
   theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))
}


# Tt - week 9
windows(height=4, width=5)
ggplot(morph_plant %>% filter(week=="w9" & species=="Tt" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(blade_length, na.rm=TRUE),
                    se = se(blade_length),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(name = 'pH', values = ph_col_tt) +
   labs(title = "Thalassia size - Recovery (wk 9)",
        x = "Nutrient treatment (g)",
        y = "Blade length (cm)") +
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/tt_BL_w9.png", height=4, width=5, units="in", dpi=300)


# Tt BL - acclimation, stress, recovery periods
ggplot(morph_plant %>% filter(species=="Tt" & week %in% c('w2', 'w6', 'w9') & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(blade_length, na.rm=TRUE),
                    se = se(blade_length),
                    .by = c(treatment_ph, treatment_nutrients, week))) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(name = 'pH', values = ph_col_tt) +
   labs(title = "Thalassia size",
        x = "Nutrient treatment (g)",
        y = "Blade length (cm)") +
   facet_wrap(facets = vars(week)) +
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/tt_BL_periods.png", height=3, width=8, units="in", dpi=300)



# BL - Box and whisker plots with jittered data points
{
# Hw blade length at wk 9, as box and whisker with jittered points
ggplot(morph_plant %>% filter(week=="w9" & species=="Hw" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_boxplot(aes(x = treatment_nutrients, y = blade_length, color = treatment_ph), outliers = FALSE) +
   geom_jitter(aes(x = treatment_nutrients, y = blade_length, color = treatment_ph, shape = site), 
               position = position_jitterdodge(jitter.width=0.2, jitter.height=0.2)) +
   labs(title = "Hw blade length at week 9",
        x = "Nutrient treatment",
        y = "Hw blade length (cm)")


# Tt blade length at wk 9, as box and whisker with jittered points
ggplot(morph_plant %>% filter(week=="w9" & species=="Tt" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor())) +
   geom_boxplot(aes(x = treatment_nutrients, y = blade_length, color = treatment_ph), outliers = FALSE) +
   geom_jitter(aes(x = treatment_nutrients, y = blade_length, color = treatment_ph, shape = site), 
               position = position_jitterdodge(jitter.width=0.2, jitter.height=0.2)) +
   labs(title = "Tt blade length at week 9",
        x = "Nutrient treatment",
        y = "Tt blade length (cm)")
}



#-- Leaf Surface Area ----

#' total surface area of all leaves in each pot

# Hw total leaf area at wk 9, as mean + SE
ggplot(morph_plant %>% filter(week=="w9" & species=="Hw" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(tot_leaf_area, na.rm=TRUE),
                    se = se(tot_leaf_area),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(values = ph_col_hw) +
   labs(title = "Halodule total leaf area - Recovery (wk 9)",
        x = "Nutrient treatment (g)",
        y = "Leaf surface area (cm^2)") +
   theme_classic() %>%
   fig_theme()


# Hw LSA - acclimation, stress, recovery periods
ggplot(morph_plant %>% filter(species=="Hw" & week %in% c('w2', 'w6', 'w9') & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(tot_leaf_area, na.rm=TRUE),
                    se = se(tot_leaf_area),
                    .by = c(treatment_ph, treatment_nutrients, week))) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(values = ph_col_hw) +
   labs(title = "Halodule total leaf area",
        x = "Nutrient treatment (g)",
        y = "Leaf surface area (cm^2)") +
   facet_wrap(facets = vars(week)) +
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/hw_LSA_periods.png", height=3, width=8, units="in", dpi=300)


   # Hw LSA - split panels by nutrient treatment instead of sample period
   ggplot(morph_plant %>% filter(species=="Hw" & week %in% c('w2', 'w6', 'w9') & treatment_nutrients!="pulsed") %>%
             mutate(treatment_nutrients = factor(treatment_nutrients, levels = c('0g', '2g', '4g', '7g', '10g'))) %>%
             mutate(week = parse_number(week) %>% as.factor()) %>%
             summarize(mean = mean(tot_leaf_area, na.rm=TRUE),
                       se = se(tot_leaf_area),
                       .by = c(treatment_ph, treatment_nutrients, week))) +
      geom_line(aes(x = week, y = mean, color = treatment_ph, group = treatment_ph), 
                position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
      geom_errorbar(aes(x = week, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                    position = position_dodge(width=0.3), width=0, linewidth=0.67) +
      geom_point(aes(x = week, y = mean, color = treatment_ph), 
                 size=3.5, shape=19, position = position_dodge(width=0.3)) +
      scale_color_manual(values = ph_col_hw) +
      labs(title = "Halodule total leaf area",
           x = "Week",
           y = "Leaf surface area (cm^2)") +
      facet_wrap(facets = vars(treatment_nutrients), ncol=1) +
      theme_classic() %>%
      fig_theme()
   
   ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/hw_LSA_treatments.png", height=8, width=4, units="in", dpi=300)


# Tt total leaf area at wk 9, as mean + SE
ggplot(morph_plant %>% filter(week=="w9" & species=="Tt" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(tot_leaf_area, na.rm=TRUE),
                    se = se(tot_leaf_area),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   scale_color_manual(values = ph_col_tt) +
   labs(title = "Thalassia total leaf area - Recovery (wk 9)",
        x = "Nutrient treatment (g)",
        y = "Leaf surface area (cm^2)") +
   theme_classic() %>%
   fig_theme()


# Tt LSA - acclimation, stress, recovery periods
ggplot(morph_plant %>% filter(species=="Tt" & week %in% c('w2', 'w6', 'w9') & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(tot_leaf_area, na.rm=TRUE),
                    se = se(tot_leaf_area),
                    .by = c(treatment_ph, treatment_nutrients, week))) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(values = ph_col_tt) +
   labs(title = "Thalassia total leaf area",
        x = "Nutrient treatment (g)",
        y = "Leaf surface area (cm^2)") +
   facet_wrap(facets = vars(week)) +
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/tt_LSA_periods.png", height=3, width=8, units="in", dpi=300)


   # Tt LSA - split panels by nutrient treatment instead of sample period
   ggplot(morph_plant %>% filter(species=="Tt" & week %in% c('w2', 'w6', 'w9') & treatment_nutrients!="pulsed") %>%
             mutate(treatment_nutrients = factor(treatment_nutrients, levels = c('0g', '2g', '4g', '7g', '10g'))) %>%
             mutate(week = parse_number(week) %>% as.factor()) %>%
             summarize(mean = mean(tot_leaf_area, na.rm=TRUE),
                       se = se(tot_leaf_area),
                       .by = c(treatment_ph, treatment_nutrients, week))) +
      geom_line(aes(x = week, y = mean, color = treatment_ph, group = treatment_ph), 
                position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
      geom_errorbar(aes(x = week, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                    position = position_dodge(width=0.3), width=0, linewidth=0.67) +
      geom_point(aes(x = week, y = mean, color = treatment_ph), 
                 size=3.5, shape=19, position = position_dodge(width=0.3)) +
      scale_color_manual(values = ph_col_tt) +
      labs(title = "Thalassia total leaf area",
           x = "Week",
           y = "Leaf surface area (cm^2)") +
      facet_wrap(facets = vars(treatment_nutrients), ncol=1) +
      theme_classic() %>%
      fig_theme()
   
   ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/tt_LSA_treatments.png", height=8, width=4, units="in", dpi=300)



#--
# Shoot Biomass ----
#-- 

# Hw leaf biomass at wk 6, as mean + SE
ggplot(biomass_plant %>% filter(week=="w6" & species=="Hw" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor(),
                 # convert to mg
                 shoot_biomass_g = shoot_biomass_g * 1000) %>%
          summarize(mean = mean(shoot_biomass_g, na.rm=TRUE),
                    se = se(shoot_biomass_g),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   scale_color_manual(values = ph_col_hw) +
   labs(title = "Halodule shoot biomass - Stress (wk 6)",
        x = "Nutrient treatment",
        y = "Shoot biomass (mg DM)") +
   theme_classic() %>%
   fig_theme()


# Hw leaf biomass at wk 9, as mean + SE
windows(height=4, width=5)
ggplot(biomass_plant %>% filter(week=="w9" & species=="Hw" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor(),
                 # convert to mg
                 shoot_biomass_g = shoot_biomass_g * 1000) %>%
          summarize(mean = mean(shoot_biomass_g, na.rm=TRUE),
                    se = se(shoot_biomass_g),
                    .by = c(treatment_ph, treatment_nutrients))) +
   # faint data points in background
   geom_point(data = biomass_plant %>% filter(week=="w9" & species=="Hw" & treatment_nutrients!="pulsed") %>%
                 mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor(),
                        shoot_biomass_g = shoot_biomass_g * 1000),
              aes(x = treatment_nutrients, y = shoot_biomass_g, color = treatment_ph), 
              # size=1.5, shape=16, alpha=0.4, position = position_dodge(width=0.3)) +
              size=1.75, shape=19, alpha=0.3, position = position_jitterdodge(dodge.width=0.3, jitter.width=0.1)) +
   #
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(name = 'pH', values = ph_col_hw) +
   labs(title = "Halodule shoot biomass - Recovery (wk 9)",
        x = "Nutrient treatment (g)",
        y = "Shoot biomass (mg DM)") +
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/hw_biomass_w9.png", height=4, width=5, units="in", dpi=300)


# Tt leaf biomass at wk 6, as mean + SE
ggplot(biomass_plant %>% filter(week=="w6" & species=="Tt" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor(),
                 # convert to mg
                 shoot_biomass_g = shoot_biomass_g * 1000) %>%
          summarize(mean = mean(shoot_biomass_g, na.rm=TRUE),
                    se = se(shoot_biomass_g),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(values = ph_col_tt) +
   labs(title = "Thalassia shoot biomass - Stress (wk 6)",
        x = "Nutrient treatment",
        y = "Shoot biomass (mg DM)") +
   theme_classic() %>%
   fig_theme()


# Tt leaf biomass at wk 9, as mean + SE
windows(height=4, width=5)
ggplot(biomass_plant %>% filter(week=="w9" & species=="Tt" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor(),
                 # convert to mg
                 shoot_biomass_g = shoot_biomass_g * 1000) %>%
          summarize(mean = mean(shoot_biomass_g, na.rm=TRUE),
                    se = se(shoot_biomass_g),
                    .by = c(treatment_ph, treatment_nutrients))) +
   # faint data points in background
   geom_point(data = biomass_plant %>% filter(week=="w9" & species=="Tt" & treatment_nutrients!="pulsed") %>%
                 mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor(),
                        shoot_biomass_g = shoot_biomass_g * 1000),
              aes(x = treatment_nutrients, y = shoot_biomass_g, color = treatment_ph), 
              # size=1.5, shape=16, alpha=0.4, position = position_dodge(width=0.3)) +
              size=1.75, shape=19, alpha=0.3, position = position_jitterdodge(dodge.width=0.3, jitter.width=0.1)) +
   #
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(name = 'pH', values = ph_col_tt) +
   labs(title = "Thalassia shoot biomass - Recovery (wk 9)",
        x = "Nutrient treatment (g)",
        y = "Shoot biomass (mg DM)") +
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/tt_biomass_w9.png", height=4, width=5, units="in", dpi=300)



#-- For BioOce 2026 proposal - Week 6 biomass, Ambient pH only

# Hw 
ggplot(biomass_plant %>% filter(week=="w6" & species=="Hw" & treatment_nutrients!="pulsed" & treatment_ph=='ambient') %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.numeric(),
                 # convert to mg
                 shoot_biomass_g = shoot_biomass_g * 1000) %>%
          summarize(mean = mean(shoot_biomass_g, na.rm=TRUE),
                    se = se(shoot_biomass_g),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   
   geom_smooth(aes(x = treatment_nutrients, y = mean, color = treatment_ph), se=TRUE, span=2) +
   
   scale_color_manual(values = ph_col_hw) +
   labs(title = "Halodule shoot biomass - Stress (wk 6)",
        x = "Nutrient treatment",
        y = "Shoot biomass (mg DM)") +
   theme_classic() %>%
   fig_theme()


# Tt 
ggplot(biomass_plant %>% filter(week=="w6" & species=="Tt" & treatment_nutrients!="pulsed" & treatment_ph=='ambient') %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.numeric(),
                 # convert to mg
                 shoot_biomass_g = shoot_biomass_g * 1000) %>%
          summarize(mean = mean(shoot_biomass_g, na.rm=TRUE),
                    se = se(shoot_biomass_g),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph),
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   
   # geom_smooth(aes(x = treatment_nutrients, y = mean, color = treatment_ph), se=TRUE, span=2) +
   
   scale_color_manual(values = ph_col_tt) +
   labs(title = "Thalassia shoot biomass - Stress (wk 6)",
        x = "Nutrient treatment",
        y = "Shoot biomass (mg DM)") +
   theme_classic() %>%
   fig_theme()



# both
windows(height=2, width=3)
ggplot(biomass_plant %>% filter(week=="w6" & treatment_nutrients!="pulsed" & treatment_ph=='ambient') %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.numeric(),
                 # convert to mg
                 shoot_biomass_g = shoot_biomass_g * 1000,
                 species = species %>% replace_values("Hw" ~ "Halodule", "Tt" ~ "Thalassia")) %>%
          summarize(mean = mean(shoot_biomass_g, na.rm=TRUE),
                    se = se(shoot_biomass_g),
                    .by = c(treatment_ph, treatment_nutrients, species))) +
   geom_line(aes(x = treatment_nutrients, y = mean, group = species),
             linewidth=0.75, alpha=0.4, color="gray40") +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = species), 
                 width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = species), 
              size=3, shape=19) +
   
   # geom_smooth(aes(x = treatment_nutrients, y = mean, color = treatment_ph), se=TRUE, span=2) +
   
   scale_color_manual(values = c('Halodule' = "#4988C4", 'Thalassia' = "#00B7B5")) +
   scale_x_continuous(breaks = c(0, 2, 4, 7, 10), limits = c(-0.5, 10.5)) +
   labs(#title = "Shoot biomass - Stress (wk 6)",
        x = "Nutrient amendment (g)",
        y = "Shoot mass (mg)") +
   theme_classic() +
   theme(legend.position = "none",
         axis.text = element_text(color='black', size=9),
         axis.title = element_text(color='black', size=10),
         axis.title.x = element_text(margin = margin(t=3, 'line')),
         axis.title.y = element_text(margin = margin(r=4, 'line'))) +
   facet_wrap(facets = vars(species), scales = "free_y") #%>%
   # fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Mote-Thresholds-NutrOA/NSF_w6_biomass.png", height=4, width=5, units="in", dpi=300)



