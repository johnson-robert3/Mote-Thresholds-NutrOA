#~~~
# View various epiphyte biomass variables
#
# By: R. Johnson
#~~~



#--
# Halodule
#--

#- plain dry mass values

# Hw epiphytes at wk 6
ggplot(epi_plant %>% filter(week=="w6" & species=="Hw" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(epi_biomass_g, na.rm=TRUE),
                    se = se(epi_biomass_g),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), width=0.2) +
   labs(title = "Hw epiphytes at week 6 (mean ± SE)",
        x = "Nutrient treatment",
        y = "Epiphyte mass (g)")


# Hw epiphytes at wk 9
ggplot(epi_plant %>% filter(week=="w9" & species=="Hw" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(epi_biomass_g, na.rm=TRUE),
                    se = se(epi_biomass_g),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), width=0.2) +
   labs(title = "Hw epiphytes at week 9 (mean ± SE)",
        x = "Nutrient treatment",
        y = "Epiphyte mass (g)")



# Tt epiphytes at wk 6
ggplot(epi_plant %>% filter(week=="w6" & species=="Tt" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(epi_biomass_g, na.rm=TRUE),
                    se = se(epi_biomass_g),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), width=0.2) +
   labs(title = "Tt epiphytes at week 6 (mean ± SE)",
        x = "Nutrient treatment",
        y = "Epiphyte mass (g)")


# Tt epiphytes at wk 9
ggplot(epi_plant %>% filter(week=="w9" & species=="Tt" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(epi_biomass_g, na.rm=TRUE),
                    se = se(epi_biomass_g),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), size=2) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), width=0.2) +
   labs(title = "Tt epiphytes at week 9 (mean ± SE)",
        x = "Nutrient treatment",
        y = "Epiphyte mass (g)")



#- Epiphyte mass relative to total leaf surface area

# Hw - wk 6
ggplot(epi_plant %>% filter(week=="w6" & species=="Hw" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(epi_by_leafSA, na.rm=TRUE),
                    se = se(epi_by_leafSA),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(name = 'pH', values = ph_col_hw) +
   labs(title = "Halodule epiphytes per leaf area - Stress (wk 6)",
        x = "Nutrient treatment (g)",
        y = "Epiphyte mass (mg DM / cm^2 leaf)") +
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Seagrass-NutrOA-Thresholds/hw_epiphytes_w6.png", height=4, width=5, units="in", dpi=300)


# Hw - wk 9
ggplot(epi_plant %>% filter(week=="w9" & species=="Hw" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(epi_by_leafSA, na.rm=TRUE),
                    se = se(epi_by_leafSA),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(name = 'pH', values = ph_col_hw) +
   labs(title = "Halodule epiphytes per leaf area - Recovery (wk 9)",
        x = "Nutrient treatment (g)",
        y = "Epiphyte mass (mg DM / cm^2 leaf)") +
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Seagrass-NutrOA-Thresholds/hw_epiphytes_w9.png", height=4, width=5, units="in", dpi=300)


# Tt - wk 6
ggplot(epi_plant %>% filter(week=="w6" & species=="Tt" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(epi_by_leafSA, na.rm=TRUE),
                    se = se(epi_by_leafSA),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(name = 'pH', values = ph_col_tt) +
   labs(title = "Thalassia epiphytes per leaf area - Stress (wk 6)",
        x = "Nutrient treatment (g)",
        y = "Epiphyte mass (mg DM / cm^2 leaf)") +
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Seagrass-NutrOA-Thresholds/tt_epiphytes_w6.png", height=4, width=5, units="in", dpi=300)


# Tt - wk 9
ggplot(epi_plant %>% filter(week=="w9" & species=="Tt" & treatment_nutrients!="pulsed") %>%
          mutate(treatment_nutrients = parse_number(treatment_nutrients) %>% as.factor()) %>%
          summarize(mean = mean(epi_by_leafSA, na.rm=TRUE),
                    se = se(epi_by_leafSA),
                    .by = c(treatment_ph, treatment_nutrients))) +
   geom_line(aes(x = treatment_nutrients, y = mean, color = treatment_ph, group = treatment_ph), 
             position = position_dodge(width=0.3), linewidth=0.75, alpha=0.4) +
   geom_errorbar(aes(x = treatment_nutrients, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 position = position_dodge(width=0.3), width=0, linewidth=0.67) +
   geom_point(aes(x = treatment_nutrients, y = mean, color = treatment_ph), 
              size=3.5, shape=19, position = position_dodge(width=0.3)) +
   scale_color_manual(name = 'pH', values = ph_col_tt) +
   labs(title = "Thalassia epiphytes per leaf area - Recovery (wk 9)",
        x = "Nutrient treatment (g)",
        y = "Epiphyte mass (mg DM / cm^2 leaf)") +
   theme_classic() %>%
   fig_theme()

ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Seagrass-NutrOA-Thresholds/tt_epiphytes_w9.png", height=4, width=5, units="in", dpi=300)





