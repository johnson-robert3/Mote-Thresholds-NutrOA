#~~
# Halodule figures for Mote YR4 proposal
#
#~~

library(cowplot)


#---

#- Blades per shoot
bps <-
ggplot(shoots_plant %>% filter(species == "Hw" & week=="w9" & treatment_nutrients=="0g" & bps>0) %>%  # only including plants w/ living Hw
          summarize(mean = mean(bps, na.rm=TRUE),
                    se = se(bps),
                    .by = treatment_ph)) +
   #
   geom_point(aes(x = treatment_ph, y = mean, color = treatment_ph), 
              size=3.5, shape=19) +
   geom_errorbar(aes(x = treatment_ph, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 width=0, linewidth=0.67) +
   scale_color_manual(name = 'pH', values = ph_col_hw) +
   # labs(title = "Halodule BPS - Recovery (wk 9)",
   #      x = "pH treatment",
   #      y = "Blades per shoot") +
   scale_x_discrete(name = NULL, labels = c("amb.", "OA")) +
   scale_y_continuous(name = "Leaves per shoot", limits = c(1.4, 2.5), breaks = seq(1.5, 2.5, by=0.5)) +
   theme_classic() +
   theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
         legend.position = "none",
         axis.ticks = element_line(color='black'),
         axis.text = element_text(color='black', size=9),
         axis.title.y = element_text(color="black", size=10, margin = margin(r=4, 'line')))


#- Blade length
bl <-
ggplot(morph_plant %>% filter(species=="Hw" & week=="w9" & treatment_nutrients=="0g") %>%
          summarize(mean = mean(blade_length, na.rm=TRUE),
                    se = se(blade_length),
                    .by = treatment_ph)) +
   #
   geom_point(aes(x = treatment_ph, y = mean, color = treatment_ph), 
              size=3.5, shape=19) +
   geom_errorbar(aes(x = treatment_ph, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 width=0, linewidth=0.67) +
   scale_color_manual(name = 'pH', values = ph_col_hw) +
   # labs(title = NULL,
   #      x = NULL,
   #      y = "Leaf length (cm)") +
   scale_x_discrete(name = NULL, labels = c("amb.", "OA")) +
   scale_y_continuous(name = "Leaf length (cm)", limits = c(14, 25), breaks = seq(15, 25, by=5)) +
   theme_classic() +
   theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
         legend.position = "none",
         axis.ticks = element_line(color='black'),
         axis.text = element_text(color='black', size=9),
         axis.title.y = element_text(color="black", size=10, margin = margin(r=5, 'line')))



# create 2-panel figure of blade length and blades per shoot
windows(height=2, width=3)
plot_grid(bl, bps, ncol=2)


ggsave("C:/Users/rajohnson6/Desktop/Local-Repos/Seagrass-NutrOA-Thresholds/hw_bl-bps_forMote.png", height=1.5, width=2.5, units="in", dpi=300)



#---


#- Number of shoots per plant
ggplot(shoots_plant %>% filter(species == "Hw" & week=="w9" & treatment_nutrients=="0g" & shoot_count>0) %>%  # only including plants w/ living Hw
          summarize(mean = mean(shoot_count, na.rm=TRUE),
                    se = se(shoot_count),
                    .by = treatment_ph)) +
   #
   geom_point(aes(x = treatment_ph, y = mean, color = treatment_ph), 
              size=3.5, shape=19) +
   geom_errorbar(aes(x = treatment_ph, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 width=0, linewidth=0.67) +
   scale_color_manual(name = 'pH', values = ph_col_hw) +
   labs(title = "Halodule shoots - Recovery (wk 9)",
        x = "pH treatment",
        y = "Number of shoots (per pot)") +
   theme_classic() +
   theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
         legend.position = "none")



#- Number of leaves per plant
ggplot(shoots_plant %>% filter(species == "Hw" & week=="w9" & treatment_nutrients=="0g" & leaf_count>0) %>%  # only including plants w/ living Hw
          summarize(mean = mean(leaf_count, na.rm=TRUE),
                    se = se(leaf_count),
                    .by = treatment_ph)) +
   #
   geom_point(aes(x = treatment_ph, y = mean, color = treatment_ph), 
              size=3.5, shape=19) +
   geom_errorbar(aes(x = treatment_ph, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 width=0, linewidth=0.67) +
   scale_color_manual(name = 'pH', values = ph_col_hw) +
   labs(title = "Halodule number leaves - Recovery (wk 9)",
        x = "pH treatment",
        y = "Number of leaves (per pot)") +
   theme_classic() +
   theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
         legend.position = "none")



#- Leaf biomass
# b <-
ggplot(biomass_plant %>% filter(week=="w9" & species=="Hw" & treatment_nutrients=="0g") %>%
                 # convert to mg
                 mutate(shoot_biomass_g = shoot_biomass_g * 1000) %>%
          summarize(mean = mean(shoot_biomass_g, na.rm=TRUE),
                    se = se(shoot_biomass_g),
                    .by = treatment_ph)) +
   #
   geom_point(aes(x = treatment_ph, y = mean, color = treatment_ph), 
              size=3.5, shape=19) +
   geom_errorbar(aes(x = treatment_ph, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 width=0, linewidth=0.67) +
   scale_color_manual(name = 'pH', values = ph_col_hw) +
   # labs(title = NULL,
   #      x = NULL,
   #      y = "Shoot biomass (mg)") +
   scale_x_discrete(name = NULL, labels = c("Ambient", "OA")) +
   scale_y_continuous(name = "Shoot biomass (mg)", limits = c(25, 70), breaks = seq(30, 70, by=10)) +
   theme_classic() +
   theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
         legend.position = "none")



#- Epiphyte load
ggplot(epi_plant %>% filter(week=="w9" & species=="Hw" & treatment_nutrients=="0g") %>%
          summarize(mean = mean(epi_by_leafSA, na.rm=TRUE),
                    se = se(epi_by_leafSA),
                    .by = treatment_ph)) +
   #
   geom_point(aes(x = treatment_ph, y = mean, color = treatment_ph), 
              size=3.5, shape=19) +
   geom_errorbar(aes(x = treatment_ph, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
                 width=0, linewidth=0.67) +
   scale_color_manual(name = 'pH', values = ph_col_hw) +
   labs(title = "Halodule epiphytes per leaf area - Recovery (wk 9)",
        x = "pH treatment",
        y = "Epiphyte mass (mg DM / cm^2 leaf)") +
   theme_classic() +
   theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
         legend.position = "none")



#- Total leaf area (DO NOT USE)

## This is actually not applicable to Halodule. Morph was only measured on 1 shoot per pot, so this is only total leaf area for the shoot that
##  happened to be measured. Would need to combine the morph df with the leaf/shoot count df to get this total. 

# ggplot(morph_plant %>% filter(week=="w9" & species=="Hw" & treatment_nutrients=="0g") %>%
#           summarize(mean = mean(tot_leaf_area, na.rm=TRUE),
#                     se = se(tot_leaf_area),
#                     .by = treatment_ph)) +
#    #
#    geom_point(aes(x = treatment_ph, y = mean, color = treatment_ph), 
#               size=3.5, shape=19) +
#    geom_errorbar(aes(x = treatment_ph, y = mean, ymin = mean - se, ymax = mean + se, color = treatment_ph), 
#                  width=0, linewidth=0.67) +
#    scale_color_manual(name = 'pH', values = ph_col_hw) +
#    labs(title = "Halodule total leaf area - Recovery (wk 9)",
#         x = "pH treatment",
#         y = "Leaf surface area (cm^2)") +
#    theme_classic() +
#    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
#          legend.position = "none")



#- Mortality (survivorship)
   # need to create 'mort.comp' df from 'figs_plant-mortality' script

ggplot(mort.comp %>% filter(species=="Hw" & week=='w9' & treatment_nutrients=="0g")) +
   #
   geom_col(aes(x = treatment_ph, y = survival, fill = treatment_ph), color="gray20") +
   scale_fill_manual(name = 'pH', values = ph_col_hw) +
   lims(y = c(0, 100)) +
   labs(title = "Halodule survival - Recovery (wk 9)",
        x = "pH treatment",
        y = "Survival (%)") +
   theme_classic() +
   theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
         legend.position = "none")





