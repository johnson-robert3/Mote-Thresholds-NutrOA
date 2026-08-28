#~~~
# Stats for comparing differences among treatments
#
#~~~


library(nlme)


# difference in porewater sulfide at week 9

tmp <- porewater_plant %>%
   # lengthen the df; create a 'parameter' column of variables and a 'result' column of values and drop missing values 
   pivot_longer(cols = c(pH, porewater_S_uM, pw_h2s_uM), names_to = 'parameter', values_to = 'result', values_drop_na = TRUE) %>%
   select(sample_id, plant_id, table, week, treatment_nutrients, treatment_ph, site, parameter, result)


# LME model
mod1 <- lme(result ~ treatment_nutrients * treatment_ph, 
            random = ~1|table,
            data = tmp %>% filter(week == "w9" & parameter == "porewater_S_uM") %>% filter_out(treatment_nutrients == "pulsed"),
            method = "REML")

summary(mod1)

emmeans(mod1, ~ treatment_nutrients * treatment_ph)

contrast(emmeans(mod1, ~ treatment_nutrients * treatment_ph),
         method = "pairwise", by = "treatment_ph", adjust = "tukey")
contrast(emmeans(mod1, ~ treatment_nutrients * treatment_ph),
         method = "pairwise", by = "treatment_nutrients", adjust = "tukey")



# difference in HW BPS at week 9
tmp <- shoots_plant %>% 
   filter(species == "Hw" & week == 'w9' & treatment_nutrients!="pulsed" & bps>0)

mod2 <- lme(bps ~ treatment_nutrients + treatment_ph, 
            random = ~1|table,
            data = tmp,
            method = "REML")
   #' no need for interaction, same effect of nutrients across both pHs

summary(mod2)
car::Anova(mod2)

emmeans(mod2, ~ treatment_nutrients * treatment_ph)

contrast(emmeans(mod2, ~ treatment_nutrients), method = "pairwise")
   #' some are sig., 10g > 0/2/4g, 7g > 0g (so, high nutrients have sig effect)
contrast(emmeans(mod2, ~ treatment_ph), method = "pairwise")
   #' sig., OA > ambient



# differences in Tt shoot count at week 6 and week 9
tmp <- shoots_plant %>% 
   filter(species == "Tt" & treatment_nutrients!="pulsed" & shoot_count>0)

#w6
mod3 <- lme(shoot_count ~ treatment_nutrients * treatment_ph, 
            random = ~1|table,
            data = tmp %>% filter(week == "w6"),
            method = "REML")

summary(mod3)
car::Anova(mod3)

contrast(emmeans(mod3, ~ treatment_nutrients * treatment_ph),
         method = "pairwise", by = "treatment_ph", adjust = "tukey")
   #' some sig., 10g > 0/2/4g in ambient pH (so, high nutrients have sig effect, but not in OA)
contrast(emmeans(mod3, ~ treatment_nutrients * treatment_ph),
         method = "pairwise", by = "treatment_nutrients", adjust = "tukey")
   #' no differences between pHs at nutrient levels


# w9
mod4 <- lme(shoot_count ~ treatment_nutrients * treatment_ph, 
            random = ~1|table,
            data = tmp %>% filter(week == "w9"),
            method = "REML")

summary(mod4)
car::Anova(mod4)

contrast(emmeans(mod4, ~ treatment_nutrients * treatment_ph),
         method = "pairwise", by = "treatment_ph", adjust = "tukey")  # ns
contrast(emmeans(mod4, ~ treatment_nutrients * treatment_ph),
         method = "pairwise", by = "treatment_nutrients", adjust = "tukey")  # ns



# differences in Hw shoot count at week 6 and week 9
tmp <- shoots_plant %>% 
   filter(species == "Hw" & treatment_nutrients!="pulsed" & shoot_count>0)

# w6
mod5 <- lme(shoot_count ~ treatment_nutrients * treatment_ph, 
            random = ~1|table,
            data = tmp %>% filter(week == "w6"),
            method = "REML")

summary(mod5)
car::Anova(mod5)

contrast(emmeans(mod5, ~ treatment_nutrients * treatment_ph),
         method = "pairwise", by = "treatment_ph", adjust = "tukey")
   #' some sig., 0g > 4/7/10g in OA pH (so, high nutrients have neg effect, but not in ambient pH)
contrast(emmeans(mod5, ~ treatment_nutrients * treatment_ph),
         method = "pairwise", by = "treatment_nutrients", adjust = "tukey")
   #' p=0.078 for pH diff in 0g trt


# w9
mod6 <- lme(shoot_count ~ treatment_nutrients * treatment_ph, 
            random = ~1|table,
            data = tmp %>% filter(week == "w9"),
            method = "REML")

summary(mod6)
car::Anova(mod6)

contrast(emmeans(mod6, ~ treatment_nutrients * treatment_ph),
         method = "pairwise", by = "treatment_ph", adjust = "tukey")
   #' some sig., 0g > 4/7/10g in OA pH (so, high nutrients have neg effect, but not in ambient pH)
contrast(emmeans(mod6, ~ treatment_nutrients * treatment_ph),
         method = "pairwise", by = "treatment_nutrients", adjust = "tukey")
   #' p=0.062 for pH diff in 0g trt



# differences in porewater pH at week 9






