#~~~
# Test differences in mortality across treatments
#
#~~~


library(emmeans)
library(ggResidpanel)


# Create a new combined survivorship dataset
tmp <- bind_rows(hw_surv %>%
                    select(plant_id, table, week, contains("treatment"), contains("alive"), w2_for_w9) %>%
                    filter(week == "w2") %>%
                    mutate(species = "Hw"),
                 tt_surv %>%
                    select(plant_id, table, week, contains("treatment"), contains("alive"), w2_for_w9) %>%
                    filter(week == "w2") %>%
                    mutate(species = "Tt")) %>%
   select(-week)



# Binomial GLM to test differences among treatments


# differences among treatments at week 6?

# Hw
mod1 <- glm(alive_w6 ~ treatment_ph * treatment_nutrients, 
            data = tmp %>% filter(species == "Hw" & alive_w2 == 1), 
            family = binomial(link = "logit"))

summary(mod1)
car::Anova(mod1, type = "III")
emmeans(mod1, ~ treatment_ph * treatment_nutrients, type = "response")
contrast(emmeans(mod1, ~ treatment_ph * treatment_nutrients, type = "response"), method = "pairwise", by = "treatment_nutrients")
contrast(emmeans(mod1, ~ treatment_ph * treatment_nutrients, type = "response"), method = "pairwise", by = "treatment_ph")


# Tt
mod2 <- glm(alive_w6 ~ treatment_ph * treatment_nutrients, 
            data = tmp %>% filter(species == "Tt" & alive_w2 == 1), 
            family = binomial(link = "logit"))

summary(mod2)
car::Anova(mod2, type = "III")
emmeans(mod2, ~ treatment_ph * treatment_nutrients, type = "response")
contrast(emmeans(mod2, ~ treatment_ph * treatment_nutrients, type = "response"), method = "pairwise", by = "treatment_nutrients")
contrast(emmeans(mod2, ~ treatment_ph * treatment_nutrients, type = "response"), method = "pairwise", by = "treatment_ph")


# differences among treatments at week 9? 

# Hw
mod3 <- glm(alive_w9 ~ treatment_ph * treatment_nutrients, 
            data = tmp %>% filter(species == "Hw" & alive_w2 == 1), 
            family = binomial(link = "logit"))

summary(mod3)
car::Anova(mod3, type = "III")
emmeans(mod3, ~ treatment_ph * treatment_nutrients, type = "response")
contrast(emmeans(mod3, ~ treatment_ph * treatment_nutrients, type = "response"), method = "pairwise", by = "treatment_nutrients")
contrast(emmeans(mod3, ~ treatment_ph * treatment_nutrients, type = "response"), method = "pairwise", by = "treatment_ph")


# Tt
mod4 <- glm(alive_w9 ~ treatment_ph * treatment_nutrients, 
            data = tmp %>% filter(species == "Tt" & alive_w2 == 1), 
            family = binomial(link = "logit"))

summary(mod4)
car::Anova(mod4, type = "III")
emmeans(mod4, ~ treatment_ph * treatment_nutrients, type = "response")
contrast(emmeans(mod4, ~ treatment_ph * treatment_nutrients, type = "response"), method = "pairwise", by = "treatment_nutrients")
contrast(emmeans(mod4, ~ treatment_ph * treatment_nutrients, type = "response"), method = "pairwise", by = "treatment_ph")




