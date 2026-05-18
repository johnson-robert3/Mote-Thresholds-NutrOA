#~~~
# Epiphytes
#
# By: R. Johnson
#~~~


# initial data
tmp <- epiphytes %>%
   # calculate epiphyte dry mass (units = g) (from all blades/shoots in a pot)
   mutate(epi_biomass_g = sample_dish_mass_g - dish_mass_g) %>%
   # add other plant info
   left_join(plant_dat %>% select(plant_id, treatment_ph, treatment_nutrients, site)) %>%
   relocate(treatment_ph, site, .after = treatment_nutrients)



# add other plant parameters on
tmp1 <- tmp %>%
   select(-sample_dish_mass_g, -dish_mass_g) %>%
   rename(notes_epi = notes) %>%
   # shoot biomass
   left_join(biomass_plant %>% select(plant_id, week, species, shoot_biomass_g)) %>%
   # morphometry
   left_join(morph_plant %>% select(plant_id, week, species, blade_length, blade_width, tot_leaf_area)) 


# standardize epiphytes to other vars
epi_plant <- tmp1 %>%
   mutate(
      # to blade length (BL) (units = mg/cm)
      epi_by_BL = epi_biomass_g*1000 / blade_length,
      # to total leaf surface area (leafSA) (units = mg/cm^2)
      epi_by_leafSA = epi_biomass_g*1000 / tot_leaf_area)




