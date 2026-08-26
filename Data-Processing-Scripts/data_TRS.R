#~~~
# Script to process and calculate sediment TRS concentration
#
# Total reducible inorganic sulfur
#
# By: R. Johnson
#~~~


#' need to run 'data_S-std-curves' and 'data_sulfide-spec-data' scripts first


#--
# Sediment TRS
#--

# Calculate TRS concentration in original sediment sample

   # (standardize to sediment mass used for the extraction)
   
   #' calculated S concentration from TRS ZnAc trap is in units of uM. 
   #' use that to calculate the total micromoles of S in the sulfide trap (10ml). 
   #' this is then the total micromoles of S produced by the sediment sample during extraction. 
   #' then calculate the micromoles produced per gram of sediment (using the mass of sediment used for the extraction). 
   #' this then gives the final result of TRS density in the sediment, in units of micromoles per gram of sediment. 
   #' can then adjust for DBD of the sample to get units of micromoles per cubic centimeter of sediment.


# calculate mean sediment dry bulk density value (units = g/ml = g/cm^3)
mean_dbd <- dbd_dat %>%
   mutate(mass_g = sample_dish_mass_g - dish_mass_g,
          dbd = mass_g / sample_vol_ml) %>%
   pull(dbd) %>%
   mean()


# Calculate TRS concentration in samples
trs_plant <- trs_dat %>%
  # create sample_id column to match spec data for joining
   mutate(sample_id = paste0("MSI25-", table, "-", plant_id, "-", week)) %>%
   relocate(sample_id) %>%
   # add sulfide concentration from spec data
   left_join(trs %>% select(sample_id, trap_S_uM, flag_spec = flag, notes_spec = notes)) %>%
   # calculate sediment TRS concentration on mass basis (units = umol/g)
   mutate(
      # total sulfide in ZnAc trap (units = micromoles)
      trap_S_umol = trap_S_uM * (extraction_znac_ml / 1000),
      # sediment TRS concentration (units = umol S / g sediment)
      sediment_TRS_umol_g = trap_S_umol / extraction_mass_g) %>%
   # calculate sediment TRS concentration on volume basis (units = umol/cm^3)
   mutate(sediment_TRS_umol_cc = sediment_TRS_umol_g * mean_dbd) %>%  #' not really necessary, since all samples are multiplied by the same DBD value...
   # add treatment and plant info
   left_join(plant_dat %>% select(plant_id, table, site, treatment_ph, treatment_nutrients))





