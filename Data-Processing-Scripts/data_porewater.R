#~~~
# Porewater data
#
# By: R. Johnson
#~~~



#--
# Porewater Sulfide 
#--

#- Total sulfide concentration in original porewater
porewater_plant <- porewater %>%
   # create sample_id column to match spec data for joining
   mutate(sample_id = paste0("MSI25-", table, "-", plant_id, "-", week)) %>%
   relocate(sample_id) %>%
   rename(notes_sample = notes) %>%
   # add sulfide concentration from spec data
   left_join(sulfide %>%
                 select(sample_id, scint_S_uM, flag_spec = flag, notes_spec = notes)) %>%
   # calculate porewater total dissolved sulfide concentration (units = uM)
   mutate(
      # total S in vial (concentration times total aqueous volume) (units = umol)
      scint_S_umol = scint_S_uM * ((h2s_vol_ml + h2s_znac_vol_ml) / 1000),
      # concentration of S in porewater (units = uM)
      porewater_S_uM = scint_S_umol / (h2s_vol_ml / 1000)) %>%
   # add treatment and plant info
   left_join(plant_dat %>% select(plant_id, treatment_ph, treatment_nutrients, site)) %>%
   # remove the outlier sulfide data point
   #' sample T2-L152-w2 has a PW conc. of ~5000 uM (5 mM)  (next highest is < 1000 uM)
   mutate(porewater_S_uM = replace_when(porewater_S_uM, porewater_S_uM > 4000 ~ NA_real_))


#- Estimating concentration of hydrogen sulfide (H2S) fraction

# Function to calculate H2S concentration using measured total sulfide, temperature, salinity, and pH
calc_h2s_frac <- function(.sulfide_uM, .temp_c, .salinity_ppm, .pH) {
   
   # Equations from MacLeod et al. 2023 (Aquatic Botany) and Millero 1986 (Marine Chemistry)
   
   # from MacLeod et al. 2023 (section 2.3.1): 
   
      # [H2S]/[ST] = 1 / (10^(pH - pK) + 1)
      
         # ST = total dissolved sulfide concentration
         # pK = pK at in situ temperature and salinity from equation 17 in Millero 1986
   
   
   # from Millero 1986: 
   
      # temp in Kelvin
      temp_k = 273.15 + .temp_c
      
      # constants (from Almgren et al. 1976, as cited in Millero 1986)
      A = -0.2391 + (35.685 / temp_k)  # eq. 18
      B = 0.0109 - (0.3776 / temp_k)   # eq. 19
      
      # eq. 12 (calculate pK at in situ temperature)
      pk1 = 32.55 + (1519.44 / temp_k) - (15.672 * (log10(temp_k))) + (0.02722 * temp_k)
      
      # eq. 17 (calculate pK at in situ temp and salinity)
      pK = pk1 + (A * .salinity_ppm^0.5) + (B * .salinity_ppm)
   
   
   #- Calculations
   
   # from MacLeod et al. 2023
   fraction = 1 / (10^(.pH - pK) + 1)
   
   conc_h2s = .sulfide_uM * fraction
   
   # result
   return(conc_h2s)

}

   #' have total sulfide and pH measured from porewater samples
   #' need to get Temperature data for the tables from the HOBO time series
   #' get salinity data from the CarbCHEM dataset

# temperature in tables
tmp1 <- temp_dat %>%
   filter(date %in% c(porewater %>% pull(date) %>% unique())) %>% 
   select(-date_time) %>%
   filter(between(.$time, "06:00:00", "12:00:00")) %>%
   summarize(temp_c = mean(temperature_c), .by = c(table, date)) %>%
   mutate(treatment_ph = case_when(table %in% c('T1', 'T2') ~ "OA",
                                   table %in% c('T4', 'T5') ~ "ambient"))

# salinity in tables
tmp2 <- caos_chem %>%
   filter_out(is.na(treatment_ph)) %>%
   select(date, treatment_ph, ysi_salinity_mg.L)

# combine table temp and salinity
tmp3 <- tmp1 %>%
   # join CAOS data by closest date to porewater sampling date
   left_join(tmp2, by = join_by(treatment_ph, closest(x$date <= y$date))) %>%
   select(-date.y) %>%
   rename(date = date.x, salinity = ysi_salinity_mg.L)


# Calculate H2S concentration in samples
porewater_plant <- porewater_plant %>%
   # add temp and salinity to porewater dataset
   left_join(tmp3, by = c('table', 'date', 'treatment_ph')) %>%
   # calc H2S using new function
   mutate(pw_h2s_uM = calc_h2s_frac(porewater_S_uM, temp_c, salinity, pH))



#- Calculate mean and SE for each treatment over time
porewater_trt <- porewater_plant %>%
   # lengthen the df; create a 'parameter' column of variables and a 'result' column of values and drop missing values 
   pivot_longer(cols = c(pH, porewater_S_uM, pw_h2s_uM), names_to = 'parameter', values_to = 'result', values_drop_na = TRUE) %>% 
   # calculate treatment means, errors, and sample sizes
   group_by(treatment_ph, treatment_nutrients, week, parameter) %>% 
   summarize(mean = mean(result, na.rm=TRUE),
             sd = sd(result, na.rm=TRUE),
             se = se(result),
             n = n(),
             .groups = 'drop')
   # treatment mean and SE
   # summarize(across(c(pH, porewater_S_uM, pw_h2s_uM), list(mean = ~mean(., na.rm=TRUE), sd = ~sd(., na.rm=TRUE), se = se), .names = "{.fn}_{.col}"), 
   #           .by = c(treatment_ph, treatment_nutrients, week))



# clean up
rm(tmp1, tmp2, tmp3)

