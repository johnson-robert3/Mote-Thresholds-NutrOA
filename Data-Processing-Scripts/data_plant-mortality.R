#~~~
# Calculate and view seagrass plant mortality/survivorship at intervals throughout the experiment
#
# By: R. Johnson
#~~~


# 1. Combine leaf/shoot count data with plant_ID data
plants <- leaf_counts %>% rename(count_notes = notes) %>% 
   left_join(plant_dat)


# 2. Lists of plant IDs

# all plant IDs used in experiment
plant_ids_w0 <- plants %>% pull(plant_id) %>% unique()

# plant IDs that were harvested for genetics at wk6
harvest_ids <- plant_dat %>% filter(str_detect(removal_notes, "genetics")) %>% pull(plant_id)


# 3. Calculate survivorship/mortality of each species at each time point

   #' There is additional mortality data for weeks 3 and 8 within the 'leaf_counts' dataset if ever desired. 
   #' In week 3, morphometry was measured on a subset of living plant IDs. In both weeks 3 and 8, all plant ID's were checked for mortality.
   #' If either species in a pot was dead/missing, its shoot/leaf count was recorded as '0', and if the other species was still alive, its 
   #'  its shoot/leaf count was left blank (NA). If both species for a plant ID were still alive, nothing was recorded, and that plant ID will
   #'  not have a row in the 'leaf_count' dataset for that week (except for the plants in week 3 that were measured for morphometry). 


#-- HALODULE ----

# Create vectors of living plant IDs at each time point

# Plant IDs with living Hw at wk 2
hw_w2_ids <- plants %>% filter(week == "w2" & Hw_blades > 0) %>% pull(plant_id)
# Plant IDs with living Hw at wk 6
hw_w6_ids <- plants %>% filter(week == "w6" & Hw_blades > 0) %>% pull(plant_id)
# Plant IDs with living Hw at wk 9
hw_w9_ids <- plants %>% filter(week == "w9" & Hw_blades > 0) %>% pull(plant_id)


#' Determine which Hw plant IDs were alive at wk 2 that were not harvested for genetics at wk 6 (to calculate survivorship of these at wk 9)
#'  However, can't just exclude all the plant IDs that were harvested in wk 6; for three of these plants, Hw died before wk 6,
#'   so those 3 plant IDs need to be retained and counted toward the survivorship at wk 9.
#'  Only exclude the genetics IDs that still had living Hw when they were harvested at wk 6 (b/c this wasn't mortality due to the experiment).
#'   - (it's actually only 2 plant IDs (A002, H051), b/c Hw was already dead at wk 2 in the third (A010))

   # view the plants harvested at wk 6 where  Hw was already dead
   plants %>% filter(week == 'w6' & plant_id %in% harvest_ids) %>% filter(Hw_blades == 0) %>% View
   
   # view the plants harvested at wk 6 where HW was still living
   plants %>% filter(week == 'w6' & plant_id %in% harvest_ids) %>% filter(Hw_blades > 0) %>% View

# so the final set of plant IDs from wk 2 to be used for calculating survivorship at wk 9 is...
hw_w2_no_genetics <- plants %>% filter(week == "w2" & Hw_blades > 0) %>% 
   filter(!(plant_id %in% 
               c(plants %>% filter(week == 'w6' & plant_id %in% harvest_ids) %>% filter(Hw_blades > 0) %>% pull(plant_id))
            )) %>% 
   pull(plant_id)


#- Halodule survivorship over time

   # survivorship at wk 6 relative to wk 2
   length(hw_w6_ids) / length(hw_w2_ids) * 100

   # survivorship at wk 9 relative to wk 2
   length(hw_w9_ids) / length(hw_w2_no_genetics) * 100

   #' - but these are just total, need to split it by treatments


# make a df denoting whether a plant ID was alive at time points
hw_surv <- plants %>%
   mutate(
      alive_w0 = 1,
      alive_w2 = if_else(plant_id %in% hw_w2_ids, 1, 0),
      alive_w6 = if_else(plant_id %in% hw_w6_ids, 1, 0),
      alive_w9 = if_else(plant_id %in% hw_w9_ids, 1, 0),
      w2_for_w9 = if_else(plant_id %in% hw_w2_no_genetics, 1, 0)) #%>%
   # filter(alive_w2 != 0)


# view survivorship by treatment
hw_surv %>%
   filter(week == "w2") %>%
   summarize(across(c(alive_w2:w2_for_w9), ~ sum(.)), .by = c(treatment_ph, treatment_nutrients)) %>%
   mutate(survive_w6_perc = alive_w6 / alive_w2 * 100,
          survive_w9_perc = alive_w9 / w2_for_w9 * 100) %>%
   View

   # view ambient ph and 2g treatment, why does w9 have higher survivorship than wk 6?
   hw_surv %>% filter(treatment_ph=="ambient" & treatment_nutrients=="2g") %>% arrange(plant_id, week) %>% View

   # view plants that are recorded as dead in wk 6 but then alive in wk 9
   hw_surv %>% filter(alive_w6==0 & alive_w9==1) %>% arrange(plant_id, week) %>% View
      #' L006 was accidentally not counted in wk 6 (and missing values are counted as dead in the code above)
      #' L171 had 0 blades in wk 6, and then 1 blade in wk 9; must have been missed in wk 6, or roots survived and a new blade grew by wk 9
      #' - just omit these two for survivorship counts


# df of Halodule survivorship
hw_surv <- hw_surv %>% filter_out(plant_id %in% c("L006", "L171"))

# view survivorship by treatment
hw_mort <- hw_surv %>%
   filter(week == "w2") %>%
   summarize(across(c(alive_w0:w2_for_w9), ~ sum(.)), .by = c(treatment_ph, treatment_nutrients)) %>%
   mutate(survive_w2_perc = alive_w2 / alive_w0 * 100,   # percent that survived initial acclimation period
          survive_w6_perc = alive_w6 / alive_w2 * 100,   # percent that survived the experimental period (relative to those living at wk 2)
          survive_w9_perc = alive_w9 / w2_for_w9 * 100)  # percent that survived to the end (relative to those living at wk 2, not including living plants harvested at wk 6)



#-- THALASSIA ----

# Create vectors of living plant IDs at each time point

# Plant IDs with living Tt at wk 2
tt_w2_ids <- plants %>% filter(week == "w2" & Tt_blades > 0) %>% pull(plant_id)
# Plant IDs with living Tt at wk 6
tt_w6_ids <- plants %>% filter(week == "w6" & Tt_blades > 0) %>% pull(plant_id)
# Plant IDs with living Tt at wk 9
tt_w9_ids <- plants %>% filter(week == "w9" & Tt_blades > 0) %>% pull(plant_id)


#' Determine which Tt plant IDs were alive at wk 2 that were not harvested for genetics at wk 6 (to calculate survivorship of these at wk 9)
#'  However, can't just exclude all the plant IDs that were harvested in wk 6; for five of these plants, Tt died before wk 6,
#'   so those 5 plant IDs need to be retained and counted toward the survivorship at wk 9.
#'  Only exclude the genetics IDs that still had living Tt when they were harvested at wk 6 (b/c this wasn't mortality due to the experiment).

   # view the plants harvested at wk 6 where Tt was already dead
   plants %>% filter(week == 'w6' & plant_id %in% harvest_ids) %>% filter(Tt_blades == 0) %>% View
   
   plants %>% filter(plant_id %in% harvest_ids) %>% filter(Tt_blades == 0) %>% arrange(plant_id, week) %>% View
      #' L163 was recorded as 0 Tt in wk 2 and wk 3, but then 5 Tt blades in wk 6; leaves fell off but must not have been dead, and then re-grew
   
   # view the plants harvested at wk 6 where Tt was still living
   plants %>% filter(week == 'w6' & plant_id %in% harvest_ids) %>% filter(Tt_blades > 0) %>% View
   
      # view IDs that are recorded as alive in wk 6, but were "dead" in wk 2
      setdiff(tt_w6_ids, tt_w2_ids)
      #' two plants: L163, H117
      #' both of these plants were recorded as having Tt blades in wk 6, but not in any other weeks
      #' - just omit these two for survivorship counts

# so the final set of plant IDs from wk 2 to be used for calculating survivorship at wk 9 is...
tt_w2_no_genetics <- plants %>% filter(week == "w2" & Tt_blades > 0) %>% 
   filter(!(plant_id %in% 
               c(plants %>% filter(week == 'w6' & plant_id %in% harvest_ids) %>% filter(Tt_blades > 0) %>% pull(plant_id))
            )) %>% 
   pull(plant_id)


#- Thalassia survivorship over time

   # survivorship at wk 6 relative to wk 2
   length(tt_w6_ids) / length(tt_w2_ids) * 100

   # survivorship at wk 9 relative to wk 2
   length(tt_w9_ids) / length(tt_w2_no_genetics) * 100

   #' - but these are just total, need to split it by treatments


# make a df denoting whether a plant ID was alive at time points
tt_surv <- plants %>%
   mutate(
      alive_w0 = 1,
      alive_w2 = if_else(plant_id %in% tt_w2_ids, 1, 0),
      alive_w6 = if_else(plant_id %in% tt_w6_ids, 1, 0),
      alive_w9 = if_else(plant_id %in% tt_w9_ids, 1, 0),
      w2_for_w9 = if_else(plant_id %in% tt_w2_no_genetics, 1, 0)) #%>%
   # filter(alive_w2 != 0)

# df of Thalassia survivorship 
# (remove the two plants identified above that were only recorded as alive at wk 6)
tt_surv <- tt_surv %>% filter_out(plant_id %in% c("L163", "H117"))


# view survivorship by treatment
tt_mort <- tt_surv %>%
   filter(week == "w2") %>%
   summarize(across(c(alive_w0:w2_for_w9), ~ sum(.)), .by = c(treatment_ph, treatment_nutrients)) %>%
   mutate(survive_w2_perc = alive_w2 / alive_w0 * 100,   # percent that survived initial acclimation period
          survive_w6_perc = alive_w6 / alive_w2 * 100,   # percent that survived the experimental period (relative to those living at wk 2)
          survive_w9_perc = alive_w9 / w2_for_w9 * 100)  # percent that survived to the end (relative to those living at wk 2, not including living plants harvested at wk 6)



#-- View the number of living plants at each time point for each species --#

   #' not sure this is needed/helpful; treatments didn't start with the same number of plants, so viewing numbers of survivors doesn't
   #' give information about treatment effects.
   #' more useful to use percent mortality data from the _mort dfs above

# Halodule
hw_surv %>% filter(week=="w2") %>% summarize(across(starts_with("alive"), sum))  # wk 9 sum is inaccurate, b/c includes harvest at wk 6

# Thalassia
tt_surv %>% filter(week=="w2") %>% summarize(across(starts_with("alive"), sum))  # wk 9 sum is inaccurate, b/c includes harvest at wk 6



