#~~
# Process rhizome carbohydrate results
#
#~~


#' total non-structural carbohydrates [TNC] = total_glucose + sucrose + fructose
#' starch = (total_glucose - glucose) * 0.9
#' soluble carbohydrates = glucose + sucrose + fructose



#--
# Carbohydrates
#--

carbs <- rhiz_carbs %>%
   # lengthen dataset
   rename(totalglucose_percent = total_glucose_percent) %>%
   pivot_longer(cols = contains("percent"), names_to = c("analyte", "units"), names_sep = "_", values_to = "result") %>%
   # add data flag for samples below detection limit
   mutate(data_flag = case_when(str_detect(result, pattern="<") ~ "DL",
                                .default = NA_character_)) %>%
   # replace flagged samples with half the DL 
   mutate(result = str_remove(result, pattern="<"),
          result = as.numeric(result),
          result = replace_when(result, data_flag == "DL" ~ result / 2)) %>%
   # the note about vial 16 being out of range only applies to the Sucrose result (it was the only sample below DL for sucrose)
   mutate(analysis_notes = replace(analysis_notes, sample_id=='T1-A122-w6' & analyte!='sucrose', NA_character_)) %>%
   relocate(analysis_notes, .after = last_col())
   
# calculate soluble carbohydrates
carbs <- carbs %>%
   bind_rows(carbs %>% 
                filter(analyte %in% c('glucose', 'sucrose', 'fructose')) %>%
                pivot_wider(id_cols = c(sample_id, units), names_from = 'analyte', values_from = 'result') %>%
                mutate(soluble_carbs = glucose + sucrose + fructose) %>%
                select(sample_id, units, soluble_carbs) %>%
                pivot_longer(cols = soluble_carbs, names_to = "analyte", values_to = "result")) %>%
   mutate(analyte = replace(analyte, analyte=="totalglucose", "total_glucose"))


# add plant ID and treatment info
carbs_plant <- carbs %>%
   # split sample_id column for joining
   separate_wider_delim(sample_id, delim="-", names = c("table", "plant_id", "week")) %>%
   left_join(plant_dat %>% select(plant_id, treatment_ph, treatment_nutrients, site))




