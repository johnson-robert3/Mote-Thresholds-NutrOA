#~~
# Figures for Thalassia rhizome carbohydrate results
#
# By: R. Johnson
#~~


ggplot(carbs_plant) +
   geom_point(aes(x = treatment_nutrients, y = result, color = treatment_ph, shape = week), 
              position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75), size = 2, alpha=0.6) +
   facet_wrap(~analyte, scales = 'free_y') +
   theme_classic()
   


# look at TNC, starch, and soluble carbs
# facet: carb variables as rows, week as cols
ggplot(carbs_plant %>% filter(analyte %in% c("TNC", "starch", "soluble_carbs")), 
       aes(x = treatment_nutrients, y = result, color = treatment_ph, shape = treatment_nutrients)) +
   geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5), size = 3, alpha=0.6) +
   facet_grid(rows = vars(analyte), cols = vars(week), scales = 'free_y') +
   scale_color_manual(values = ph_col_env) +
   theme_bw() 

   #' potential pattern: in wk9, there is less variability among samples (n=3) for the 0g and ambient pH treatment for all three carb variables
   #' variability is higher w/ OA in the 0g treatment, and variability is higher in the 10g treatment at both ambient and OA pH levels
   #' 
   #' no other clear patterns in data (though sample replicates are very limited)


