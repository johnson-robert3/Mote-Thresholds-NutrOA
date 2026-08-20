#~~
# Figures for rhizome carbohydrate results
#
# By: R. Johnson
#~~


ggplot(carbs_plant) +
   geom_point(aes(x = treatment_nutrients, y = result, color = treatment_ph, shape = week), 
              position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75), size = 2, alpha=0.6) +
   facet_wrap(~analyte, scales = 'free_y') +
   theme_classic()
   

