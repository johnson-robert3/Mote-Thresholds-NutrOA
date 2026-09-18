# Seagrass Threshold Responses to Nutrients and OA

Investigating seagrass threshold responses to co-occurring nutrient enrichment and ocean acidification



### Mortality/Survivorship

For determining % survivorship (for each species individually): 
- Find all plant IDs that still had living seagrass at end of acclim. period (wk2)
- At 6 wks and 9 wks, calculate the proportion of those plant IDs that still had living seagrass
   - For survivorship at 9 wks, need to account for the plants that were harvested for genetic analysis at 6 wks (n=38 plants; 19 Ambient and 19 OA). 
      These plants are missing at 9 wks, but this is not mortality due to the experiment. Calculate survivor proportion based on the number of plants 
      that were alive at 2 wks minus those plants that were harvested at 6 wks.

To evaluate differences between treatments at specific time points
- Use a GLM with a binomial distribution, using binomial data to designate each plant
   as alive or dead (1 vs 0). Calculating percent survival (or mortality) results in a single value for each treatment, which can't be used for statistical
   comparisons. Use the 'hw_surv' and 'tt_surv' dfs, which already have this binomial data (alive_w0:alive_w9).
   - Treatments didn't all start with the same number of plants though; think about how to address this in the GLM. Just comparing the total numbers of living
      plants at a given time point won't be accurate if the treatments didn't start with the same number of plants. 



### Morphometry

For leaf length: only using plant IDs with living seagrass at the time point of interest (mortality not taken into account) (i.e., just using 'morphometry' df)



### Blade structure

Number of shoots, and blades-per-shoot, come from the 'leaf and shoot counts' dataset (counted on more plants than from the morph dataset)



### Epiphytes

Relate epiphyte load (i.e., dry mass per plant ID) to a structural parameter (e.g., leaf length, blade area, or shoot biomass) to standardize across plants.



