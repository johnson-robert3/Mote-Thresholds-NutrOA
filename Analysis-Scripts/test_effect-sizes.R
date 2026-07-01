

# testing different 'Effect sizes' for Halodule blades-per-shoot

# 1. calculate effect size metrics
#    - mean diff, Hedges' g, log ratio

# set up data


#' another alternative method; no need to use nest() and map(), can just lengthen and then compute on the df, grouping by the parameter variable
#' need to make dfs for summary stats and for effect sizes separately this way, though


# summary stats
#' this way is now done correctly with a lengthened dataset directly for the 'shoots_trt' df


# effect sizes
tmp.e <- bind_rows(shoots_trt, morph_trt, biomass_trt) %>%  # can combine all 3 struc. trt dfs, b/c they are same format now that they're lengthened
   group_by(species, treatment_nutrients, week, parameter) %>%
   summarize(
      # mean difference
      mean_diff = mean[treatment_ph == "OA"] - mean[treatment_ph == "ambient"],
      # hedges' g
      hedges_g = mean_diff / sqrt(((sd[treatment_ph == "OA"])^2 + (sd[treatment_ph == "ambient"])^2) / 2),
      # log ratio
      log_ratio = log(mean[treatment_ph == "OA"]) - log(mean[treatment_ph == "ambient"]),
      # groups
      .groups = 'drop')





# 2. figures

ggplot(tmp.e %>% mutate(treatment_nutrients = factor(treatment_nutrients, levels = c('pulsed', '0g', '2g', '4g', '7g', '10g'))) %>%
          filter(treatment_nutrients!='pulsed') %>%
          filter(parameter == 'bps')) +
   # mean diff (black)
   geom_point(aes(x = treatment_nutrients, y = mean_diff), size = 2, color = "black") +
   geom_line(aes(x = treatment_nutrients, y = mean_diff, group=1), color = "black") +
   # hedges' g (blue)
   geom_point(aes(x = treatment_nutrients, y = hedges_g), size = 2, color = "blue") +
   geom_line(aes(x = treatment_nutrients, y = hedges_g, group=1), color = "blue") +
   # log ratio (red)
   geom_point(aes(x = treatment_nutrients, y = log_ratio), size = 2, color = "red") +
   geom_line(aes(x = treatment_nutrients, y = log_ratio, group=1), color = "red") +
   # styling
   labs(x = "Nutrient treatment", y = "Effect size") +
   theme_bw() +
   facet_grid(species ~ week, scales = "free_y")










