

# testing different 'Effect sizes' for Halodule blades-per-shoot

# 1. calculate effect size metrics
#    - mean diff, Hedges' g, log ratio

# set up data

tmp <- shoots_plant %>%
   # remove wks 3 and 8 (only dead/missing plants recorded these weeks; number of blades/shoots were not counted if plant was present)
   filter_out(week %in% c('w3', 'w8')) %>%
   # replace 0's with NA, so that missing plants are excluded from mean values
   mutate(across(c(shoot_count, leaf_count, bps), ~replace_values(., 0 ~ NA_real_))) %>% 
   # treatment means
   summarize(across(c(shoot_count, leaf_count, bps), list(mean=~mean(., na.rm=TRUE), se=se, sd=~sd(., na.rm=TRUE)), .names="{.fn}_{.col}"), 
             n=n(),
             .by=c(treatment_ph, treatment_nutrients, week, species)) %>%
   #
   filter(week == "w9", species == "Hw") %>%
   # group_by(treatment_nutrients) %>%
   summarize(
      # mean difference
      mean_diff = mean_bps[treatment_ph == "OA"] - mean_bps[treatment_ph == "ambient"],
      # hedges' g
      hedges_g = mean_diff / sqrt(((sd_bps[treatment_ph == "OA"])^2 + (sd_bps[treatment_ph == "ambient"])^2) / 2),
      # log ratio
      log_ratio = log(mean_bps[treatment_ph == "OA"]) - log(mean_bps[treatment_ph == "ambient"]),
      # groups
      .by = c(species, week, treatment_nutrients))




# 2. figures

ggplot(tmp %>% mutate(treatment_nutrients = factor(treatment_nutrients, levels = c('pulsed', '0g', '2g', '4g', '7g', '10g'))) %>%
          filter(treatment_nutrients!='pulsed')) +
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










