###############################################################################

# Panel Analysis

###############################################################################

# Set up models-----------------------------------------------------------------

# Basic OLS
gen_mod1 <- feols(log(total_gen) ~ total_pm,
               data = census_panel,
               cluster = ~ geoid)

# Add a variable for plant capacity
gen_mod2 <- feols(log(total_gen) ~ total_pm + log(capacity_mw),
                  data = census_panel,
                  cluster = ~ geoid)

# Add climate variables
gen_mod3 <- feols(log(total_gen) ~ total_pm + log(capacity_mw) + tmin + 
                    tmax + tdmean + precip,
               data = census_panel,
               cluster = ~ geoid)

# Include individual fixed effects 
gen_mod4 <- feols(log(total_gen) ~ total_pm + log(capacity_mw) + tmax 
                  + tmin + tdmean + precip | geoid,
                  data = census_panel,
                  cluster = ~ geoid)

# Include time fixed effects 
gen_mod5 <- feols(log(total_gen) ~ total_pm + log(capacity_mw) + tmax
                  + tmin + tdmean + precip | month,
                  data = census_panel,
                  cluster = ~ geoid)

# Individual and time fixed effects 
gen_mod6 <- feols(log(total_gen) ~ total_pm + log(capacity_mw) + tmax + tmin 
                  + tdmean + precip | geoid + month + geoid^month,
                  data = census_panel,
                  cluster = ~ geoid)

# log capacity 
gen_mod7 <- feols(log(total_gen) ~ total_pm + log(capacity_mw) + tmax + tmin 
                  + tdmean + precip | geoid + month + geoid^month,
                  data = census_panel,
                  cluster = ~ geoid)

# Produce tables---------------------------------------------------------------

regression_table_1 <- etable(
  gen_mod1, 
  gen_mod2, 
  gen_mod3, tex = TRUE)

regression_table_2 <- etable(
  gen_mod4, 
  gen_mod5, 
  gen_mod6, tex = TRUE)

census_panel_sum <- as.data.frame(census_panel[,c(5:11,15)])

panel_summary <- stargazer(census_panel_sum, type = "text")

fwrite(panel_summary, "data/panel_summary.csv")

ggplot(census_panel, aes(x = log(total_gen), y = total_pm)) +
  geom_point(data = census_noYear, color = "grey85") +
  geom_point() +
  facet_wrap(~ state_fips)

census_panel2016 <- census_panel %>% 
  filter(year >=2016)

census_noYear <- census_panel[,-3]

census_panelmid <- census_panel %>%
  filter(capacity_mw >= 20)

gen_mod7 <- feols(log(total_gen) ~ total_pm + capacity_mw + tmax + tmin 
                              + tdmean + precip | geoid + geoid^month,
                              data = census_panel2016,
                              cluster = ~ geoid)
summary(gen_mod7)

fegen_mod6 <- plm(log(total_gen) ~ total_pm + capacity_mw + tmax + tmin 
                  + tdmean + precip,
                  data = census_panel,
                  model = "within")

regen_mod6 <- plm(log(total_gen) ~ total_pm + capacity_mw + tmax + tmin 
                  + tdmean + precip,
                  data = census_panel,
                  model = "random")

phtest(fegen_mod6, regen_mod6)
