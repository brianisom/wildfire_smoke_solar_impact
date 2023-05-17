mt_panel.fe <- plm(
  log(total_gen) ~ total_pm + factor(year) + factor(month),
                     data = census_panel_2015,
                     model = "within",
                     effect = "individual")

summary(mt_panel.fe, cluster = "geoid")

mt_panel_large_gen.fe <- plm(
  log(total_gen) ~ total_pm + factor(year) + factor(month),
                   data = cp_large_cap,
                   model = "within",
                   effect = "individual")

summary(mt_panel_large_gen.fe, cluster = "geoid")


mt_panel_full.fe <- plm(
  log(total_gen) ~ total_pm + factor(year) + factor(month),
                   data = census_panel,
                   model = "within",
                   effect = "individual")

summary(mt_panel_full.fe, cluster = "geoid")
coeftest(mt_panel_full.fe, vcov. = vcovHC, type  = "HC1")

mt_panel.smoke.fe <- plm(
  log(total_gen) ~ smoke_days + factor(year) + factor(month),
                   data = census_panel,
                   model = "within",
                   effect = "individual")

summary(mt_panel.smoke.fe, cluster = "geoid")
coeftest(mt_panel.smoke.fe, vcov. = vcovHC, type  = "HC1")




