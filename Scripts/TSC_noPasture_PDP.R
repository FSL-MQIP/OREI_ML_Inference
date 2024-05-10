set.seed(1)
TSC_split= initial_split(TSC_noPasture, prop = 501/502, strata = logConc)
TSC_train = training(TSC_split)
TSC_test = testing(TSC_split)

TSC_recipe = recipe(logConc ~ ., data = TSC_noPasture) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>%
  step_pca(c("tempmax","tempmin","temp","humidity","precip","precipcover","windgust",      
             "windspeed","solarradiation","tempmax_1d","tempmin_1d","temp_1d","humidity_1d",
             "precip_1d", "precipcover_1d", "windgust_1d", "windspeed_1d", "solarradiation_1d","tempmax_2d",
             "tempmin_2d","temp_2d", "humidity_2d","precip_2d","precipcover_2d","windgust_2d","windspeed_2d",
             "solarradiation_2d", "tempmax_3d","tempmin_3d", "temp_3d",  "humidity_3d", "precip_3d",
             "precipcover_3d", "windgust_3d", "windspeed_3d","solarradiation_3d"), num_comp = 5) %>%
  step_dummy(all_nominal_predictors(), one_hot = FALSE) %>% 
  step_nzv(all_predictors(), freq_cut = 85/15) 

rf_spec = rand_forest(
  trees = 1000,
  min_n = tune(),
  mtry = tune()) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")


dimension = TSC_recipe %>% prep() %>% bake(NULL) %>% dim()
nVar = dimension[2]

rf_grid = grid_max_entropy(
  min_n(c(1L, 40L)),
  mtry(c(1L, nVar)),
  size = 40)

TSC_wf = workflow() %>% 
  add_recipe(TSC_recipe) %>% 
  add_model(rf_spec)

TSC_folds = vfold_cv(TSC_train, strata = logConc, v = 3)

TSC_rs = tune_grid(
  TSC_wf,
  TSC_folds,
  grid = rf_grid,
  metrics = metric_set(rsq),
  control = control_grid(verbose = FALSE)
)

#best_rs = show_best(TSC_rs, metric = "rsq")

choose_tree = TSC_rs %>% select_best(metric = "rsq") 

#extract_fit_parsnip(TSC_rs)

final_model = TSC_wf %>% 
  finalize_workflow(choose_tree) %>% 
  last_fit(TSC_split)

final_model$.workflow[[1]]

TSC_fit = TSC_wf %>% fit(data = TSC_noPasture)

imp_spec = rf_spec %>% 
  finalize_model(select_best(TSC_rs, "rsq")) %>% 
  set_engine("ranger", importance = "permutation")

vip_plot = workflow() %>% 
  add_recipe(TSC_recipe) %>% 
  add_model(imp_spec) %>% 
  fit(TSC_train) %>% 
  extract_fit_parsnip() %>% 
  vip(geom = "point", num_features = 15)
vip_plot

vip_features = vip_plot$data$Variable
vip_features

TSC_explainer = explain_tidymodels(
  final_model$.workflow[[1]],
  data = TSC_noPasture,
  y = TSC_noPasture$logConc,
  verbose = F
)


numVar = c("CowNum", "CertYear", "PplNumPerWk", "PplNumPerShift", "SPCvar", "FullEmpNum",
           "NonFamEmpNum", "StockDen")
catVar = c("CowHoldClean", "CowWaitMilk", "GloveFreq", "ClipFlame", "PartEmp")

numPlots = list()
for (i in 1:length(numVar)) {
  profile = model_profile(TSC_explainer, N = NULL, variables = numVar[i])
  
  pdp = ggplot_pdp(profile, !!as.name(numVar[i])) +
    labs(x = numVar[i],
         y = "logConc",
         color = NULL)
  
  file_name = paste0("Figure/Partial dependence plots/TSC/TSC_noPasture_pdp_", numVar[i], ".tiff")
  #ggsave(file_name, height = 4, width = 4, units = "in", dpi = "retina")
  
  numPlots[[i]] = pdp
  
}

numPlots_grid = gridExtra::grid.arrange(grobs = numPlots, ncol = 3)
ggsave("Figure/Partial dependence plots/TSC/NumVars_TSC_noPasture.pdf", numPlots_grid, 
       height = 4, width = 8, units = "in", dpi = "retina")


catPlots = list()
for (i in 1:length(catVar)) {
  profile = model_profile(TSC_explainer, N = NULL, variables = catVar[i])
  
  pdp = ggplot_pdp_cat(profile, !!as.name(catVar[i])) +
    labs(x = catVar[i],
         y = "logConc",
         color = NULL)+
    theme(axis.text.x = element_text(size = 4))
  
  file_name = paste0("Figure/Partial dependence plots/TSC/TSC_noPasture_pdp_", catVar[i], ".tiff")
  #ggsave(file_name, height = 4, width = 4, units = "in", dpi = "retina")
  
  catPlots[[i]] = pdp
  
}

catPlots_grid = gridExtra::grid.arrange(grobs = catPlots, ncol = 3)
ggsave("Figure/Partial dependence plots/TSC/CatVars_TSC_noPasture.pdf", catPlots_grid,
       height = 4, width = 8, units = "in", dpi = "retina")