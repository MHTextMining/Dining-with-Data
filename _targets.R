library(targets)
library(tidyverse)
source("funs/get_data.R")
source("funs/def-models.R")
source("funs/def-recipes.R")
source("funs/utility.R")


tar_option_set(packages = c("tidyverse", "tidymodels", "tidytext", "SnowballC", "easystats", "textrecipes", "textfeatures", "stopwords", "glmnet", "parsnip", "discrim", "naivebayes", "doParallel", "xgboost"))

# Pipeline
list(
  tar_target(d_train, get_train_data()),
  tar_target(d_test, get_test_data()),
  
  #tar_target(rec0, def_rec0(d_train)),
  #tar_target(rec0_prepped, prep(rec0)),
  #tar_target(rec0_baked, bake(rec0_prepped, new_data = NULL)),
  
  tar_target(rec1, def_rec1(d_train)),
  tar_target(rec1_prepped, prep(rec1)),
  tar_target(rec1_baked, bake(rec1_prepped, new_data = NULL)),
  
  tar_target(rec2, def_rec2(d_train)),
  tar_target(rec2_prepped, prep(rec2)),
  tar_target(rec2_baked, bake(rec2_prepped, new_data = NULL)),
  
  
  
  tar_target(model_nb, def_m_nb()),
  tar_target(model_lasso, def_m_lasso()),
  tar_target(model_ridge, def_m_ridge()),
  tar_target(model_xgboost, def_m_boost()),
  
  
  tar_target(wf_set, workflow_set(preproc = list(rec1 = rec1, rec2 = rec2),
                                  models = list(model_nb = model_nb, model_lasso = model_lasso, model_ridge = model_ridge, model_xgboost = model_xgboost),
                                  cross = TRUE)),
  
  tar_target(wf_set_adjusted, wf_set %>%
               option_add(grid = 2, id = str_match(wf_set$wflow_id, ".*model_nb$")) %>%
               option_add(grid = 2, id = str_match(wf_set$wflow_id, ".*model_lasso$")) %>%
               option_add(grid = 2, id = str_match(wf_set$wflow_id, ".*model_ridge$")) %>%
               option_add(grid = 2, id = str_match(wf_set$wflow_id, ".*model_xgboost$"))),
  
  
  tar_target(wf_set_fit,
             workflow_map(wf_set_adjusted, fn = "tune_grid", resamples = vfold_cv(d_train, v = 2, strata = c1), verbose = TRUE)),
  
  tar_target(do_autoplot, autoplot(wf_set_fit)),
  tar_target(train_metrics, wf_set_fit %>%
               collect_metrics() %>%
               filter(.metric == "roc_auc") %>%
               arrange(-mean)),
  tar_target(best_wf_id, train_metrics %>% 
               slice_head(n = 1) %>% 
               pull(wflow_id)),
  tar_target(best_wf, wf_set_fit %>%
               extract_workflow(best_wf_id)),
  tar_target(best_wf_fit, wf_set_fit %>% 
               extract_workflow_set_result(best_wf_id)),
  tar_target(best_wf_finalized, best_wf %>% 
               finalize_workflow(select_best(best_wf_fit))),
  tar_target(last_fit,
             fit(best_wf_finalized, d_train)),
  tar_target(test_predicted,
             bind_cols(d_test, predict(last_fit, new_data = d_test)) %>% 
               mutate(c1 = factor(c1))),
  tar_target(test_metrics, test_predicted %>% 
               metrics(c1, .pred_class))
)



# tar_load()

# tar_make()

# tar_visnetwork()

# tar_meta(fields = warnings, complete_only = TRUE)