# MAGIC %md ### Fitting a model use workflow function
# MAGIC `fit_model(tuned, wkflow, train_split, test_split, model, key_names, target_name = "target")`

# COMMAND ----------

fit_model <- function(
  tuned,
  wkflow,
  train_split,
  test_split = NULL,
  model,
  key_names,
  target_name = "target"
) {
  # Fits the best model based on the tuning grid results.
  #
  # Args:
  #   tuned:        The tuned object.
  #   wkflow:       The workflow.
  #   train_split:  The data to use for training the model.
  #   test_split:   The data to use for predictions.
  #   model:        The name of the model.
  #   key_names:    The key names in the data.
  #   target_name:  The target name in the data.
  #
  # Returns:
  #   A list containing:
  #     - preds:    The predictions from the train and test split.
  #     - metrics:  The train and test metrics along with the parameters for the best model.
  #     - fit:      The fitted best model.
  
  # Account for no test data
  if (is.null(test_split)) {
    test_split <- train_split[FALSE, ]
  }
  
  # Select best model
  best_mod <- tuned %>%
    select_best("rmse")

  final_wf <- wkflow %>%
    finalize_workflow(best_mod)

  # Fit final best model
  final_fit <- final_wf %>%
    fit(data = train_split)

  # Set up data for predictions
  dat <- rbind(
    train_split %>% mutate(split = "train"),
    test_split %>% mutate(split = "test")
  )
  
  varname <- paste0(model,"_pred")
  
  # Predictions
  pred <- final_fit %>%
    predict(new_data = dat) %>% bind_cols(dat, .) %>%
    select(all_of(key_names), split, all_of(target_name), !!varname := .pred)

  # Metrics
  metrics <- pred %>%
    group_by(split) %>%
    metrics(truth = target, estimate = !!varname) %>%
    select(-.estimator) %>%
    filter(.metric != "mae") %>%
    mutate(name = paste(split, .metric, sep = "_"), .keep = "unused") %>%
    pivot_wider(
      names_from = name,
      values_from = .estimate
    ) %>%
    mutate(
      model = model,
      best_sub_model = best_mod %>% pull(.config),
      param = jsonlite::toJSON(best_mod %>% select(-.config))
    ) %>%
    select(model, everything())
  
  # Output
  return(list(
    pred = pred,
    metrics = metrics, 
    fit = final_fit
  ))
}
