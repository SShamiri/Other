# MAGIC %md ### MLflow related functions

# COMMAND ----------

# MAGIC %md
# MAGIC `log_workflow_parameters(workflow, prefix = "")`

# COMMAND ----------

log_workflow_parameters <- function(workflow, prefix = "") {
  # Extracts the parameters from a tidymodels workflow object and logs them to mlflow.
  #
  # Args:
  #   workflow: The final workflow object with the tuned parameters.
  #   prefix:   Optional prefix to add at the start of the parameter name. Defaults to no prefix.
  #
  # Returns:
  #   workflow: The input workflow object.

  # Extract the parameter names and values
  spec <- workflows::pull_workflow_spec(workflow)
  parameter_names <- names(spec$args)
  if (prefix != "") {
    parameter_names <- paste0(prefix, "_", parameter_names)
  }
  parameter_values <- lapply(spec$args, rlang::get_expr)

  # Log each parameter
  for (i in seq_along(spec$args)) {
    parameter_name <- parameter_names[[i]]
    parameter_value <- parameter_values[[i]]
    if (!is.null(parameter_value)) {
      mlflow_log_param(parameter_name, parameter_value)
    }
  }

  # Return the input workflow
  workflow
}

# COMMAND ----------

# MAGIC %md
# MAGIC `log_metrics(metrics, estimator = "standard")`

# COMMAND ----------

log_metrics <- function(metrics, estimator = "standard") {
  # Extracts the metrics from a tidymodels yardstick tibble and logs them to mlflow.
  #
  # Args:
  #   metrics: The metrics tibble from yardstick.
  #
  # Returns:
  #   metrics: The input metrics tibble.

  # Log each metric
  metrics %>%
    filter(.estimator == estimator) %>%
    pmap(
      function(.metric, .estimator, .estimate) {
        mlflow_log_metric(.metric, .estimate)
      }
    )

  # Return the input metrics
  metrics
}
