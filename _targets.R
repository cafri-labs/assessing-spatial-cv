# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(future)
library(future.callr)
plan(callr)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c("tibble", "sf"),
  format = "rds"
)

# Load the R scripts with your custom functions:
lapply(list.files("R", full.names = TRUE, recursive = TRUE), source)

list(
  tar_target(
    name = landscapes,
    command = make_simulated_landscapes(n.sims = 100)
  ),
  tar_target(
    name = autocorrelation_ranges,
    command = calculate_autocorrelation_range(landscapes, workflow)
  ),
  tar_target(
    name = v,
    command = c(2, 5, 10, 20)
  ),
  tar_target(
    name = buffer,
    command = seq(0, 0.3, 0.03)
  ),
  tar_target(
    name = extra_buffer,
    command = seq(0.33, 0.48, 0.03)
  ),
  tar_target(
    name = radius,
    command = seq(0, 0.3, 0.03)
  ),
  tar_target(
    name = cellsize,
    command = c("1/100", "1/64", "1/36", "1/25", "1/16", "1/9", "1/4", "1/2")
  ),
  tar_target(
    name = cluster_function,
    command = c("kmeans", "hclust")
  ),
  tar_target(
    name = blocking_method,
    command = c("random", "snake", "continuous")
  ),
  tar_target(
    name = workflow,
    command = create_rf_workflow()
  ),
  tar_target(
    name = ideal_rmse,
    command = calculate_ideal(landscapes, workflow)
  ),
  tar_target(
    name = training_rmse,
    command = calculate_training(landscapes, workflow)
  ),
  tar_target(
    name = target_rmse,
    command = calculate_target(ideal_rmse, c(0.05, 0.95))
  ),
  tar_target(
    name = all_rmse,
    command = combine_rmse_tables(
      ideal_rmse, 
      target_rmse, 
      training_rmse, 
      random_resamples, 
      block_resamples, 
      disc_resamples,
      clustered_resamples,
      extra_buffered_loo
    )
  ),
  tar_target(
    name = prop_good,
    command = proportion_all_runs_in_good(all_rmse)
  ),
  tar_target(
    name = random_resamples,
    command = calculate_random_resamples(landscapes, workflow, v),
    pattern = map(v)
  ),
  tar_target(
   name = block_resamples,
   command = calculate_block_resamples(landscapes, workflow, cellsize, buffer),
   pattern = cross(cellsize, buffer)
  ),
  tar_target(
    name = block_resamples_with_v,
    command = calculate_block_resamples_with_v(landscapes, workflow, cellsize, buffer = NULL, blocking_method = blocking_method),
    pattern = cross(cellsize, blocking_method)
  ),
  tar_target(
    name = clustered_resamples,
    command = calculate_clustered_resamples(landscapes, workflow, v, cluster_function, buffer),
    pattern = cross(v, cluster_function, buffer)
  ),
  tar_target(
    name = disc_resamples,
    command = calculate_leave_disc_out_resamples(landscapes, workflow, buffer, radius),
    pattern = cross(landscapes, buffer, radius)
  ),
  tar_target(
    name = extra_buffered_loo,
    command = calculate_leave_disc_out_resamples(landscapes, workflow, extra_buffer, radius = 0),
    pattern = cross(landscapes, extra_buffer)
  ),
  tar_quarto(
    name = paper,
    packages = c("dplyr", "kableExtra", "ggplot2", "svglite", "patchwork")
  )
)
