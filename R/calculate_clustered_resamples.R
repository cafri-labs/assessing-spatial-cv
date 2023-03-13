calculate_clustered_resamples <- function(landscapes, workflow, v, cluster_function, buffer) {
  
  tibble(
    cluster_function = cluster_function,
    v = v,
    buffer = buffer,
    rmse = purrr::map_dbl(
      1:length(landscapes),
      \(i) {
        resample <- spatialsample::spatial_clustering_cv(
          landscapes[[i]], 
          v = v,
          cluster_function = cluster_function,
          buffer = buffer
        )
        
        current_model <- tune::fit_resamples(
          workflow, 
          resample,
          metrics = yardstick::metric_set(yardstick::rmse)
        )
        tune::collect_metrics(current_model)$mean
        
      }
    )
  )
  
}