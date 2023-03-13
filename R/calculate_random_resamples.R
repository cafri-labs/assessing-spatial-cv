calculate_random_resamples <- function(landscapes, workflow, v) {
  
  tibble(
    v = v,
    rmse = purrr::map_dbl(
      1:length(landscapes),
      \(i) {
        resample <- rsample::vfold_cv(landscapes[[i]], v = v)
        
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
