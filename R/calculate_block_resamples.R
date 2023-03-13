calculate_block_resamples <- function(landscapes, workflow, cellsize, buffer) {
  
  actual_size <- switch(
    cellsize,
    "1/100" = 0.1,
    "1/64" = 0.125,
    "1/36" = 0.167,
    "1/25" = 0.2,
    "1/16" = 0.25,
    "1/9" = 0.334,
    "1/4" = 0.5,
    "1/2" = c(0.5, 1)
  )
  
  tibble::tibble(
    buffer = buffer,
    cellsize = cellsize,
    rmse = purrr::map_dbl(
      1:length(landscapes),
      \(i) {
        resample <- spatialsample::spatial_block_cv(
          landscapes[[i]], 
          v = Inf,
          buffer = buffer,
          cellsize = actual_size
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