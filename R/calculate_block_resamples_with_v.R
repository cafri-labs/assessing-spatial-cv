calculate_block_resamples_with_v <- function(landscapes, workflow, cellsize, buffer, blocking_method) {
  
  actual_size <- switch(
    cellsize,
    "1/100" = list(0.1, c(2, 4, 9, 16, 25, 36, 64)),
    "1/64" = list(0.125, c(2, 4, 9, 16, 25, 36)),
    "1/36" = list(0.167, c(2, 4, 9, 16, 25)),
    "1/25" = list(0.2, c(2, 4, 9, 16)),
    "1/16" = list(0.25, c(2, 4, 9)),
    "1/9" = list(0.334, c(2, 4)),
    "1/4" = list(0.5, c(2))
  )
  
  purrr::map_dfr(
    actual_size[[2]],
    \(v) {
      tibble::tibble(
        buffer = buffer,
        cellsize = cellsize,
        v = v,
        method = blocking_method,
        rmse = purrr::map_dbl(
          1:length(landscapes),
          \(i) {
            resample <- spatialsample::spatial_block_cv(
              landscapes[[i]], 
              v = v,
              buffer = buffer,
              method = blocking_method,
              cellsize = actual_size[[1]]
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
  )
  
  

}