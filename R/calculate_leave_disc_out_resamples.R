calculate_leave_disc_out_resamples <- function(landscapes, workflow, buffer, radius) {
  
  tibble::tibble(
    buffer = buffer,
    radius = radius,
    rmse = {
      resample <- spatialsample::spatial_buffer_vfold_cv(
        landscapes[[1]], 
        v = Inf,
        buffer = buffer,
        radius = radius
      )
      
      current_model <- tune::fit_resamples(
        workflow, 
        resample,
        metrics = yardstick::metric_set(yardstick::rmse)
      )
      tune::collect_metrics(current_model)$mean
    }
  )
}