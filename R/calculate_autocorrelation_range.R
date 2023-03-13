calculate_autocorrelation_range <- function(landscapes, workflow) {
  
  grd <- expand.grid(Y = seq(0, 1, len = 50), X = seq(0, 1, len = 50))
  
  purrr::map_dfr(
    landscapes, 
    function(land) {
      
      lm_model <- lm(workflow$pre$actions$formula$formula, data = land)
      rf_model <- workflows:::fit.workflow(workflow, land)
      
      lm_residuals <- predict(lm_model, land, type = "response") - land$y
      rf_residuals <- predict(rf_model, land)$.pred - land$y
      
      lm_variogram <- automap::autofitVariogram(
        lm_residuals ~ 1,
        sp::SpatialPointsDataFrame(grd, data.frame(resids = lm_residuals)),
        model = "Sph"
      )
      rf_variogram <- automap::autofitVariogram(
        rf_residuals ~ 1,
        sp::SpatialPointsDataFrame(grd, data.frame(resids = rf_residuals)),
        model = "Sph"
      )
      
      outcome_variogram <- automap::autofitVariogram(
        land$y ~ 1,
        sp::SpatialPointsDataFrame(grd, data.frame(resids = land$y)),
        model = "Sph"
      )
      
      tibble::tibble(
        lm_range = lm_variogram$var_model$range[2],
        rf_range = rf_variogram$var_model$range[2],
        outcome_range = outcome_variogram$var_model$range[2]
      )
      
    }
  )
  
}
