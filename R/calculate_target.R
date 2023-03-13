calculate_target <- function(ideal_rmse, bounds) {
  quantile(ideal_rmse, bounds) |> as.vector()
}
