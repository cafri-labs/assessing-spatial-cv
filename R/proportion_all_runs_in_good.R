proportion_all_runs_in_good <- function(all_rmse) {
  all_rmse |> 
    dplyr::group_by(Method) |> 
    dplyr::summarise(proportion_good = mean(good_rmse))
}