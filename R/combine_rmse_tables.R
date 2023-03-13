combine_rmse_tables <- function(
    ideal_rmse, 
    target_rmse, 
    training_rmse, 
    random_resamples, 
    block_resamples, 
    disc_resamples, 
    clustered_resamples, 
    extra_buffered_loo
) {
  do.call(
    dplyr::bind_rows,
    list(
      data.frame(
        name = "ideal",
        rmse = ideal_rmse
      ),
      data.frame(
        name = "training",
        rmse = training_rmse
      ),
      cbind(
        random_resamples,
        name = "randomized"
      ),
      cbind(
        block_resamples,
        name = "block"
      ),
      cbind(
        clustered_resamples,
        name = "clustered"
      ),
      cbind(
        disc_resamples,
        name = "disc"
      ),
      cbind(
        extra_buffered_loo,
        name = "buffered_loo"
      )
    )
  ) |> 
    dplyr::mutate(
      good_rmse = rmse > target_rmse[[1]] & rmse < target_rmse[[2]],
      name = ifelse(
        name == "disc" & radius == 0,
        "buffered_loo", 
        name
      ),
      Method = dplyr::recode(
        name,
        ideal = "Ideal RMSE",
        clustered = "Clustered",
        disc = "LODO",
        block = "Blocked",
        randomized = "V-fold",
        buffered_loo = "BLO3CV",
        training = "Resubstitution"
      )
    )
}