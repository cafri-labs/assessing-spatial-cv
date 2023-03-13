calculate_ideal <- function(landscapes, workflow) {
  
  vapply(
    1:length(landscapes),
    \(i) {
      validation_data <- do.call(rbind, landscapes[-i])
      current_model <- workflows:::fit.workflow(
        workflow, 
        landscapes[[i]]
      )
      validation_data$predictions <- predict(current_model, validation_data)
      yardstick::rmse_vec(validation_data$y, validation_data$predictions$.pred)
    },
    numeric(1)
  )

}