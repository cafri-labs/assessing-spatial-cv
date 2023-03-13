create_rf_workflow <- function() {
  workflows::workflow(
    spec = parsnip::rand_forest(
      mode = "regression", 
      mtry = NULL, 
      trees = 500, 
      min_n = 5, 
      engine = "ranger"
    )
  ) |> 
    workflows::add_formula(y ~ x.2 + x.3 + x.6 + x.7 + x.8 + x.9 + x.10)
}