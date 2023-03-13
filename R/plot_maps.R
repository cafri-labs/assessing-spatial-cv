plot_maps <- function(landscape, width = 190, dpi = 600) {
  
  outcome <- cbind(
    sf::st_coordinates(sf::st_geometry(landscape)),
    outcome = landscape$y
  ) |> 
    as.data.frame() |> 
    dplyr::mutate(X = X * 100, Y = Y * 100) |> 
    ggplot(aes(X, Y, fill = outcome)) + 
    geom_tile(color = NA, size = 0.02) + 
    scale_fill_viridis_c("Target", option = "E") + 
    coord_fixed() + 
    theme_pub(base_family = "Helvetica") + 
    theme(
      legend.position = "bottom",
      axis.title = element_blank(),
      axis.line = element_blank(),
      legend.justification = c("center"),
      legend.key.width = unit(1.75 * 11, "pt")
    ) + 
    labs(subtitle = "A: Target variable")
  
  cluster_splits <- spatialsample::spatial_clustering_cv(landscape, v = 6)
  clustering <- landscape
  clustering$fold <- NA
  for (i in seq_along(cluster_splits$splits)) {
    clustering$fold[as.integer(cluster_splits$splits[[i]], "assessment")] <- i
  }
  clustering <- cbind(
    sf::st_coordinates(sf::st_geometry(landscape)),
    fold = clustering$fold
  ) |> 
    as.data.frame() |> 
    dplyr::mutate(X = X * 100, Y = Y * 100) |> 
    ggplot(aes(X, Y, fill = factor(fold))) + 
    geom_tile(color = NA, size = 0.02) + 
    scale_fill_manual("Fold", values = discrete_colors()) + 
    coord_fixed() + 
    theme_pub(base_family = "Helvetica") + 
    theme(
      legend.position = "bottom",
      axis.title = element_blank(),
      axis.line = element_blank(),
      legend.justification = c("center"),
      legend.key.width = unit(1.75 * 11, "pt")
    ) + 
    labs(subtitle = "B: Spatial clustering")
  
  block_splits <- spatialsample::spatial_block_cv(landscape, v = Inf, n = c(3, 2))
  blocking <- landscape
  blocking$fold <- NA
  for (i in seq_along(block_splits$splits)) {
    blocking$fold[as.integer(block_splits$splits[[i]], "assessment")] <- i
  }
  blocking <- cbind(
    sf::st_coordinates(sf::st_geometry(landscape)),
    fold = blocking$fold
  ) |> 
    as.data.frame() |> 
    dplyr::mutate(X = X * 100, Y = Y * 100) |> 
    ggplot(aes(X, Y, fill = factor(fold))) + 
    geom_tile(color = NA, size = 0.02) + 
    scale_fill_manual("Fold", values = discrete_colors()) + 
    coord_fixed() + 
    theme_pub(base_family = "Helvetica") + 
    theme(
      legend.position = "bottom",
      axis.title = element_blank(),
      axis.line = element_blank(),
      legend.justification = c("center"),
      legend.key.width = unit(1.75 * 11, "pt")
    ) + 
    labs(subtitle = "C: Spatial blocking")
  
  disc_splits <- spatialsample::spatial_buffer_vfold_cv(landscape, 
                                                        radius = 0.1, 
                                                        buffer = 0.1, 
                                                        v = Inf)
  discing <- landscape
  discing$fold <- "Buffer"
  discing$fold[disc_splits$splits[[2]]$in_id] <- "Analysis"
  discing$fold[disc_splits$splits[[2]]$out_id] <- "Assessment"
  discing <- cbind(
    sf::st_coordinates(sf::st_geometry(landscape)),
    fold = discing$fold
  ) |> 
    as.data.frame() |> 
    dplyr::mutate(X = as.numeric(X) * 100, Y = as.numeric(Y) * 100) |> 
    ggplot(aes(X, Y, fill = factor(fold))) + 
    geom_tile(color = NA, size = 0.02) + 
    scale_fill_manual("Fold", values = discrete_colors(3)) + 
    coord_fixed() + 
    theme_pub(base_family = "Helvetica") + 
    theme(
      legend.position = "bottom",
      axis.title = element_blank(),
      axis.line = element_blank(),
      legend.justification = c("center"),
      legend.key.width = unit(1.75 * 11, "pt")
    ) + 
    labs(subtitle = "D: LODO")
  
  p <- (outcome + clustering) / (blocking + discing)
  
  
  p
  
}
