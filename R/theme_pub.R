theme_pub <- function(base_fill = "white",
                      base_size = 11,
                      base_family = "Lato") {
  ggplot2::theme_minimal() %+replace%
    ggplot2::theme(axis.line = element_line(),
                   panel.grid = element_blank(),
                   axis.text = element_text(size = base_size * 0.9),
                   axis.title = element_text(size = base_size * 1.15),
                   text = element_text(family = base_family),
                   strip.background = element_blank(),
                   strip.text = element_text(size = base_size),
                   legend.text = element_text(size = base_size * 0.85),
                   legend.title = element_text(hjust = 0, size = base_size * 1.1),
                   legend.background = element_blank(),
                   legend.spacing = unit(base_size, "pt"),
                   legend.margin = margin(0, 0, 0, 0),
                   legend.key = element_blank(),
                   legend.key.size = unit(1.2 * base_size, "pt"),
                   legend.box.margin = margin(0, 0, 0, 0),
                   legend.box.background = element_blank(),
                   legend.box.spacing = unit(base_size, "pt"),
                   panel.background = element_rect(fill = base_fill, color = NA),
                   plot.background = element_rect(fill = base_fill, color = NA),
                   title = element_text(size = base_size * 1.3, family = base_family),
                   complete = TRUE
    )
}
