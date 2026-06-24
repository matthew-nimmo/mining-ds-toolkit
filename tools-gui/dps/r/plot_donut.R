plot_donut <- function(value, title=NULL) {
  df <- data.frame(x = c(1, 1),
                   y <- c(value, 1-value),
                   name = c("G1", "G2"))
  
  ggplot(data = df, aes(x = x, y = y, fill = name)) +
    geom_col(show.legend = FALSE) +
    coord_polar(theta = "y",
                direction = -1) +
    xlim(c(-2, 2)) +
    scale_fill_manual(values = c("red", "grey90")) +
    labs(title = title) +
    theme_void() +
    annotate("text",
             label = paste0(signif(100*value, 3), "%"),
             fontface = "bold",
             color = "red",
             size = 6,
             x = -2,
             y = 0) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill=NA, colour=NA))
}