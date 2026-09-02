cor_network <- function(x, cor=0.5, t.size=3, p.size=2) {
  if (class(x) != "cor_df") {
    x <- corrr::correlate(as.data.frame(x))
  }
  x <- corrr::stretch(x)
  x[is.na(x)] <- 0

  y <- x %>%
    dplyr::filter(!is.na(r)) %>%
    dplyr::filter(abs(r) > cor) %>%
    igraph::graph_from_data_frame(directed=FALSE)

  ggraph(y) +
    geom_edge_link(aes(edge_width=abs(r), edge_alpha=abs(r), color=r)) +
    scale_edge_width(range = c(0.5, 2)) +
    guides(edge_alpha="none", edge_width="none") +
    scale_edge_colour_gradient2(low="red", high="blue") +
    geom_node_point(color="black", size=p.size) +
    geom_node_text(aes(label=name), repel=TRUE, size=t.size) +
    theme_graph(base_family="sans") +
    theme(legend.key.width = unit(8, 'points'),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
}
