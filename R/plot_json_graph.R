#' Plots an igraph visualization of a single json document
#'
#' This function first calls json_structure, and then uses that data to create
#' an igraph object, and then plots that object.
#'
#' Each dot on the plot corresponds to a node in the JSON document, which
#' could include an object or an array (which will have children nodes) or
#' a string, number, logical or null value which will be terminal nodes. The
#' graph connects parent nodes to child nodes, and the vertices are colored
#' based on json_types.
#'
#' If show.labels is TRUE, then the names for object values are plotted on
#' the value node.
#'
#' If you have a very large document (json_complexity larger than a few
#' hundred), you should consider setting show.labels to FALSE, and reducing
#' the vertex.size and edge.width. Documents that are even more complex may
#' need to be broken into smaller chunks to be visualized effectively.
#'
#' Note that the legend is plotted automatically, but may not be scaled
#' correctly. Set legend to FALSE and manually create your own legend if
#' you wish to reposition it.
#'
#' Also note that this function sets the plot margins to zero in order to
#' maximize the size of the graph on the page. the par() is reset afterwards.
#'
#' @param x a tbl_json object
#' @param legend add a type color legend automatically
#' @param vertex.size the size of the vertices (helpful to reduce this if the
#'        json is very complex
#' @param edge.color the color for the edges
#' @param edge.width the width of the edge lines, helpful to reduce this if
#'        the json is very complex
#' @param show.labels should object names be shown
#' @param plot should the plot be rendered?
#' @param ... further arguments to igraph::plot.igraph
#' @return the igraph object
#' @export
#' @examples
#'
#' # An illustrative example
#' '{"object" : {"key": 1},
#'   "array"  : ["a", "b"],
#'   "string" : "value",
#'   "number" : 1,
#'   "logical": true,
#'   "null"   : null
#'  }' %>% plot_json_graph
#'
#' # a more complex real example
#' worldbank[1] %>% plot_json_graph
#'
#' # a very complex real example
#' companies[1] %>% plot_json_graph(show.labels = FALSE, vertex.size = 4)
#'
plot_json_graph <- function(x, legend = TRUE, vertex.size = 6,
                            edge.color = 'grey70', edge.width = .5,
                            show.labels = TRUE, plot = TRUE,
                            ...) {

  if (!is.tbl_json(x)) x <- as.tbl_json(x)

  assert_that(nrow(x) == 1)

  structure <- x %>% json_structure

  type_colors <- RColorBrewer::brewer.pal(6, "Accent")

  graph_edges <- structure %>%
    filter(!is.na(parent.id)) %>%
    select(parent.id, child.id)

  graph_vertices <- structure %>%
    transmute(child.id,
              vertex.color = type_colors[as.integer(type)],
              vertex.label = key)

  if (!show.labels)
    graph_vertices$vertex.label <- rep(NA_character_, nrow(graph_vertices))

  g <- igraph::graph_from_data_frame(graph_edges, vertices = graph_vertices,
                             directed = FALSE)

  if (plot) {
    op <- par(mar = c(0, 0, 0, 0))
    plt <- igraph::plot.igraph(g,
         vertex.color = igraph::V(g)$vertex.color,
         vertex.size  = vertex.size,
         vertex.label = igraph::V(g)$vertex.label,
         vertex.frame.color = NA,
         layout = layout_with_kk,
         edge.color = edge.color,
         edge.width = edge.width,
         ...)

    if (legend)
      legend(x = -1.3, y = -.6, levels(structure$type), pch = 21,
             col= "white", pt.bg = type_colors,
             pt.cex = 2, cex = .8, bty = "n", ncol = 1)

    par(op)
  }

  invisible(g)

}

