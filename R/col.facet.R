#' Plot the different treatments for PLANAQUA with different color background and style
#'
#' Used to custom the facet_wrap visualisation. The 4 treatments are gathered in each 4 corners, with label color matching the treatment.
#'
#' @param p a ggplot object
#' @param order the order of lakes for the facet (colors are not ordered automatically), by row. Default: c(1, 8, 3, 6, 11, 14, 9, 16, 2, 7, 4, 5, 12, 13, 10, 15)
#' @param col color vector, default = c("#8DADD6", "#90EE90", "#8DADD6", "#90EE90") (light blue, light green, light blue, light green), order treatment is c('No perch - unfert.', 'No perch - fert.', 'Perch - unfert.', 'Perch - fert.')
#' @keywords visualisation, wavelet
#' @examples col.facet(p1, c(1, 8, 3, 6, 11, 14, 9, 16, 2, 7, 4, 5, 12, 13, 10, 15))
#' 

col.facet <- function(p, order = c(1, 8, 3, 6, 11, 14, 9, 16, 2, 7, 4, 5, 12, 13, 10, 15), col = c("#8DADD6", "#90EE90", "#8DADD6", "#90EE90")) {
  if(length(col) < 4) col <- rep(col, 4) 
  if(length(col) > 4) col <- col[1:4]
  
  g <- ggplot_gtable(ggplot_build(p+ theme_bw()))
  stripr <- which(grepl('strip-t', g$layout$name))
  treatments <- data.frame(lake = 1:16, 
                           treatment = c('No perch - fert.', 'No perch - unfert.', 'Perch - fert.', 'Perch - unfert.', 'Perch - unfert.', 'Perch - fert.', 'No perch - unfert.', 'No perch - fert.', 'Perch - fert.', 'Perch - unfert.', 'No perch - fert.', 'No perch - unfert.', 'No perch - unfert.', 'No perch - fert.', 'Perch - unfert.', 'Perch - fert.')) %>% 
    mutate(color = case_when(
      treatment == 'No perch - unfert.' ~ col[1], 
      treatment == 'No perch - fert.' ~ col[2], 
      treatment == 'Perch - unfert.' ~ col[3], #light blue
      treatment == 'Perch - fert.' ~ col[4] #light green 
    ))
  treatments$lake <- factor(treatments$lake, levels = order)
  fills <- treatments$color[rev(order(treatments$lake))]
  k <- 1
  for (i in stripr) {
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k+1
  }
  
  # Remove spaces between panels
  g$heights[[12]] = g$heights[[22]] = unit(0, "lines")
  g$widths[[7]] = g$widths[[15]] = unit(0, "points")
  
  
  return(grid::grid.draw(g))
}
