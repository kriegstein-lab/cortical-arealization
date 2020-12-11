vennGroup <- function (vectors, 
                       title,
                       cat.cex = 1.5, cex = 1, 
                       cat.pos = NULL, cat.dist = NULL, 
                       label = TRUE, lab.cex = 1, lab.col = "black", 
                       fill = NULL, 
          file = NULL, ext = NULL, width = 8, height = 8) 
{
  save <- !is.null(file)
  if (save) {
    .get.dev(file, ext, height = height, width = width)
  }
  if (!requireNamespace("VennDiagram")) {
    stop("package 'VennDiagram' is required for this function")
  }
  if (!requireNamespace("RColorBrewer")) {
    stop("package 'RColorBrewer' is required for this function")
  }
  if (!requireNamespace("grid")) {
    stop("package 'grid' is required to use this function")
  }
  len <- length(vectors)
  if (is.null(fill)) {
    if (len == 2) {
      fill = c("lightpink", "lightblue")
    }
    else {
      fill = RColorBrewer::brewer.pal(len, "Pastel1")
    }
  }
  else {
    if (length(fill) == len) {
      fill = fill
    }
    else if (length(fill) > len) {
      warning(paste("more colors being provided than required, will ignore ", 
                    length(fill) - len, " colors", sep = ""))
      fill = fill[1:len]
    }
    else {
      warning("not enough colors being provided, will use default")
      if (len == 2) {
        fill = c("lightpink", "lightblue")
      }
      else {
        fill = RColorBrewer::brewer.pal(len, "Pastel1")
      }
    }
  }
  if (len > 2 && label) {
    warning("currently only support 2 groups to have actual item labels; will only use numbers")
  }
  else if (len > 5 || len < 2) {
    stop("please provide 2 to 5 vectors")
  }
  
  alpha = rep(0.5, len)
  
  if (!is.null(cat.pos) && !is.null(cat.dist)) {
    
    v <- VennDiagram::venn.diagram(main = title,
                                   x = vectors, 
                                   fill = fill, 
                                   main.fontfamily = 'sans',
                                   fontfamily = rep('sans', 3),
                                   force.unique = TRUE, 
                                   alpha = alpha, 
                                   cat.dist = cat.dist, cat.pos = cat.pos, 
                                   cat.cex = cat.cex, cex = cex, 
                                   filename = NULL)
  }
  
  else if (!is.null(cat.pos) && is.null(cat.dist)) {
    v <- VennDiagram::venn.diagram(vectors, fill = fill, 
                                   alpha = alpha, cat.pos = cat.pos, cat.fontface = "bold", 
                                   cat.cex = cat.cex, cex = cex, filename = NULL)
  }
  else if (is.null(cat.pos) && !is.null(cat.dist)) {
    v <- VennDiagram::venn.diagram(main = title_use, vectors, fill = fill, 
                                   alpha = alpha, cat.fontface = "bold", cat.dist = cat.dist, 
                                   cat.cex = cat.cex, cex = cex, filename = NULL)
  }
  else {
    v <- VennDiagram::venn.diagram(vectors, fill = fill, 
                                   alpha = alpha, cat.fontface = "bold", cat.cex = cat.cex, 
                                   cex = cex, filename = NULL)
  }
  if (len > 2 && len <= 5) {
    grid::grid.newpage()
    grid::grid.draw(v)
  }
  if (len == 2) {
    if (!label) {
      grid::grid.newpage()
      grid::grid.draw(v)
    }
    else {
      name <- lapply(v, names)
      
      v.labels <- lapply(v, function(i) i$label)
      v.lab <- vector()
      for (i in 1:length(v.labels)) {
        if (length(v.labels[[i]] %in% names(vectors)) != 
            0 && isTRUE(v.labels[[i]] %in% names(vectors))) {
          v.lab <- c(v.lab, v.labels[[i]])
        }
      }
      
      v1 <- vectors[[v.lab[1]]]
      v2 <- vectors[[v.lab[2]]]
      
      v[[5]]$label <- paste(c(v[[5]]$label, 
                              setdiff(v1, v2) %>% head(30)), 
                              collapse = "\n"
                              )
      
      v[[5]]$gp$cex <- lab.cex
      v[[5]]$gp$col <- lab.col
      
      v[[6]]$label <- paste(c(v[[6]]$label, 
                              setdiff(v2, v1)), 
                              collapse = "\n")
      
      v[[6]]$gp$cex <- lab.cex
      v[[6]]$gp$col <- lab.col
      
      # Intersecting genes
      v[[8]]$label <- paste(c(v[[8]]$label, 
                              intersect(v1, v2)), 
                              collapse = "\n")
      v[[8]]$gp$cex <- lab.cex
      v[[8]]$gp$col <- lab.col
      
      grid::grid.newpage()
      grid::grid.draw(v)
    }
  }
  
  if (save) {
    dev.off()
  }
  
  return(v)
  
}

