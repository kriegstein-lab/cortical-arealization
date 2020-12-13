# 2020-11-25
# function: makeDotPlot

makeDotPlot <- function(seurat_object,  # seurat_object = ncx_full
                        group, 
                        genes_list,  
                        idents_use, 
                        area, 
                        title,
                        group_by, 
                        split_by, 
                        scale = FALSE,
                        scale_by = 'gene',
                        color_scale = 'viridis') {

  # dotplots_celltype_area_byStage <- 
    # imap(.x = genes_list, 
    #     .f = ~ 
  
        dotplot <- Seurat::DotPlot(object = seurat_object,
                           idents = idents_use,
                           features = genes_list,
                           group.by = group_by,
                           split.by = split_by,
                           scale = scale,
                           cols =  "RdYlBu",
                           dot.min = 0.01)
        
        
  # guides(size = guide_legend(direction = "vertical")) +
        
        # scale_y_discrete(labels = function(x) 
        #                           str_extract(x, areas.rgx)) +
         
        # geom_hline(yintercept = c(6.5, 12.5)) +
         
        # ggtitle(.y %>% str_extract("early|mid|late")) +
        # ylab("early            mid               late") +
         
    #    theme(axis.text.x = element_text(size = 6, angle = 45, vjust = 1, hjust = 1),
     #         axis.title.x = element_blank(),
      #        legend.position = "bottom") 
  


  x <- dotplot$data %>%
        separate(id, c('id_1', 'id_2'), remove = FALSE)
  
  if(any(x$id_1 == 'pfc')) { 
    x %<>% mutate(id_1 = factor(id_1, levels = levels(ncx_full@meta.data$area))) 
  }
  
  if(any(str_detect(x$id_1, '18'))) {
    x %<>% mutate(id_1 = as.numeric(id_1)) 
  }
  
  if(any(x$id_2 == 'pfc')) { 
    x %<>% mutate(id_1 = factor(id_1, levels = levels(ncx_full@meta.data$area))) 
  }
  
  if(any(str_detect(x$id_1, '[:digit:]+'))) {
    x %<>% mutate(id_1 = as.numeric(id_1)) 
  }
           
  
  # SCALING . . . . . . . . . . . . . . . . . . . . . . . . . . 
  
  # Scale average expression values by individual.
  message('Scaling. \n ')
  
    if(scale_by == 'gene') {
       cat('Scaling each gene across all groups.\n')
       x %<>% group_by(features.plot) %>%
         mutate(scaled_exp = scale(avg.exp, center = TRUE)) %>%
         ungroup
     } 
  
  if(scale_by == 'id_2') {
    cat('Scaling each gene within each id_2.\n')
    # Scale average expression values by individual.
    x %<>% group_by(id_2, features.plot)  %>%
      mutate(scaled_exp = scale(avg.exp, center = TRUE)) %>%
      ungroup
  } else {
       cat('Scaling each gene within each id_1.\n')
       # Scale average expression values by individual.
       x %<>% group_by(id_1, features.plot)  %>%
         mutate(scaled_exp = scale(avg.exp, center = TRUE)) %>%
         ungroup
     }

  #  # Scale average expression values by gene.
  #  if(is.null(group_by)) {
  #  
  #    print('Scaling across all groups.')
  #  
  #    x <- dotplot$data %>% 
  #        separate(id, c('cell_type', 'stage', 'area'), remove = FALSE) %>% 
  #          group_by(features.plot)
  #
  #  }

 # \ END SCALING
     
 # PLOT . . . . . . . . . . . . . . . . . . . . . . . . . . 
 
  # Use x$data to make own ggplot dotplot
  
  message('Making ggplot \n x: \n')
  
  x %<>% dfilter(id %>% str_detect('pfc|v1') & 
                !(id_2 %>% str_detect('14|25')))


  
  x %<>% unite(gene_region, 'features.plot', 'id_1', remove = FALSE)

  
  message('n_genes: \n')
  cat(n_genes <- n_distinct(x$features.plot))
  
  message('Max size: \n')
  cat(max_size <- 500 / n_genes)
  
  x %<>% bind_rows(data.frame(features.plot = x$features.plot, gene_region = paste0(x$features.plot, '_NA'), scaled_exp = NA)) %>%
    mutate(x_label = case_when(gene_region %>% str_detect('_1') ~ as.character(features.plot),
                                                                TRUE ~ ''))
    
 x %<>% group_by(features.plot, id_2) %>% 
  mutate(diff = scaled_exp[id_1 == 1] - scaled_exp[id_1 == 6]) %>% ungroup
  
 dist <- x %>% dfilter(id_1 == 1) %>% select(features.plot, id_2, diff) %>% 
   spread(key = id_2, value = diff) %>% column_to_rownames('features.plot') %>% 
   dist(diag = TRUE, upper = TRUE)
 
 genes_hclust <- hclust(dist)
 
 x %<>% mutate(features.plot = features.plot %>% factor(levels = genes_hclust$labels[genes_hclust$order]),
              id = as_factor(as.character(id)))
              
 x %<>% arrange(features.plot, id_1, id_2) %>%
         mutate(gene_region = as_factor(gene_region))
 
  print(x)
  
  dotplot <- ggplot(x) + ggtitle(title) +
  
            geom_point(aes(x = gene_region, y = id_2, 
                 colour = scaled_exp,
                 # alpha = scaled_exp,
                 size = pct.exp),
                 shape = 16) +
    
    # Color scale . . . . . . . . . . . . . . . . 
    
    
    if(color_scale == 'viridis') {  
      
      dotplot <- dotplot + 
        scale_colour_gradientn(colours = viridis(n = 100, option = 'plasma', end = 0.95),
                               na.value = "white")
      }
  
    if(color_scale == 'red_yellow_blue'){
      
      dotplot <- dotplot + 
                    scale_colour_gradientn(colours = rev(heatmap.colors),
                                       # TODO Maybe keep high yellow, low blue gradient.
                                       #values = c(0, 0.3, 1),
                                       # breaks = c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9),
                                       # limits = c(0.01, 0.9),
                                       na.value = "white")
    }
    
    dotplot <- dotplot +
    
    # scale_x_discrete(labels = x$x_label) +
      
    scale_size_area(limits = c(0, 100), max_size = max_size) +
    # scale_alpha(range = c(0.5, 0.9)) +
    guides(size = guide_legend(direction = "vertical")) +
    
    theme_void() +
    
    theme(plot.title = element_text(size = 9), 
          text = element_text(size = 5, colour = 'grey30'),
          axis.text.x = element_text(size = 5, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 5, angle = 0, hjust = 1),
          axis.title = element_blank(),
          legend.position = 'right',
          legend.key.size = unit(0.1, units = 'in'))
      
    }


heatmap.colors <- 
  c("Vermilion" = "#DF4619",
    "Flame"="#E45A17",
    "Marigold"="#F4AD39",
    "Naples Yellow"="#FAD85F",
    "Key Lime"="#F0FD8C",
    "Light Green"="#ADF196",
    "Maximum Blue Green"="#25BFC6",
    "Pacific Blue"="#21AFCB",
    "Blue NCS"="#1A90C1")
