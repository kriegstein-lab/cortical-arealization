# 2020-11-25
# function: makeDotPlot

makeDotPlot <- function(seurat_object, group, genes_list,  
                        idents_use, area,
                        group_by, split_by, scale = FALSE) {
                        # seurat_object = ncx_full
  
  print(idents_use)
  
  # dotplots_celltype_area_byStage <- 
    # imap(.x = genes_list, 
    #     .f = ~ 
  
dotplot <- DotPlot(object = seurat_object,
             idents = idents_use,
            features = genes_list,
                 group.by = group_by,
                 split.by = split_by,
                 scale = scale,
                 cols =  "RdYlBu",
                 # cols = c('red', 'blue', 'green', 'purple', 'orange', 'yellow', 'pink'),
                 dot.min = 0.01) 
  # + 
         
        # scale_colour_gradientn(colours = rev(heatmap.colors),
                       # TODO Maybe keep high yellow, low blue gradient.
                         # values = c(0, 0.3, 1),
                         # breaks = c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9),
                         # limits = c(0.01, 0.9),
                         # na.value = "white"
        # ) +
        
  # guides(size = guide_legend(direction = "vertical")) +
        
        # scale_y_discrete(labels = function(x) 
        #                           str_extract(x, areas.rgx)) +
         
        # geom_hline(yintercept = c(6.5, 12.5)) +
         
        # ggtitle(.y %>% str_extract("early|mid|late")) +
        # ylab("early            mid               late") +
         
    #    theme(axis.text.x = element_text(size = 6, angle = 45, vjust = 1, hjust = 1),
     #         axis.title.x = element_blank(),
      #        legend.position = "bottom") 
  
  print('Making ggplot')
  
  # print(head(dotplot$data))
  
  x <- dotplot$data
  
  x %<>% separate(id, c('id_1', 'area'), remove = FALSE) %>%
            mutate(area = factor(area, levels = levels(ncx_full@meta.data$area)))
  # SCALING
  
  # Scale average expression values by individual.
  #if(group_by == 'individual') {
    
   # print('Scaling within each individual.')
    # Scale average expression values by individual.
     x %<>% group_by(id_1, features.plot)  %>%
        mutate(scaled_exp = scale(avg.exp, center = TRUE)) %>%
          ungroup

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

 # \END SCALING
  
  x %<>% arrange(id_1, area) %>%
    mutate(id = as_factor(as.character(id)),
           features.plot = as.factor(as.character(features.plot)))
  
  # print(head(x))
  
  dotplot <- ggplot(x) + ggtitle(group) +
  
  geom_point(aes(x = features.plot, y = id, 
                 color = scaled_exp, size = pct.exp)) +
    scale_colour_gradientn(colours = rev(heatmap.colors),
                           # TODO Maybe keep high yellow, low blue gradient.
                           #values = c(0, 0.3, 1),
                           # breaks = c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9),
                           # limits = c(0.01, 0.9),
                           na.value = "white") +
    
    scale_size_area(limits = c(0, 100)) +
    
    guides(size = guide_legend(direction = "vertical")) +
    
    theme_cowplot() +
    theme(axis.text.x = element_text(size = 6, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 5, angle = 45, vjust = 1, hjust = 1),
          axis.title = element_blank(),
          legend.position = 'right') 
      
}


areas.rgx <- ncx_full@meta.data$area %>% unique %>% paste(collapse = "|")

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

heatmap.colors
