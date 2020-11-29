# 2020-11-25
# function: makeDotPlot

makeDotPlot <- function(seurat_object, cell_type, area, genes_list){
                        # seurat_object = ncx_full
  dotplots_celltype_area_byStage <- 
    
    imap(.x = genes_list, 
         .f = ~ DotPlot(object = seurat_object,
                 idents = cell_type,
                 features = .x,
                 group.by = 'individual',
                 split.by = 'area',
                 scale = TRUE,
                 cols =  "RdYlBu",
                 dot.min = 0.01) + 
         
        scale_colour_gradientn(colours = rev(heatmap.colors),
                       # TODO Maybe keep high yellow, low blue gradient.
                         # values = c(0, 0.3, 1),
                         # breaks = c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9),
                         # limits = c(0.01, 0.9),
                         # na.value = "white"
        ) +
        guides(size = guide_legend(direction = "vertical")) +
        
        # scale_y_discrete(labels = function(x) 
        #                           str_extract(x, areas.rgx)) +
         
        # geom_hline(yintercept = c(6.5, 12.5)) +
         
        ggtitle(.y %>% str_extract("early|mid|late")) +
        ylab("early            mid               late") +
         
        theme(axis.text.x = element_text(size = 6, angle = 45, vjust = 1, hjust = 1),
              axis.title.x = element_blank(),
              legend.position = "bottom") 
      )
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
