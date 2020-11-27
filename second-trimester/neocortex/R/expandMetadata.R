expandMetadata <- function(data_frame) {
  # data_frame can be ex. @meta_data or markers df.
            data_frame %>% set_names(names(.) %>% tolower) %>%
              mutate(cell_type = tolower(cell_type) %>% factor(levels = c('rg', 'ipc', 'neuron')),
                     #structure = tolower(structure),
                     individual = as.factor(tolower(individual)), 
                     # ^ Use base R `as.factor` so it orders them alphabetically/numerically
                     stage = fct_collapse(individual,
                                          early = c("gw14", "gw16", "gw17"),
                                           mid = c("gw18_2", "gw18", "gw19_2", 
                                                   "gw19", "gw20_31", "gw20_34", "gw20"),
                                           late = c("gw22", "gw22t", "gw25")),
                     area = factor(tolower(area), 
                                   levels = c('pfc', 'motor', 'somatosensory', 
                                              'parietal', 'temporal', 'v1')),
                     region = fct_collapse(area, 
                                           msp = c("motor", "somatosensory", "parietal"))
                     ) %>%
                     unite("stage_region", stage, region, sep = "_", remove = FALSE) %>%
                     unite("stage_area", stage, area, sep = "_", remove = FALSE) %>%
                     unite("celltype_stage", cell_type, stage, sep = "_", remove = FALSE) %>%
                     unite("celltype_region", cell_type, region, sep = "_", remove = FALSE)

}