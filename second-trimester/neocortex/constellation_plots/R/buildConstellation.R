# title: "buildconstellation"
# date: 2021-03-03
# output: html_document
# params:
  # cells.cl.df: NULL # s.obj@meta.data
  # groups.col: NULL  # character, name of column to group by
  # rd.dat: list()
  # colors.use : c()
  # cl: NULL
  # cl.numeric: NULL
  # n.pcs: 20,
  # run.knn: TRUE
  # k: 15,
  # frac.th: 0.05,
  # cl.df: NULL
  # out.dir: '../out'


source_rmd("~/cse-phd/second-trimester/neocortex/constellation_plots/R/scrattch.hicat_fxns.Rmd")

buildConstellation <- function(args_list, 
                               groups.col, colors.use,
                               run.knn, n.pcs, k, frac.th, 
                                out.dir) {


cells.cl.df <- args_list$cells_cl_df
rd.dat <- args_list$reductions
cl <- args_list$cl
cl.numeric <- args_list$cl_numeric

# 1. Build dataframes for constellation plots.

# 1.1 cl_and_cl_numeric
# cl and cl numeric can also be passed as a param to the Rmd.

if(is.null(cl) & is.null(cl.numeric)) {
  
    # Cluster, or area-celltype combination.
    cl <- cells.cl.df[[groups.col]] %>% as.factor %>% 
    set_names(cells.cl.df$cell.name)
    # 128 clusters in all _combo2_ cells
    # 77 clusters in all _combo2_ & ExN lineage cells.

    cl.numeric <- as.numeric(cl) %>% set_names(nm = names(cl))
}

## 2 cl.df

cl.df <- get_cl_df(cl)

cl.df$area <- str_split_fixed(cl.df$cluster_label, "-", 2)[ ,2] %>% tolower 

cl.df$clade <- str_split_fixed(cl.df$cluster_label, "-", 2)[ ,1] %>% tolower 
# Add clade_id, clade_color to cl.df
# cl.df <- cl.df %>% left_join(clade.cols)


cl.df <- cl.df %>% left_join(colors.use, by = c("clade" = "celltype")) 

# %>%  rename(cluster_color = "colors.use")
# rm(group.cols)

# cells.cl.df: Add cluster_id column from cl.df; remove unused columns. 
cells.cl.df <- cells.cl.df %>% select(-cluster_label) %>% rename(cluster_label = groups.col) %>%
                        left_join(
                         # %>% select(cell.name, groups.col, combined.cluster.2),
                         cl.df, by = "cluster_label") %>%
                         
                         # Requires cells.cl.df (metadata) to have column being used for groups
                         # named 'cluster_label' to match with cl_df during join.
                 mutate(cluster_id = as.factor(cluster_id))

# ----------------------------------------------
## 4 Find cluster centroids from UMAP coordinates
## rd.cl.center

rd.cl.center <- get_RD_cl_center(rd.dat = rd.dat$umap, cl)
message("Got rd.cl.center")

# update-rd.cl.center

rd.cl.center %<>% 
  as.data.frame %>% 
  set_names(c("x", "y")) %>%
  add_column(cl = cl.df$cluster_id, .before = "x") %>%
  # add_column preserves rownames.
  # but moving rownames to column cluster_label anyway bc of left_join below.
  # Needs to be cl (not cl_id) or else you get error:
  # Error in `$<-.data.frame`(`*tmp*`, "edge.frac.within", value = numeric(0)) : 
  # replacement has 0 rows, data has 26 
  rownames_to_column("cluster_label")

message("Updated rd.cl.center")

## 5 Join `cl.df` and `rd.cl.center` into `cl.center.df` for input into `get_KNN_graph`.
## cl.center.df
cl.center.df <- left_join(rd.cl.center, cl.df,
                          by = c("cluster_label")) 


# 6 Get knn and cluster counts
# Calls `knn.cl` in scrattch.hicat_fxns.Rmd
# knn.result
if(run.knn == TRUE) {

knn.result <<- RANN::nn2(data = rd.dat$pca[, 1:n.pcs], k = k)

}else{knn.result <- knn.result}

knn.cl <- get_knn_graph(knn.result = knn.result,
                        rd.dat = rd.dat$umap, 
                        cl.df =  cl.df, 
                        cl = cl.numeric,
                        cl.numeric = cl.numeric,
                        knn.outlier.th = 2, 
                        outlier.frac.t = 0.5)

# rm(rd.dat, ncx.clusters)

# -----------------------------------------------------------------------------
# 2. Make constellation plot

# knn_cl_df_filter

# Keep only cells whose $frac >= 0.05.
# frac = fraction of cells in cluster with nearest neighbors in a different cluster.
# Defined in `get_knn_graph`: 
# knn.cl.df$frac = knn.cl.df$Freq / knn.cl.df$cl.from.total
# 10% : 213 edges
knn.cl.df.filter <- knn.cl$knn.cl.df %>% dplyr::filter(frac >= frac.th) %>% 
  mutate(cl.from = as.numeric(cl.from), cl.to = as.numeric(cl.to))

# cl.to, cl.from numeric or factor?
# Need to be numeric for getting the rows where cl.to == cl.from (knn.cl.df$same)


## plot-constellation

# To plot only edges between ExN lineage clusters:
# knn.cl.df %<>% filter_at(vars(cl.from.label, cl.to.label), 
#                        all_vars(str_detect(., "RG|IPC|Neuron|OPC|Dividing")))

cl.plot <- plot_constellation(knn.cl.df = knn.cl.df.filter, 
                              cl.center.df = cl.center.df, 
                              out.dir = out.dir,
                              node.label = "cluster_label",  # name of column in cl.center.df with cluster/node names
                              exxageration = 0.4, curved = TRUE, 
                              plot.parts = FALSE, plot.hull = NULL, 
                              plot.height = 40, plot.width = 40,
                              node.dodge = TRUE, 
                              label.size = 3, max_size = 25)

return(lst(cl.center.df, knn.result, cl.plot))

}