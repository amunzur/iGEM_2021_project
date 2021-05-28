library(plotly)

sce <-  sce_norm

colors_list <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3", '#E2201C', '#1D91C0', '#6B3E9A', '#32A028', "deepskyblue", "black")
min_cluster <- as.numeric(min(as.vector(sce$cluster)))
max_cluster <- max(as.numeric(as.character(sce$cluster)))
cluster_name <- min_cluster:max_cluster
colors_list <- colors_list[1:max_cluster]

tsne_1 <- as.numeric(sce@int_colData@listData[["reducedDims"]]@listData[["TSNE"]][, 1])
tsne_2 <- as.numeric(sce@int_colData@listData[["reducedDims"]]@listData[["TSNE"]][, 2])
tsne_3 <- as.numeric(sce@int_colData@listData[["reducedDims"]]@listData[["TSNE"]][, 3])

pca_1 <- as.numeric(sce@int_colData@listData[["reducedDims"]]@listData[["PCA"]][, 1])
pca_2 <- as.numeric(sce@int_colData@listData[["reducedDims"]]@listData[["PCA"]][, 2])
pca_3 <- as.numeric(sce@int_colData@listData[["reducedDims"]]@listData[["PCA"]][, 3])

umap_1 <- as.numeric(sce@int_colData@listData[["reducedDims"]]@listData[["UMAP"]][, 1])
umap_2 <- as.numeric(sce@int_colData@listData[["reducedDims"]]@listData[["UMAP"]][, 2])
umap_3 <- as.numeric(sce@int_colData@listData[["reducedDims"]]@listData[["UMAP"]][, 3])

df_tsne <- data.frame(tsne_1, tsne_2, tsne_3)
df_tsne$cluster <- as.factor(sce$cluster)

df_pca <- data.frame(pca_1, pca_2, pca_3)
df_pca$cluster <- as.factor(sce$cluster)

df_umap <- data.frame(umap_1, umap_2, umap_3)
df_umap$cluster <- as.factor(sce$cluster)

tsne_3d <- plot_ly(data = df_tsne,
                   x = ~tsne_1, y = ~tsne_2, z = ~tsne_3,
                   opacity = 1,
                   color = ~cluster,
                   type = "scatter3d",
                   mode = "markers",
                   marker = list(size = 3), 
                   colors = colors_list) %>% 
  layout(legend= list(itemsizing='constant'))

tsne_2d <- plot_ly(data = df_tsne,
                   x = ~tsne_1, y = ~tsne_2,
                   opacity = 1,
                   color = ~cluster,
                   type = "scatter",
                   mode = "markers",
                   marker = list(size = 3), 
                   colors = colors_list) %>% 
  layout(legend= list(itemsizing='constant'))

umap_2d <- plot_ly(data = df_umap,
                   x = ~umap_1, y = ~umap_2,
                   opacity = 1,
                   color = ~cluster,
                   type = "scatter",
                   mode = "markers",
                   marker = list(size = 3), 
                   colors = colors_list) %>% 
  layout(legend= list(itemsizing='constant'))



































