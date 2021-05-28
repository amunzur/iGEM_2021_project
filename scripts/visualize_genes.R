

visualize_gene_TSNE <- function(sce, gene_name){
  
  idx <- grep(gene_name, rownames(sce), ignore.case = TRUE)
  gene_expression <- as.vector(logcounts(sce[idx, ])[1, ])
  
  tsne_1 <- as.numeric(sce@int_colData@listData[["reducedDims"]]@listData[["TSNE"]][, 1])
  tsne_2 <- as.numeric(sce@int_colData@listData[["reducedDims"]]@listData[["TSNE"]][, 2])
  
  df_tsne <- data.frame(tsne_1, tsne_2, gene_expression)
  colnames(df_tsne)[3] <- paste(gene_name, "logcounts")

  p <- plot_ly(data = df_tsne,
                      x = ~tsne_1, y = ~tsne_2,
                      opacity = 1,
                      color = ~gene_expression,
                      type = "scatter",
                      mode = "markers",
                      marker = list(size = 3), 
                      colors = "Reds") %>% 
    layout(legend= list(itemsizing='constant'))
  
  return(p)
  
}


visualize_gene_TSNE(sce, "TNFAIP2")
