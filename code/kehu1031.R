dat <- read.csv("~/Rcode/cytofkit_annotated_Rphenograph_added_diffusionmap_markerFiltered_transformed_merged_exprssion_data.csv")
label_dat <- read.csv("~/Rcode/cytofkit_annotated_Rphenograph_added_diffusionmap_clusters.csv")
names(dat)
#tsne
library(Rtsne)
Labels<-label_dat[, 2]
dat$X<-Labels
table(dat$X)

## for plotting
colors = rainbow(length(unique(dat$X)))
names(colors) = unique(dat$X)

## Executing the algorithm on curated data
tsne<- Rtsne(dat[,-1], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)

library(fpc)
label_ <- dbscan(tsne$Y, 10)
str(label_)
## Plotting
plot_df <- data.frame(tsne1 = tsne$Y[, 1], tsne2 = tsne$Y[, 2],
                      label = factor(label_$cluster))
library(factoextra)
fviz_cluster(label_, data = df, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco") #, ggtheme = theme_classic())

library(ggplot2)
ggplot(plot_df, aes(x = tsne1, y = tsne2, colour = label)) + geom_point()




#heatmap#########################################
library(pheatmap)
dat <- read.csv("~/Rcode/cytofkit_annotated_Rphenograph_added_diffusionmap_Rphenograph_cluster_mean_data.csv")
names(dat)
mat <- as.matrix(dat[, -1])
colnames(mat) <- colnames(mat)
rownames(mat) <- unlist(dat$X)
mat 
pheatmap(mat)
