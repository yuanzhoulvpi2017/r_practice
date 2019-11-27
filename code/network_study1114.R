library(devtools)
library(UserNetR)
library(statnet)
library(RColorBrewer)
library(readxl)

?RColorBrewer # 共八个调色板
display.brewer.pal(8, "Set2")
display.brewer.pal(9, "Pastel1")
display.brewer.pal(8, "Pastel2")

wb=read_xlsx("data1114003.xlsx", row.names = TRUE)
colnames(wb)
wb = wb[, -1]
rownames(wb)

wb1=network(wb,matrix.type="adjacency")
gplot(wb1,vertex.col=2)

set.vertex.attribute(wb1, "fans", c("A","A","A","A","A","B","B","B","B","B","C","C","C","C","C","C","C","C","C","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D"))
wb_pal=brewer.pal(4,"Pastel2") # wb_pal 为字符型向量
fanl=as.factor(get.vertex.attribute(wb1,"fans")) 
gplot(wb1,vertex.col=wb_pal[fanl])

wb2=symmetrize(wb1,rule="weak")
wb2
gplot(wb2,usearrows=F,vertex.col=wb_pal[fanl])

wbc=network(wb2,directed=FALSE)
gplot(wbc,usearrows=F,vertex.col=wb_pal[fanl],xlab="Circle Graph")
wbs=network(wb2,directed=FALSE)
gplot(wbs,usearrows=F,vertex.col=wb_pal[fanl],xlab="Star Graph")

vignette("networkVignette")

plot(wbs)

vignette("networkDynamic")



library(igraph)

# Create data

network <- graph_from_adjacency_matrix(as.matrix(wb) , mode='undirected', diag=F)

# When ploting, we can use different layouts:
par(mfrow=c(2,2), mar=c(1,1,1,1))
plot(network, layout=layout.sphere, main="sphere")
plot(network, layout=layout.circle, main="circle")
plot(network, layout=layout.random, main="random")
plot(network, layout=layout.fruchterman.reingold, main="fruchterman.reingold")



par(mfrow = c(1, 1))

plot(network,
     edge.color=rep(c("red","pink"),5),           # Edge color
     edge.width=seq(1,10),                        # Edge width, defaults to 1
     edge.arrow.size=1,                           # Arrow size, defaults to 1
     edge.arrow.width=1,                          # Arrow width, defaults to 1
     edge.lty=c("solid")                           # Line type, could be 0 or “blank”, 1 or “solid”, 2 or “dashed”, 3 or “dotted”, 4 or “dotdash”, 5 or “longdash”, 6 or “twodash”
     #edge.curved=c(rep(0,5), rep(1,5))            # Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
)


deg <- degree(network, mode="all")
plot(network, vertex.size=deg*6, vertex.color=rgb(0.1,0.7,0.8,0.5))



par(bg="black")
plot(network,
     
     # === vertex
     vertex.color = rgb(0.8,0.4,0.3,0.8),          # Node color
     vertex.frame.color = "blue",                 # Node border color
     vertex.shape="circle",                        # One of “none”, “circle”, “square”, “csquare”, “rectangle” “crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
     vertex.size=5,                               # Size of the node (default is 15)
     vertex.size2=NA,                              # The second size of the node (e.g. for a rectangle)
     
     # === vertex label
     vertex.label=LETTERS[1:10],                   # Character vector used to label the nodes
     vertex.label.color="white",
     vertex.label.family="Times",                  # Font family of the label (e.g.“Times”, “Helvetica”)
     vertex.label.font=2,                          # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
     vertex.label.cex=1,                           # Font size (multiplication factor, device-dependent)
     vertex.label.dist=0,                          # Distance between the label and the vertex
     vertex.label.degree=0 ,                       # The position of the label in relation to the vertex (use pi)
     
     # === Edge
     edge.color="orange",                           # Edge color
     edge.width=1,                                 # Edge width, defaults to 1
     edge.arrow.size=1,                            # Arrow size, defaults to 1
     edge.arrow.width=1,                           # Arrow width, defaults to 1
     edge.lty="solid",                             # Line type, could be 0 or “blank”, 1 or “solid”, 2 or “dashed”, 3 or “dotted”, 4 or “dotdash”, 5 or “longdash”, 6 or “twodash”
     edge.curved=0.3    ,                          # Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
)
