
suppressPackageStartupMessages(require(golubEsets))
data(Golub_Merge)




golub <- data.frame(Golub_Merge)[1:7129]
golub.rearrange <- golub[, order(apply(golub, 2, var), decreasing = T)]



#question1

golub <- golub.rearrange[, 1:10]
fit <- kmeans(x = golub, 2)
library(RColorBrewer)
mat_gol <- data.matrix(golub)
cols<-brewer.pal(n=3,name="Set1")
cols_t1 <- cols[as.matrix(fit$cluster)]
heatmap(mat_gol,Colv = NA, Rowv = NA, scale="column", RowSideColors=cols_t1, xlab="Genes", ylab="Patients")


golub <- golub.rearrange[, 1:50]
fit <- kmeans(x = golub, 2)
library(RColorBrewer)
mat_gol <- data.matrix(golub)
cols<-brewer.pal(n=3,name="Set1")
cols_t1 <- cols[as.matrix(fit$cluster)]
heatmap(mat_gol,Colv = NA, Rowv = NA, scale="column", RowSideColors=cols_t1, xlab="Genes", ylab="Patients")

library(Biobase)
library(genefilter)
marr <- read.csv("calcineurin.csv",header=TRUE)
n <- ncol(marr)
print(n)
m <- nrow(marr)
print(m)




marrall <- marr[1:nrow(marr),2:ncol(marr)]
matmarr <- as.matrix(marrall)
pData <- read.table("http://www.cse.unsw.edu.au/~mike/binf2010/calcineurin.hdr", row.names = 1, header = TRUE, sep = ",")
phenoData <- new("AnnotatedDataFrame", data = pData)
eset <- new('ExpressionSet',exprs=matmarr, phenoData = phenoData)
gStr <- marr[,1]
featureNames(eset) <- gStr
show(eset)



fnsp <- function(x) (IQR(x, na.rm = T) > 0.5)
fnna15 <- function(x) { (!is.na(x[9])) }
cnca15 <- function(x) { (x[9] < -1.0) }
fnna30 <- function(x) { (!is.na(x[10])) }
cnca30 <- function(x) { (x[10] < -1.0) }

ff <- filterfun(fnsp,fnna15,cnca15,fnna30,cnca30)
selected <- genefilter(exprs(eset),ff)
sum(selected)
esetSub <- exprs(eset)[selected,]


d <- dist(esetSub)
hcl <- hclust(d, method = "complete", members=NULL)
plot(hcl)

heatmap(esetSub,
        Rowv=as.dendrogram(hcl),
        col = topo.colors(64),
        margins = c(10,10),
        xlab = "Samples", ylab= "Genes"
)


plot(as.dendrogram(hcl))
identify(hcl, function(k) print.listof(k))



partition <- cutree(hcl,k=4)
table(partition)
subset <- partition[partition==2]
genes <- names(subset)
gene_list <- paste(genes, collapse = "\n")
cat(gene_list)

