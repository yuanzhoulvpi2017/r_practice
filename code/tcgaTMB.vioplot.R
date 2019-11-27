#install.packages("vioplot")

library(vioplot)                                                    #???ð?
#setwd("C:\\Users\\lexb4\\Desktop\\tcgaTMB\\23.vioplot")          #???ù???Ŀ¼
type_1=30#??????Ʒ??Ŀ
type_2 = 36
type_3 = 32
type_4 = 35                                                           #??????Ʒ??Ŀ

rt=read.table("CIBERSORT.filter.txt",sep="\t",header=T,row.names=1,check.names=F)   #??ȡ?????ļ?

label <- sample(x = c("l1", "l2"), replace = TRUE, size = 155)

value <- as.matrix(rt)
dim(value) <- 155 * 22

rt_plot_data <- data.frame(
  label <- rep(label, time = 22),
  columns <- rep(names(rt), each = 155),
  value = value
)
rm(rt_plot_data)
ggplot(data = rt_plot_data, aes(x = columns, y = value, fill = label)) + geom_violin(position="dodge")



pdf("vioplot.pdf",height=8,width=15)              #????ͼƬ???ļ?????
par(las=1,mar=c(10,6,3,3))
x=c(1:ncol(rt))
y=c(1:ncol(rt))
plot(x,y,
     xlim=c(0,63),ylim=c(min(rt),max(rt)+0.02),
     main="",xlab="", ylab="Fraction",
     pch=21,
     col="white",
     xaxt="n")

#??ÿ??????ϸ??ѭ????????vioplot????TMB????ɫ??ʾ????TMB?ú?ɫ??ʾ
for(i in 1:ncol(rt)){
  if(sd(rt[1:type_1,i])==0){
    rt[1,i]=0.001
  }
  if(sd(rt[(type_1+1):(type_1+type_2),i])==0){
    rt[(type_1+1),i]=0.001
  }
  type_1Data=rt[1:type_1,i]
  type_2Data=rt[(type_1+1):(type_1+type_2),i]
  vioplot(type_1Data,at=3*(i-1),lty=1,add = T,col = 'green')
  vioplot(type_2Data,at=3*(i-1)+1,lty=1,add = T,col = 'red')
  wilcoxTest=wilcox.test(type_1Data,type_2Data)
  p=round(wilcoxTest$p.value,3)
  mx=max(c(type_1Data,type_2Data))
  lines(c(x=3*(i-1)+0.2,x=3*(i-1)+0.8),c(mx,mx))
  text(x=3*(i-1)+0.5, y=mx+0.02, labels=ifelse(p<0.001, paste0("p<0.001"), paste0("p=",p)), cex = 0.8)
  text(seq(1,64,3),-0.05,xpd = NA,labels=colnames(rt),cex = 1,srt = 45,pos=2)
}
dev.off()



######??????ѧ??: http://study.163.com/u/biowolf
######??????ѧ??: https://shop119322454.taobao.com
######??????ѧ??: http://www.biowolf.cn/
######???????䣺2740881706@qq.com
######????΢??: seqBio
######QQȺ:  259208034


# Libraries
library(ggplot2)
library(dplyr)
library(forcats)
library(hrbrthemes)
library(viridis)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/10_OneNumSevCatSubgroupsSevObs.csv", header=T, sep=",") %>%
  mutate(tip = round(tip/total_bill*100, 1))

# Grouped
data %>%
  mutate(day = fct_reorder(day, tip)) %>%
  mutate(day = factor(day, levels=c("Thur", "Fri", "Sat", "Sun"))) %>%
  ggplot(aes(fill=sex, y=tip, x=day)) + 
  geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent") +
  scale_fill_viridis(discrete=T, name="") +
  theme_ipsum()  +
  xlab("") +
  ylab("Tip (%)") +
  ylim(0,40)

l_data <- data %>%
  mutate(day = fct_reorder(day, tip)) %>%
  mutate(day = factor(day, levels=c("Thur", "Fri", "Sat", "Sun"))) 
