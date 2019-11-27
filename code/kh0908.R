#Collapse text by group in data frame
library(dplyr)
df <- read.table("ceshi1.txt", header = TRUE, sep = '\t', 
                 fileEncoding = "gbk", stringsAsFactors = FALSE)
head(df, 8)
new <- df %>% group_by(SheetID) %>%
  summarise(text = paste(unique(CateName), collapse = ","))

write.csv(new, file = 'test.csv', row.names = FALSE)

head(new)

#
new1 <- as.data.frame(sapply(unique(df$SheetID), function(x){
  paste0(unique(subset(df, SheetID == x)$CateName), collapse = ",")
}))
#
#
library(data.table)
dt <- as.data.table(df)
new2 <- dt[, list(text = paste(unique(CateName), collapse = ",")), by = SheetID]
head(new2)

#stackoverflow
df <- read.table(header = TRUE, text = "
                 group text
                 a a1
                 a a2
                 a a3
                 b b1
                 b b2
                 b b4
                 c c2 
                 c c1
                 c c4
                 c c6
                 c c7")
df
#现在进行简单的操作
sapply(unique(df$group), function(x) {
  paste(df[df$group == x, "text"], collapse = ",")
})
#aggragate
aggregate(df$text, list(df$group), paste, collapse = ",")
aggregate(text ~ group, data = df, FUN = paste, collapse = ",")

library(plyr)
ddply(df, .(group), summarise, text = paste(text, collapse = ","))

library(data.table)
dt <- as.data.table(df)
dt[, list(text = paste(text, collapse = ",")), by = group]

library(dplyr)
df %>% 
  group_by(group) %>%
  summarise(t = paste(text, collapse = ","))
