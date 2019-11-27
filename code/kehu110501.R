library(tidyverse)
library(ggsci)
library(lubridate)

googleplaystore <- read_csv("google-play-store-apps/googleplaystore.csv", 
     col_types = cols(`Android Ver` = col_factor(levels = c()), 
         Category = col_factor(levels = c()), 
         Installs = col_number(), `Last Updated` = col_character(), 
        Size = col_number(), Type = col_factor(levels = c())), na = "NA")

googleplaystore$`Last Updated` <- mdy(googleplaystore$`Last Updated`)
which(!complete.cases(googleplaystore$`Last Updated`))
googleplaystore[10473, ]


#Category
ggplot(data = googleplaystore,aes(Category, color = Type, fill = Type)) + 
  geom_bar(position = "dodge")  + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_npg()


#most popular category
data_most_category <- googleplaystore %>% 
  group_by(Category) %>% 
  summarise(TotalInstalls = sum(Installs)) %>%
  arrange(-TotalInstalls) 
ggplot(data = data_most_category,mapping = aes(x = Category, y = TotalInstalls, colour = Category)) + 
  geom_point(data = data_most_category,aes(size = TotalInstalls)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_npg()


##application size
names(googleplaystore)
ggplot(data = googleplaystore, aes(x = Category,y = Size, color = Category)) + 
  geom_boxplot(na.rm = TRUE) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_npg()


type_size_data <- googleplaystore %>% filter(Type == "Free" | Type == "Paid")
ggplot(data = type_size_data, aes(x = Size, color = Type)) + 
  geom_density() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_npg()

#Install 
googleplaystore[which(!complete.cases(googleplaystore$Installs)),'Installs'] = 0
install_data <- googleplaystore %>%
  group_by(Installs.group = cut(Installs, breaks = seq(0, 1000000000, by = 10000))) %>% summarise(n = n())
(install_data)

ggplot(data = install_data, aes(x = "", y = n, fill = Installs.group)) + 
  geom_bar(stat = "identity") + coord_polar(theta = "y") + scale_fill_npg()


#genres
genres_data <- googleplaystore %>%
  group_by(Genres) %>%
  summarise(all_install = sum(Installs)) %>% arrange(-all_install)


genres_data$Genres <- ordered(genres_data$Genres, levels = genres_data$Genres)

ggplot(data = genres_data,aes(x = Genres, y = all_install)) + 
  geom_bar(stat = "identity")  + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_npg()

