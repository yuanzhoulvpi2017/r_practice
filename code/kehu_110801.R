library(readtext)
library(networkD3)
library(stringr)
library(tidyverse)

test <- readtext("~/Rcode/zfkc.txt", encoding = "utf-8")



s1 <- unlist(str_split(test$text, "\n\n"))

people_name <- c()
for (i in seq(1, 1145)) {
  people_name <- c(people_name, str_match(s1[i], "(.*?)：")[2])
  cat("i -->", i, " ******", str_match(s1[i], "(.*?)：")[2], '\n')
}

tidy_name <- unique(people_name)
all_people <-
  tidy_name[c(
    -1,
    -2,
    -3,
    -4,
    -5,
    -6,
    -7,
    -9,
    -10,
    -13,
    -14,
    -23,-30,
    -32,
    -34,
    -35,
    -36,
    -43,
    -44,
    -45,
    -46,
    -47,
    -52
  )]
all_people

empty_list <- list()
temp_data <- data.frame(matrix(0, ncol = 30, nrow = 1145))
names(temp_data) <- c("id", all_people)
for (idx in 1:1145) {
  temp_data[idx, 2:30] <- unlist(lapply(
    all_people,
    FUN = function(i) {
      return(!is.na(str_match(s1[idx], i)[1]))
    }
  ))
  cat(all_people[unlist(lapply(
    all_people,
    FUN = function(i) {
      return(!is.na(str_match(s1[idx], i)[1]))
    }
  ))], "\n")
  empty_list[[idx]] <- all_people[unlist(lapply(
    all_people,
    FUN = function(i) {
      return(!is.na(str_match(s1[idx], i)[1]))
    }
  ))]
}

clean_list <- list()
new_idx <- 0
for (i in 1:1145) {
  if (length(empty_list[[i]]) == 2) {
    new_idx <- new_idx + 1
    clean_list[[new_idx]] <- empty_list[[i]]
  }
  
}

name1 <-
  unlist(lapply(
    c(1:length(clean_list)),
    FUN = function(i) {
      clean_list[[i]][1]
    }
  ))
name2 <-
  unlist(lapply(
    c(1:length(clean_list)),
    FUN = function(i) {
      clean_list[[i]][2]
    }
  ))

relation_data <- data.frame("name1" = name1,
                            "name2" = name2)
relation_data$name1 <- as.character(relation_data$name1)
relation_data$name2 <- as.character(relation_data$name2)
relation_data <-
  relation_data[!(relation_data['name1'] == relation_data["name2"]),]

relation_data['com'] <-
  str_c(relation_data$name1, relation_data$name2, sep = "...")


####################################################################################
head(relation_data)
names(table(relation_data$com))
result_people_f <- data.frame(table(relation_data$com))
head(result_people_f)
result_people_f$name1 <-
  unlist(str_split(result_people_f$Var1, "\\..."))[seq(from = 1, to = 126, by = 2)]
result_people_f$name2 <-
  unlist(str_split(result_people_f$Var1, "\\..."))[seq(from = 2, to = 126, by = 2)]

src <- result_people_f$name1
target <- result_people_f$name2
networkData <- data.frame(src, target, stringsAsFactors = FALSE)
nodes <-
  data.frame(name = unique(c(src, target)), stringsAsFactors = FALSE)
nodes$id <- 0:(nrow(nodes) - 1)
# create a data frame of the edges that uses id 0:9 instead of their names
edges <- networkData %>%
  left_join(nodes, by = c("src" = "name")) %>%
  select(-src) %>%
  rename(source = id) %>%
  left_join(nodes, by = c("target" = "name")) %>%
  select(-target) %>%
  rename(target = id)

edges$width <- result_people_f$Freq

# make a grouping variable that will match to colours
nodes$group <- ifelse(nodes$name %in% src, "lions", "tigers")

ColourScale <- 'd3.scaleOrdinal()
            .domain(["lions", "tigers"])
           .range(["#FF6900", "#694489"]);'
# simple with default colours
forceNetwork(
  Links = edges,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  NodeID = "name",
  Group = "group",
  Value = "width",
  opacity = 0.9,
  charge = -400,
  zoom = TRUE,
  opacityNoHover = TRUE,
  linkDistance =
    JS(
      'function(){d3.select("body").style("background-color", "#9EA0A1"); return 50;}'
    ),
  colourScale = JS(ColourScale)
)
