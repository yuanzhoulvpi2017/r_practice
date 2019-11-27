
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, skimr, GGally, plotly, viridis, caret, randomForest, e1071, rpart, 
               xgboost, h2o, ggcorrplot, rpart.plot, corrgram, lightgbm, ggplot2, highcharter, 
                ggthemes, psych, scales, treemap, treemapify, repr, cowplot, magrittr, ggpubr,
               RColorBrewer, plotrix, ggrepel, forcats, reshape2, caTools, tree, rattle)

library(rattle)
rattle()
