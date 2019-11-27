library(plotly)

p <- plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length)

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
library(plotly)

trace_0 <- rnorm(100, mean = 5)
trace_1 <- rnorm(100, mean = 0)
trace_2 <- rnorm(100, mean = -5)
x <- c(1:100)

data <- data.frame(x, trace_0, trace_1, trace_2)

p <- plot_ly(data, x = ~x, y = ~trace_0, name = 'trace 0', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers') %>%
  add_trace(y = ~trace_2, name = 'trace 2', mode = 'markers')
p

d1 <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/coffee-flavors.csv')
d2 <- read.csv('https://raw.githubusercontent.com/plotly/datasets/718417069ead87650b90472464c7565dc8c2cb1c/sunburst-coffee-flavors-complete.csv')
plot_ly() %>%
  add_trace(
    ids = d1$ids,
    labels = d1$labels,
    parents = d1$parents,
    type = 'sunburst',
    maxdepth = 2,
    domain = list(column = 0)
  ) %>%
  add_trace(
    ids = d2$ids,
    labels = d2$labels,
    parents = d2$parents,
    type = 'sunburst',
    maxdepth = 3,
    domain = list(column = 1)
  ) %>%
  layout(
    grid = list(columns =2, rows = 1),
    margin = list(l = 0, r = 0, b = 0, t = 0),
    sunburstcolorway = c(
      "#636efa","#EF553B","#00cc96","#ab63fa","#19d3f3",
      "#e763fa", "#FECB52","#FFA15A","#FF6692","#B6E880"
    ),
    extendsunburstcolors = TRUE)




library(plotly)

data <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/gapminderDataFiveYear.csv")

data_2007 <- data[which(data$year == 2007),]
data_2007 <- data_2007[order(data_2007$continent, data_2007$country),]
data_2007$size <- data_2007$pop
colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951')

plot_ly(data_2007, x = ~gdpPercap, y = ~lifeExp, z = ~pop, color = ~continent, size = ~size, colors = colors,
        marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 150),
        text = ~paste('Country:', country, '<br>Life Expectancy:', lifeExp, '<br>GDP:', gdpPercap,
                      '<br>Pop.:', pop)) %>%
  layout(title = 'Life Expectancy v. Per Capita GDP, 2007',
         scene = list(xaxis = list(title = 'GDP per capita (2000 dollars)',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   range = c(2.003297660701705, 5.191505530708712),
                                   type = 'log',
                                   zerolinewidth = 1,
                                   ticklen = 5,
                                   gridwidth = 2),
                      yaxis = list(title = 'Life Expectancy (years)',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   range = c(36.12621671352166, 91.72921793264332),
                                   zerolinewidth = 1,
                                   ticklen = 5,
                                   gridwith = 2),
                      zaxis = list(title = 'Population',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   type = 'log',
                                   zerolinewidth = 1,
                                   ticklen = 5,
                                   gridwith = 2)),
         paper_bgcolor = 'rgb(243, 243, 243)',
         plot_bgcolor = 'rgb(243, 243, 243)')



#sunburst
d1 <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/coffee-flavors.csv')
d2 <- read.csv('https://raw.githubusercontent.com/plotly/datasets/718417069ead87650b90472464c7565dc8c2cb1c/sunburst-coffee-flavors-complete.csv')
plot_ly() %>%
  add_trace(
    ids = d1$ids,
    labels = d1$labels,
    parents = d1$parents,
    type = 'sunburst',
    maxdepth = 2,
    domain = list(column = 0)
  ) %>%
  add_trace(
    ids = d2$ids,
    labels = d2$labels,
    parents = d2$parents,
    type = 'sunburst',
    maxdepth = 3,
    domain = list(column = 1)
  ) %>%
  layout(
    grid = list(columns =2, rows = 1),
    margin = list(l = 0, r = 0, b = 0, t = 0),
    sunburstcolorway = c(
      "#636efa","#EF553B","#00cc96","#ab63fa","#19d3f3",
      "#e763fa", "#FECB52","#FFA15A","#FF6692","#B6E880"
    ),
    extendsunburstcolors = TRUE) 



plot_ly() %>%
  add_trace(
    ids = d2$ids,
    labels = d2$labels,
    parents = d2$parents,
    type = 'sunburst',
    maxdepth = 2,
    domain = list(column = 1)
  ) 


#make a sunburst by my data
d_t <- read.csv("test_sun_burst.csv")
plot_ly() %>%
  add_trace(
    ids = d_t$ids,
    labels = d_t$labels,
    parents = d_t$parents,
    type = "sunburst",
    maxdepth = 10
  )

###############################################3
#bubble map
library(plotly)
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_us_cities.csv')

df$q <- with(df, cut(pop, quantile(pop)))
levels(df$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
df$q <- as.ordered(df$q)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

plot_geo(df, locationmode = 'USA-states', sizes = c(1, 250)) %>%
  add_markers(
    x = ~lon, y = ~lat, size = ~pop, color = ~q, hoverinfo = "text",
    text = ~paste(df$name, "<br />", df$pop/1e6, " million")
  ) %>%
  layout(title = '2014 US city populations<br>(Click legend to toggle)', geo = g)
