---
  title: "Indian Exports Dashboard"

output:
  
  flexdashboard::flex_dashboard:
  orientation: columns
vertical_layout: fill
---
  
  {r setup, include = FALSE}

library(flexdashboard)
library(tidyverse)
library(highcharter)

library(gt)
library(htmltools)
library(viridis)
library(readr)
library(dplyr)
library(plotly)
library(sf)
library(rnaturalearth)
library(countrycode)
library(ggrepel)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(wordcloud2)
library(treemap)
library(DT)
library(plotrix)

thm <- 
  hc_theme(
    colors = c("#1a6ecc", "#434348", "#90ed7d"),
    chart = list(
      backgroundColor = "transparent",
      style = list(fontFamily = "Source Sans Pro")
    ),
    xAxis = list(
      gridLineWidth = 1
    )
  )

custom_colors <- viridis::mako(n = 10)
custom_colors2 <- viridis::mako(n = 37)

fontStyle = list(
  family = "Source Sans Pro",
  size = 15,
  color = "black"
)

label = list(
  bgcolor = "#EEEEEE",
  bordercolor = "transparent",
  font = fontStyle
)



{r}

country_wise <- read_csv("countrywise_data_2021_22.csv")
five_years <- read_csv("countrywise_data_2017_22.csv")
total <- read_csv("total_figures_2017_22.csv")
product <- read_csv("top_export_commodities_2021_22.csv")
commodity <- read_csv("commodities_data_2021_22.csv")
state_wise <- read_csv("statewise_exports_2021_22.csv")




Column {.tabset .tabset-fade data-width=1000}
-----------------------------------------------------------------------
  
  
  
  ### Top Export Partners
  
  {r}

country_wise %>%
  group_by(Country) %>%
  summarise(Exports) %>%
  arrange(desc(Exports)) %>%
  head(10) %>%
  hchart('column', hcaes(x = Country, y = Exports, color = custom_colors)) %>%
  hc_add_theme(thm) %>%
  hc_tooltip(pointFormat = '<b>Exports: </b> {point.y} <br>' ) %>%
  hc_title(text = "Top 10 Export Trading Partners in 2021-22", 
           style = list(fontSize = '25px', fontWeight = 'bold')) %>%
  hc_subtitle(text = "Amount in USD millions",
              style = list(fontSize = '16px'))



### Top Import Partners

{r}

country_wise %>%
  group_by(Country) %>%
  summarise(Imports) %>%
  arrange(desc(Imports)) %>%
  head(10) %>%
  hchart('column', hcaes(x = Country, y = Imports, color = custom_colors)) %>%
  hc_add_theme(thm) %>%
  hc_tooltip(pointFormat = '<b>Imports: </b> {point.y} <br>' ) %>%
  hc_title(text = "Top 10 Import Trading Partners in 2021-22", 
           style = list(fontSize = '25px', fontWeight = 'bold')) %>%
  hc_subtitle(text = "Amount in USD millions",
              style = list(fontSize = '16px'))



### Trade Balance

{r}

data <- country_wise %>% mutate(hover = paste0(Country, "\nTrade Balance: $ ", (Exports-Imports), " millions"))

map <- plot_geo(data, locationmode = "ISO-3") %>% add_trace(locations = ~Code, z = ~(Exports-Imports), zmin = min(data$Exports - data$Imports), zmax = max(data$Exports - data$Imports), color = ~(Exports - Imports), text = ~hover, hoverinfo = 'text') %>% layout(title = "Trade Balance\n(2021-22)") %>% style(hoverlabel = label)

map



### Exports Text

{r}

wordcloud(c(country_wise$Country), freq = c(c(floor(country_wise$Exports))), random.color = TRUE, col = brewer.pal(9, "Blues"), scale = c(2,.5), min.freq = 1)



### Imports Text

{r}

wordcloud(c(country_wise$Country), freq = c(c(floor(country_wise$Imports))), random.color = TRUE, col = brewer.pal(9, "Blues"), scale = c(2,.5), min.freq = 1)



### Exports Data (2017-22)

{r}

data <- five_years[,c("Export", "Code", "Country", "Year")] %>% mutate(hover = paste0(Country, "\n$", Export, " millions"))

map <- plot_geo(data, locationmode = "ISO-3", frame = ~Year) %>% add_trace(locations = ~Code, z = ~Export, zmin = 0, zmax = max(data$Export), color = ~Export, text = ~hover, hoverinfo = 'text') %>% layout(title = "Exports Data\n(2017-22)") %>% style(hoverlabel = label)

map



### Imports Data (2017-22)

{r}

data <- five_years[,c("Import", "Code", "Country", "Year")] %>% mutate(hover= paste0(Country, "\n$", Import, " millions"))

map <- plot_geo(data, locationmode = "ISO-3", frame = ~Year) %>% add_trace(locations = ~Code, z = ~Import, zmin = 0, zmax = max(data$Import), color = ~Import,text = ~hover, hoverinfo = 'text') %>% layout(title = "Imports Data\n(2017-22)") %>% style(hoverlabel = label)

map



### Comparsion (2017-22)

{r}



ggplot(total, aes(x = Export, y = Import, color = Year)) +
  geom_point(size = 5, alpha = 0.3) +
  labs(x = 'Exports in US$ millions', y = 'Imports in US$ millions', title = 'Exports vs Imports (2017-22)') +
  theme_minimal() 




### Top Exports

{r}

data <- product[,c("Product", "Code", "Country", "Rank", "Amount")] %>% mutate(hover = paste0(Country, "\n", Product, "\n", "\n$", Amount, " millions"))

map <- plot_geo(data, locationmode = "ISO-3", frame = ~Rank) %>% add_trace(locations = ~Code, z = ~Amount, zmin = 0, zmax = max(data$Amount), color = ~Amount, text = ~hover, hoverinfo = 'text') %>% layout(title = "Top Ranked Export Commodities\n(2021-22)") %>% style(hoverlabel = label)

map



### Top 10 Export Commodities

{r}

data <- product %>% group_by(Product) %>% summarize(n = sum(Amount)) %>% arrange(desc(n)) %>% slice(1:10,)

data %>%
  hchart('column', hcaes(x = Product, y = n, color = custom_colors)) %>%
  hc_add_theme(thm) %>%
  hc_tooltip(pointFormat = '<b>Export Amount: </b> {point.y} <br>' ) %>%
  hc_title(text = "Top 10 Export Commodities for 2021-22", 
           style = list(fontSize = '25px', fontWeight = 'bold')) %>%
  hc_subtitle(text = "Amount in USD millions",
              style = list(fontSize = '16px'))



### Data Table of Commodities (2021-22)

{r}

data <- commodity %>% group_by(Product) %>% arrange(desc(Amount)) %>% summarise(Country, Amount)

datatable(data, filter = 'top', options = list(pageLength = 15), class = 'hover cell-border stripe')



### State Wise Exports

{r}

treemap(state_wise, index = "States", vSize = "Amount", type = "index", fontsize.labels = 10, fontcolor.labels = "white", fontface.labels = 2, bg.labels = "transparent", align.labels =c("center","center"), border.col = "black", border.lwds = 3, title = "Statewise Share of Exports (2021-22)", fontsize.title = 14, palette = "Set2")



### State Wise Share

{r}

pie_data <- state_wise[,c("States", "Amount")]


pie_inter <- pie_data %>% hchart("pie", hcaes(x = States, y = Amount, color = custom_colors2), dataLabels = list(enabled = TRUE, format = '{point.name}: {point.percentage:.2f}%')) %>% hc_title(text = "State Wise Share of Exports", style = list(fontSize = '25px', fontWeight = 'bold')) %>% hc_add_theme(thm) %>%
  hc_tooltip(pointFormat = 'Amount: {point.y} CR RS' ) %>%
  hc_subtitle(text = "For Financial Year 2021-22",
              style = list(fontSize = '16px'))

pie_inter