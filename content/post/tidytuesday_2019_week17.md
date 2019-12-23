---
title: "R TidyTuesday 2019 Week 17"
author: "Youngseok Hahm"
date: 2019-12-20
tags: [R, TidyTuesday]
draft: false
---



# Call library

```r
library(tidyverse)
library(gganimate)
library(plotly)
```


# Get the Data

```r
dog_descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')
```

# Data Cleaning

```r
# 33 observations with wrong data entry
# data from column 26 ('photo') should be moved to right by one
temp = dog_descriptions %>% filter(status != 'adoptable')
head(temp[,25:28])
```

```
## # A tibble: 6 x 4
##   tags  photo     status                   posted    
##   <chr> <chr>     <chr>                    <chr>     
## 1 <NA>  adoptable 2018-04-05T05:18:31+0000 Las Vegas 
## 2 <NA>  adoptable 2017-05-26T21:43:16+0000 Chandler  
## 3 <NA>  adoptable 2019-09-01T15:12:06+0000 Albany    
## 4 <NA>  adoptable 2019-08-06T12:15:58+0000 Albany    
## 5 <NA>  adoptable 2019-07-18T14:20:58+0000 Albany    
## 6 <NA>  adoptable 2019-07-11T20:34:42+0000 Saugerties
```

```r
temp[, 27:34] = temp[, 26:33]
temp[,26] = NA
head(temp[,25:28])
```

```
## # A tibble: 6 x 4
##   tags  photo status    posted                  
##   <chr> <lgl> <chr>     <chr>                   
## 1 <NA>  NA    adoptable 2018-04-05T05:18:31+0000
## 2 <NA>  NA    adoptable 2017-05-26T21:43:16+0000
## 3 <NA>  NA    adoptable 2019-09-01T15:12:06+0000
## 4 <NA>  NA    adoptable 2019-08-06T12:15:58+0000
## 5 <NA>  NA    adoptable 2019-07-18T14:20:58+0000
## 6 <NA>  NA    adoptable 2019-07-11T20:34:42+0000
```

```r
# Update the original dataset
dog_descriptions[match( temp$url, dog_descriptions$url ), ] <- temp
```


```r
dog_descriptions %>% 
  group_by(contact_state) %>%
  summarize(total = n()) %>%
  top_n(20, total) %>%
  select(contact_state) -> top_20_states
# Top 20 states with most adoptable dogs available by sex
dog_descriptions %>%
  filter(contact_state %in% top_20_states$contact_state ) %>%
  group_by(contact_state, sex) %>%
  summarize(count = n()) %>%
  plot_ly(x = ~contact_state, y = ~count, type = "bar", color = ~sex) %>%
  layout(
    title = "Top 20 states with most adoptable dogs available by sex",
    xaxis = list( title = "State abbreviation"),
    yaxis = list( title = "Number of adoptable dogs")
  )
```


<!--html_preserve--><div id="htmlwidget-fc5428a03a92efbaf401" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-fc5428a03a92efbaf401">{"x":{"visdat":{"2603393f98":["function () ","plotlyVisDat"]},"cur_data":"2603393f98","attrs":{"2603393f98":{"x":{},"y":{},"color":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Top 20 states with most adoptable dogs available by sex","xaxis":{"domain":[0,1],"automargin":true,"title":"State abbreviation","type":"category","categoryorder":"array","categoryarray":["AL","AZ","CA","CO","CT","FL","GA","IN","KY","MD","NC","NJ","NY","OH","OK","PA","SC","TN","VA","WA"]},"yaxis":{"domain":[0,1],"automargin":true,"title":"Number of adoptable dogs"},"hovermode":"closest","showlegend":true},"source":"A","config":{"showSendToCloud":false},"data":[{"x":["AL","AZ","CA","CO","CT","FL","GA","IN","KY","MD","NC","NJ","NY","OH","OK","PA","SC","TN","VA","WA"],"y":[716,1067,777,912,682,1279,1727,851,507,679,1282,1483,1945,1234,814,1371,759,826,1451,598],"type":"bar","name":"Female","marker":{"color":"rgba(102,194,165,1)","line":{"color":"rgba(102,194,165,1)"}},"textfont":{"color":"rgba(102,194,165,1)"},"error_y":{"color":"rgba(102,194,165,1)"},"error_x":{"color":"rgba(102,194,165,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["AL","AZ","CA","CO","CT","FL","GA","IN","KY","MD","NC","NJ","NY","OH","OK","PA","SC","TN","VA","WA"],"y":[712,1181,887,861,740,1380,1752,1029,616,815,1345,1539,2062,1439,822,1454,859,945,1610,686],"type":"bar","name":"Male","marker":{"color":"rgba(252,141,98,1)","line":{"color":"rgba(252,141,98,1)"}},"textfont":{"color":"rgba(252,141,98,1)"},"error_y":{"color":"rgba(252,141,98,1)"},"error_x":{"color":"rgba(252,141,98,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["AZ"],"y":[1],"type":"bar","name":"Unknown","marker":{"color":"rgba(141,160,203,1)","line":{"color":"rgba(141,160,203,1)"}},"textfont":{"color":"rgba(141,160,203,1)"},"error_y":{"color":"rgba(141,160,203,1)"},"error_x":{"color":"rgba(141,160,203,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://youngseokhahm.com/content/post"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

```r
    
# Top 20 states with most adoptable dogs available by age
dog_descriptions %>%
  filter(contact_state %in% top_20_states$contact_state ) %>%
  group_by(contact_state, age) %>%
  summarize(count = n()) %>%
  plot_ly(x = ~contact_state, y = ~count, type = "bar", color = ~age) %>%
  layout(
    title = "Top 20 states with most adoptable dogs available by age",
    xaxis = list( title = "State abbreviation"),
    yaxis = list( title = "Number of adoptable dogs")
  )
```


<iframe width="900" height="800" frameborder="0" scrolling="no" src="//plot.ly/~terryhahm/1.embed"></iframe>

<!--html_preserve--><div id="htmlwidget-0866704e3f75277709ba" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-0866704e3f75277709ba">{"x":{"visdat":{"2601b55697f":["function () ","plotlyVisDat"]},"cur_data":"2601b55697f","attrs":{"2601b55697f":{"x":{},"y":{},"color":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Top 20 states with most adoptable dogs available by age","xaxis":{"domain":[0,1],"automargin":true,"title":"State abbreviation","type":"category","categoryorder":"array","categoryarray":["AL","AZ","CA","CO","CT","FL","GA","IN","KY","MD","NC","NJ","NY","OH","OK","PA","SC","TN","VA","WA"]},"yaxis":{"domain":[0,1],"automargin":true,"title":"Number of adoptable dogs"},"hovermode":"closest","showlegend":true},"source":"A","config":{"showSendToCloud":false},"data":[{"x":["AL","AZ","CA","CO","CT","FL","GA","IN","KY","MD","NC","NJ","NY","OH","OK","PA","SC","TN","VA","WA"],"y":[665,1109,833,737,482,1365,1869,917,579,640,1216,1230,1752,1362,761,1272,935,860,1661,587],"type":"bar","name":"Adult","marker":{"color":"rgba(102,194,165,1)","line":{"color":"rgba(102,194,165,1)"}},"textfont":{"color":"rgba(102,194,165,1)"},"error_y":{"color":"rgba(102,194,165,1)"},"error_x":{"color":"rgba(102,194,165,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["AL","AZ","CA","CO","CT","FL","GA","IN","KY","MD","NC","NJ","NY","OH","OK","PA","SC","TN","VA","WA"],"y":[295,255,199,427,408,333,368,271,146,215,450,675,683,319,242,575,255,241,352,240],"type":"bar","name":"Baby","marker":{"color":"rgba(252,141,98,1)","line":{"color":"rgba(252,141,98,1)"}},"textfont":{"color":"rgba(252,141,98,1)"},"error_y":{"color":"rgba(252,141,98,1)"},"error_x":{"color":"rgba(252,141,98,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["AL","AZ","CA","CO","CT","FL","GA","IN","KY","MD","NC","NJ","NY","OH","OK","PA","SC","TN","VA","WA"],"y":[81,224,153,140,98,234,228,162,85,165,140,254,361,195,127,215,82,182,286,56],"type":"bar","name":"Senior","marker":{"color":"rgba(141,160,203,1)","line":{"color":"rgba(141,160,203,1)"}},"textfont":{"color":"rgba(141,160,203,1)"},"error_y":{"color":"rgba(141,160,203,1)"},"error_x":{"color":"rgba(141,160,203,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["AL","AZ","CA","CO","CT","FL","GA","IN","KY","MD","NC","NJ","NY","OH","OK","PA","SC","TN","VA","WA"],"y":[387,661,479,469,434,727,1014,530,313,474,821,863,1211,797,506,763,346,488,762,401],"type":"bar","name":"Young","marker":{"color":"rgba(231,138,195,1)","line":{"color":"rgba(231,138,195,1)"}},"textfont":{"color":"rgba(231,138,195,1)"},"error_y":{"color":"rgba(231,138,195,1)"},"error_x":{"color":"rgba(231,138,195,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://youngseokhahm.com"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->



```r
scale_graph = function(x) {
  (x - min(x))/(max(x)-min(x))
}
dog_descriptions %>%
  mutate( posted_date = str_extract(posted, "([^T]+)") ) %>%
  select(posted_date) %>%
  group_by(posted_date) %>%
  summarize(count = n()) %>%
  mutate( year = strsplit(posted_date, '-') %>% map_chr(., 1),
          month = months(as.Date(posted_date)),
          count = scale_graph(count)) %>%
  filter( year == '2019' ) -> data_visualization
data_visualization %>%
  group_by(month) %>%
  summarize(start=min(posted_date), end=max(posted_date)) %>%
  rowwise() %>%
  mutate(month_label = months(as.Date(start)),
         mid_point = as.character( as.Date(start) + floor((as.Date(end) - as.Date(start))/2))) -> data_label
p <- ggplot(data = data_visualization, aes(x = posted_date, y = count, fill=month )) + 
  geom_bar(stat="identity", alpha = 0.5) +
  ylim(-0.5,1) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
  ) +
  coord_polar(start = 0) +
  geom_text(data = data_label, aes(x = mid_point, y = -0.1, label = month_label), colour = "black", alpha=0.8, size=1.4, inherit.aes = FALSE) 
p + labs(title = "Adoptable dogs posted by month in 2019 (scaled)")
```

{{< figure src="/static/img/tidytuesday_2019_week17_1.png" title="Circular Barplot" >}}

![](/static/img/tidytuesday_2019_week17_1.png)<!-- -->
