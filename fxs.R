library(ggrepel)
library(rlang)


scat<-function(data, 
               xx = landArea, 
               yy = cum_cases, 
               lab.x = "land Area (Km^2)",
               lab.y = "cumulative cases",
               tit = ""){
  ggplot(data, aes({{xx}}, {{yy}}, label = country)) + 
    geom_point(aes(size = cum_deaths), color = ifelse(data$GeoId == "PT", "red", "grey50")) + 
    geom_text_repel(segment.size = 0.1, size = 3) + 
    labs(x = lab.x,
         y = lab.y, 
         title = tit) +
    theme_bw()
}


