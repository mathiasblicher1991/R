# Develop an actual function for it

geograk <- function(data, bbox, countries, colors){
  
  # boundering box
  xlim <- bbox$x
  ylim <- bbox$y
  
  # colors = land color, sea color
  col_sea <- colors[1]
  col_land <- colors[2]
  
  # base plot
  plot(1, type="n", xlim = xlim, ylim = ylim, xlab = "", ylab = "",
       xaxt = "n", yaxt = "n", asp = 1.73)
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = col_sea)
  
  # add countries
  for(j in 1:length(countries)){
    
    x <- data[countries[j]][[1]][[1]]
    for(k in 1:length(x)){
      polygon(x[[k]][[1]][,1],x[[k]][[1]][,2],col=col_land)
    }
    
  }
  
}
