library(raster)
library(EpiWeek)

##### Paho Graph parser
Country <- "Anguilla"

## y lim = 6
## x lim = 6 to 5 = 46

path <- list.files(pattern = ".tiff",full.names = T)

ras <- raster(path)

hist(ras)

tb <- table(values(ras))
plot(ras)

ras[values(ras) >150 | values(ras)== 0] <- -999999

### find the y value of a column 

ras_ag <- raster(ncols = 52, nrows = 6, xmn = 0, xmx = 1363,ymn = 0, ymx = 600)

extr_shp <- rasterToPolygons(x = ras_ag)

plot(ras)
plot(extr_shp,add = T)


Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

extr <- extract(x = ras,y = extr_shp, fun = Mode, na.rm = T, df = T)

### rep each row by number of columns

extr$column <- rep(1:52,6)
extr$week <- rep(c(6:52,1:5),6)
extr$year <- rep(c(rep(2016, 47),rep(2017,5)),6)
extr$row <- rep(6:1,each= 52)

summary(extr)


## find max row value for a given column  

susp <- extr[extr$PAHO_AGL ==149,]

sus_v <-  susp[!duplicated(susp$column),]


df.ew<- epiweekToDate(year = sus_v$year, weekno = sus_v$week)

sus_v$EW_date <- df.ew$d0


sus_v$EW_date_end <- df.ew$d1


