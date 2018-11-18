library(XML)
library(ggmap)


url <- paste("http://ws.bus.go.kr/api/rest/busRouteInfo/getBusRouteList?serviceKey=gH19buRy%2BiHenMjC6JkiwE6VEG1Mq1WChoYviIkZ3J5KsdshhG4N5MddOCsefPhYDOrfHH5ani2qhOR%2FiQRtpg%3D%3D&strSrch=146")

url

xmefile <- xmlParse(url)

xmefile

xmlRoot(xmefile)

xmlRoot

df <- xmlToDataFrame(getNodeSet(xmefile,"//itemList"))

head(df)

df_busRoute <- subset(df, busRouteNm==146)

df_busRoute

df_busRoute$busRouteId

url2 <-"http://ws.bus.go.kr/api/rest/buspos/getBusPosByRtid?serviceKey=gH19buRy%2BiHenMjC6JkiwE6VEG1Mq1WChoYviIkZ3J5KsdshhG4N5MddOCsefPhYDOrfHH5ani2qhOR%2FiQRtpg%3D%3D&busRouteId=100100025"

xmefile <- xmlParse(url2)

xmlRoot(xmefile)

df <- xmlToDataFrame(getNodeSet(xmefile,"//itemList"))

df

gpsX <- as.numeric(as.character(df$gpsX))

gpsY <- as.numeric(as.character(df$gpsY))


gc <-data.frame(lon=gpsX, lat=gpsY)

gc

gc1 <- head(gc, n=10)

gc1

cen <- c(mean(gc1$lon), mean(gc1$lat))

cen

key <- "AIzaSyDqdYTc1-xDy5H1K73yPoLNUD-W-qrgHeA"

map <- get_googlemap(center = cen , maptype = "roadmap" , zoom=15 , markers = gc1 , key = key)

ggmap(map)