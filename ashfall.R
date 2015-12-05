library(XML)
xml <- xmlToList("./aso.xml")

library(ggmap)

# 1つ目の予報
xml$Body$AshInfos[1]$AshInfo
## 降灰の範囲
xml$Body$AshInfos[1]$AshInfo$Item[1]$Kind$Property[1]$Polygon$text

## 小さな隕石の落下範囲

polygonStr <- xml$Body$AshInfos[1]$AshInfo$Item$Kind$Property$Polygon$text

### 文字列を分割
polygonStr <- unlist(strsplit(polygonStr, "/"))
### 文字列を座標に変換
lats <- as.numeric(substring(polygonStr, 1,8))
lons <- as.numeric(substring(polygonStr, 9,17))
ashPolygon <- data.frame(lon=lons, lat=lats)
rm(polygonStr,lats,lons)

# 図の中心
map <- get_map(as.numeric(ashPolygon[1,]), language = "ja", maptype = "roadmap")
ggmap(map) + 
  geom_polygon(data = ashPolygon, colour="gray30", alpha=0.6)