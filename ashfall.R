library(XML)
library(ggmap)

xml <- xmlToList("Aso151207T2000.xml")

# 火山座標
volcano_name<- xml$Body$VolcanoInfo$Item$Areas$Area$Name
volcano_str <- xml$Body$VolcanoInfo$Item$Areas$Area$Coordinate$text
# DMM形式の座標をDEGに変換
volcano_lat <- as.numeric(substr(volcano_str, 1, 3)) + (as.numeric(substr(volcano_str, 4, 8)))/60
volcano_lon <- as.numeric(substr(volcano_str, 9,12)) + (as.numeric(substr(volcano_str,13,17)))/60


# 1つ目の予報(予報 3時間後)
i <- 1
info <- xml$Body$AshInfos[i]
## 予報期間
duration <- paste("From",info$AshInfo$StartTime, "to", info$AshInfo$EndTime)


## 降灰の範囲
ashPolygons <- list()
for(i in 1:(length(info$AshInfo[3]$Item$Kind$Property) - 2)){
  polygonStr <- info$AshInfo[3]$Item$Kind$Property[i]$Polygon$text
  
  ### 文字列を分割
  polygonStr <- unlist(strsplit(polygonStr, "/"))
  ### 文字列を座標に変換
  lats <- as.numeric(substring(polygonStr, 1,8))
  lons <- as.numeric(substring(polygonStr, 9,17))
  ### ポリゴンを作成
  ashPolygon <- data.frame(lon=lons, lat=lats)
  
  ### リストに追加
  ashPolygons <- append(ashPolygons, geom_polygon(data = ashPolygon, colour="gray30", alpha=0.6, size = 1))
}

## 降灰の方向
plumeDirection <- paste0(info$AshInfo[3]$Item$Kind$Property$PlumeDirection$.attrs["type"],": ",
                        info$AshInfo[3]$Item$Kind$Property$PlumeDirection$.attrs["description"])
## 降灰の到達距離
distance <- paste0(info$AshInfo[3]$Item$Kind$Property$Distance$.attrs["type"],": ",
                   info$AshInfo[3]$Item$Kind$Property$Distance$text,
                   info$AshInfo[3]$Item$Kind$Property$Distance$.attrs["unit"])

## 図の中心を山頂に指定して取得
map <- get_map(c(volcano_lon, volcano_lat), language = "ja", maptype = "roadmap", zoom = 9)

## 作図
g <- ggmap(map)
### タイトル
g <- g + ggtitle(paste0(volcano_name, "\n", duration, "\n\n", plumeDirection, "\n", distance ))
### 山頂
g <- g + geom_point(x = volcano_lon, y = volcano_lat, shape = 17, size = 4)
### 降灰エリア
for(m in ashPolygons){g <- g+m}
### プロット
g



## 小さな噴石の到達距離
distance <- paste0(info$AshInfo[4]$Item$Kind$Property$Distance$.attrs["type"],": ",
                   info$AshInfo[4]$Item$Kind$Property$Distance$text,
                   info$AshInfo[4]$Item$Kind$Property$Distance$.attrs["unit"])
## 小さな噴石の落下方向
plumeDirection <- paste0(info$AshInfo[4]$Item$Kind$Property$PlumeDirection$.attrs["type"],": ",
                         info$AshInfo[4]$Item$Kind$Property$PlumeDirection$.attrs["description"])

## 小さな噴石の落下範囲
polygonStr <- info$AshInfo[4]$Item$Kind$Property$Polygon$text
### 文字列を分割
polygonStr <- unlist(strsplit(polygonStr, "/"))
### 文字列を座標に変換
lats <- as.numeric(substring(polygonStr, 1,8))
lons <- as.numeric(substring(polygonStr, 9,17))
### ポリゴンを作成
ashPolygon <- data.frame(lon=lons, lat=lats)


## 図の中心を山頂に指定して取得
map <- get_map(c(volcano_lon, volcano_lat), language = "ja", maptype = "roadmap", zoom = 12)

## 作図
g <- ggmap(map)
### タイトル
g <- g + ggtitle(paste0(volcano_name, "\n", duration, "\n\n", plumeDirection, "\n", distance ))
### 落石エリア
g <- g + geom_polygon(data = ashPolygon, colour="gray0", fill="red", alpha=0.4, size = 1)
### 山頂
g <- g + geom_point(x = volcano_lon, y = volcano_lat, shape = 17, size = 4)
### プロット
g
