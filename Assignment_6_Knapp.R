library("factoextra")

centers <- read.csv('nhl-stats.csv')

colnames(centers) <- c('Center Name', 'Team', 'Pos', 'Games', 'G', 'A', 'Pts', 'PlusMinus', 'PIM', 'SOG', 'GWG', 'PPG', 'PPA', 'SHG', 'SHA', 'Hits', 'BS')

centers <- centers[-1,]
centers <- centers[, -3]
str(centers)

centers$Games  <- as.numeric(centers$Games)
centers$G  <- as.numeric(centers$G)
centers$A  <- as.numeric(centers$A)
centers$Pts  <- as.numeric(centers$Pts)
centers$PlusMinus  <- as.numeric(centers$PlusMinus)
centers$PIM  <- as.numeric(centers$PIM)
centers$SOG  <- as.numeric(centers$SOG)
centers$GWG  <- as.numeric(centers$GWG)
centers$PPG  <- as.numeric(centers$PPG)
centers$PPA  <- as.numeric(centers$PPA)
centers$SHG  <- as.numeric(centers$SHG)
centers$SHA  <- as.numeric(centers$SHA)
centers$Hits  <- as.numeric(centers$Hits)
centers$BS  <- as.numeric(centers$BS)
str(centers)

set.seed(12345)

features <- centers[4:16]

features_c <- as.data.frame(lapply(features, scale))

km.res <- kmeans(features_c, 4, nstart = 25)

print(km.res)

km.res$cluster

km.res$size

km.res$centers

fviz_cluster(km.res, data = features_c,
             palette = c("#32FF44", "#FFF532", "#FF9232", "#FF4332"),
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot"
)
