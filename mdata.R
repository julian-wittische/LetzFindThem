mdata <- read.csv("data/observations.csv", encoding="latin1")

mdata <- mdata[complete.cases(mdata$preferred),]
mdata <- mdata[complete.cases(mdata$Lat),]
mdata <- mdata[complete.cases(mdata$Long),]

# Keep only strict minimum
reduced <- mdata[,c("Observation_Key","Lat", "Long", "preferred")]
