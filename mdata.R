# Only 900000 because Armand is scared
#text_data <- readLines("C:/Users/YNM724/Downloads/mnhn_observations_csv (3)/observations.csv", encoding = "Windows-1252")

# Write the file with UTF-8 encoding
#writeLines(text_data, "C:/Users/YNM724/Downloads/mnhn_observations_csv (3)/observations.csv", useBytes = TRUE)

mdata <- read.csv("C:/Users/YNM724/Downloads/mnhn_observations_csv (3)/observations.csv", encoding="latin1")

mdata <- mdata[complete.cases(mdata$Lat),]
mdata <- mdata[complete.cases(mdata$Long),]

# Keep only strict minimum
reduced <- mdata[,c("Observation_Key","Lat", "Long", "preferred")]

# 
