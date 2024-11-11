load_data <- function(path) {
    mdata <- read.csv(path, encoding="latin1")

    mdata <- mdata[complete.cases(mdata$preferred),]
    mdata <- mdata[complete.cases(mdata$Lat),]
    mdata <- mdata[complete.cases(mdata$Long),]

    # Keep only strict minimum
    reduced <- mdata[,c("Observation_Key","Lat", "Long", "preferred")]

    reduced <- reduced[, c(1,3,2,4)]
    colnames(reduced) <- c("name", "lng", "lat", "species")

    all_points <- st_as_sf(reduced, coords = c("lng", "lat"), crs = 4326)
    all_points <- st_transform(all_points, 2169)
    all_points
}
