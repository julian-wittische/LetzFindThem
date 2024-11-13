library(sf)
source("config.R")

load_data <- function(path) {
    (files <- list.files(DATA_PATH, full.names = TRUE, pattern="*.csv"))
    mdata <- do.call(rbind, lapply(files, function(x) read.csv(x, encoding="latin1")))

    # Keep only strict minimum
    mdata <- mdata[,c("Observation_Key","Lat", "Long", "preferred", "Taxon_Kingdom", "Taxon_Phylum", "Taxon_Class", "Taxon_Order", "Taxon_Family", "Taxon_Genus")]

    mdata <- mdata[, c(1,3,2,4,5,6,7,8,9,10)]
    colnames(mdata) <- c("name", "lng", "lat", "species", "Taxon_Kingdom", "Taxon_Phylum", "Taxon_Class", "Taxon_Order", "Taxon_Family", "Taxon_Genus")

    mdata <- mdata[complete.cases(mdata$lng),]
    mdata <- mdata[complete.cases(mdata$lat),]
    mdata <- mdata[complete.cases(mdata$species),]

    all_points <- st_as_sf(mdata, coords = c("lng", "lat"), crs = 4326)
    all_points <- st_transform(all_points, 2169)
    all_points
}
