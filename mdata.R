load_data <- function(path) {
    files <- list.files(path, full.names = TRUE, pattern="*.csv")

    mdata <- do.call(rbind, lapply(files, function(x) read.csv(x, encoding="latin1")))

    # Keep only strict minimum
    mdata <- mdata[,c("Lat", "Long", "preferred",
                      "Taxon_Kingdom", "Taxon_Phylum",
                      "Taxon_Class", "Taxon_Order",
                      "Taxon_Family", "Taxon_Genus")]

    mdata <- mdata[complete.cases(mdata$Lat),]
    mdata <- mdata[complete.cases(mdata$Long),]
    mdata <- mdata[complete.cases(mdata$preferred),]

    all_points <- st_as_sf(mdata, coords = c("Long", "Lat"),
                           crs = 4326)
    all_points <- st_transform(all_points, 2169)
    all_points
}
