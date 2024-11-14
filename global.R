source("config.R")
source("taxon_info.R")
source("mdata.R")

library(rtree)

# Load data from .csv files
all_points <- load_data(DATA_PATH)

# Build search tree
tree <- RTree(st_coordinates(all_points))

# Get taxon_info from disk. These need to be pre-fetched first
taxon_info <- load_taxon_info_from_file()
