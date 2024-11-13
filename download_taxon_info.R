source("config.R")
source("mdata.R")
source("taxon_info.R")

observations <- load_data(DATA_PATH)
download_taxon_info_from_observations(observations, 90, TRUE)
