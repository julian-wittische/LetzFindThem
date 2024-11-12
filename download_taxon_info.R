source("mdata.R")
source("taxon_info.R")

observations <- load_data('data/observations-short.csv')
download_taxon_info_from_observations(observations, 110, TRUE)
