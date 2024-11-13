library("WikidataQueryServiceR")
library("purrr")
library("data.table")

query_template <- '
SELECT
  ?scientificName
  ?speciesLabel 
  (SAMPLE(?species) AS ?speciesUrl)
  (SAMPLE(?image) AS ?imageUrl) 
  (SAMPLE(?commonNameEn) AS ?commonNameEn) 
  (SAMPLE(?commonNameDe) AS ?commonNameDe) 
  (SAMPLE(?commonNameFr) AS ?commonNameFr) 
  (SAMPLE(?commonNameLb) AS ?commonNameLb)
  (SAMPLE(?englishLabel) as ?labelEn)
  (SAMPLE(?germanLabel) AS ?labelDe) 
  (SAMPLE(?frenchLabel) AS ?labelFr) 
  (SAMPLE(?luxembourgishLabel) AS ?labelLb)
  (SAMPLE(?wikipediaLinkEn) AS ?wikipediaLinkEn)
  (SAMPLE(?wikipediaLinkFr) AS ?wikipediaLinkFr)
  (SAMPLE(?wikipediaLinkDe) AS ?wikipediaLinkDe)
  (SAMPLE(?wikipediaLinkLb) AS ?wikipediaLinkLb)
WHERE {
  VALUES ?scientificName {%s}  # Scientific names

  # Match entities based on their taxon name (scientific name)
  ?species wdt:P225 ?scientificName.

  # Fetch the image (P18) if available
  OPTIONAL { ?species wdt:P18 ?image. }

  # Fetch the common name (P1843) in different languages if available
  OPTIONAL {
    ?species wdt:P1843 ?commonNameEn.
    FILTER(LANG(?commonNameEn) = "en")  # Restrict to English common names
  }

  OPTIONAL {
    ?species wdt:P1843 ?commonNameDe.
    FILTER(LANG(?commonNameDe) = "de")  # Restrict to German common names
  }

  OPTIONAL {
    ?species wdt:P1843 ?commonNameFr.
    FILTER(LANG(?commonNameFr) = "fr")  # Restrict to French common names
  }

  OPTIONAL {
    ?species wdt:P1843 ?commonNameLb.
    FILTER(LANG(?commonNameLb) = "lb")  # Restrict to Luxembourgish common names
  }

  # Fetch labels in different languages
  OPTIONAL {
    ?species rdfs:label ?germanLabel.
    FILTER(LANG(?germanLabel) = "de")  # Restrict to German labels
  }

  OPTIONAL {
    ?species rdfs:label ?frenchLabel.
    FILTER(LANG(?frenchLabel) = "fr")  # Restrict to French labels
  }

  OPTIONAL {
    ?species rdfs:label ?luxembourgishLabel.
    FILTER(LANG(?luxembourgishLabel) = "lb")  # Restrict to Luxembourgish labels
  }

  # Retrieve the Wikipedia links if they exists
  OPTIONAL {
    ?wikipediaLinkEn schema:about ?species;
                     schema:isPartOf <https://en.wikipedia.org/>.
  }
  OPTIONAL {
    ?wikipediaLinkFr schema:about ?species;
                     schema:isPartOf <https://fr.wikipedia.org/>.
  }
  OPTIONAL {
    ?wikipediaLinkDe schema:about ?species;
                     schema:isPartOf <https://de.wikipedia.org/>.
  }
  OPTIONAL {
    ?wikipediaLinkLb schema:about ?species;
                     schema:isPartOf <https://lb.wikipedia.org/>.
  }

  # Service to retrieve English labels
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
}
GROUP BY ?scientificName ?speciesLabel'

download_taxon_info <- function(taxon_names, block_size) {
  if (missing(block_size)) {
    block_size <- 100
  }

  names_count <- length(taxon_names)
  taxon_info <- data.frame()

  i <- 1
  while (i <= names_count) {
    if (i+block_size-1 < names_count) {
      # a complete block
      taxon_names_block <- taxon_names[i:(i+block_size-1)]
    } else {
      # at end of taxa, we only have an incomplete block left
      taxon_names_block <- taxon_names[i:names_count]
    }
  
    # format names in one string: "name1" "name2" ...
    taxon_names_block_str <- paste(purrr::map(taxon_names_block,
                                              function(tn) sprintf('"%s"', tn)),
                                   collapse=" ")
    # aaand interpolate into SPARQL query
    query <- sprintf(query_template, taxon_names_block_str)
    result <- query_wikidata(query)

    for (j in seq_len(nrow(result))) {
      # Use label as common name if first is missing
      if (is.na(result[j, "commonNameEn"]) && !is.na(result[j, "labelEn"])) {
        result[j, "commonNameEn"] <- result[j, "labelEn"]
      }
      if (is.na(result[j, "commonNameFr"]) && !is.na(result[j, "labelFr"])) {
        result[j, "commonNameFr"] <- result[j, "labelFr"]
      }
      if (is.na(result[j, "commonNameDe"]) && !is.na(result[j, "labelDe"])) {
        result[j, "commonNameDe"] <- result[j, "labelDe"]
      }
      if (is.na(result[j, "commonNameLb"]) && !is.na(result[j, "labelLb"])) {
        result[j, "commonNameLb"] <- result[j, "labelLb"]
      }
    }

    ## result$mdata_taxon_name <- taxon_names_block

    # append to taxon info
    taxon_info <- rbind(taxon_info, result)

    i <- i+block_size

    print(paste(i/names_count*100, "%"))
  }
  taxon_info
}

download_taxon_info_from_observations <- function(observations, block_size, save) {
  if (missing(save)) {
    save <- false
  }

  observations <- as.data.frame(observations)
  obs_table <- as.data.table(observations)

  taxa <- sort(unique(observations$species))
  taxon_info <- download_taxon_info(taxa, block_size)

  # Supplement with higher taxon info from observation table
  for (i in seq_len(nrow(taxon_info))) {
    print(paste(i/nrow(taxon_info)*100, "%"))
    sn <- taxon_info[i, "scientificName"][[1]]
    obs <- observations[grep(sn, obs_table$species),] # find corresponding observations

    taxon_info[i, "kingdom"] <- obs$Taxon_Kingdom[1]
    taxon_info[i, "phylum"] <- obs$Taxon_Phylum[1]
    taxon_info[i, "class"] <- obs$Taxon_Class[1]
    taxon_info[i, "order"] <- obs$Taxon_Order[1]
    taxon_info[i, "family"] <- obs$Taxon_Family[1]    
  }

  if (save) {
    save(taxon_info, file="data/taxon_info.RData")
  }
  taxon_info
}

load_taxon_info_from_file <- function(path) {
  if (missing(path)) {
    path <- "data/taxon_info.RData"
  }
    
  load(path)
  taxon_info
}

find_taxon_info <- function(taxon_info, taxon_name) {
  taxon_info[grep(taxon_name, taxon_info$scientificName),]
}
