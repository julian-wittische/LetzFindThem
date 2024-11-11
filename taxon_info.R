library("WikidataQueryServiceR")
library("purrr")

query_template <- '
SELECT 
  (SAMPLE(?species) AS ?speciesUrl)
  ?speciesLabel 
  (SAMPLE(?image) AS ?imageUrl) 
  (SAMPLE(?commonNameEn) AS ?commonNameEn) 
  (SAMPLE(?commonNameDe) AS ?commonNameDe) 
  (SAMPLE(?commonNameFr) AS ?commonNameFr) 
  (SAMPLE(?commonNameLb) AS ?commonNameLb) 
  (SAMPLE(?germanLabel) AS ?labelDe) 
  (SAMPLE(?frenchLabel) AS ?labelFr) 
  (SAMPLE(?luxembourgishLabel) AS ?labelLb) 
  (SAMPLE(?wikipediaLink) AS ?wikipediaUrl)
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

  # Retrieve the English Wikipedia link if it exists
  OPTIONAL {
    ?wikipediaLink schema:about ?species;
                   schema:isPartOf <https://en.wikipedia.org/>.
  }

  # Service to retrieve English labels
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
}
GROUP BY ?species ?speciesLabel'

download_taxon_info <- function(taxon_names, block_size) {
  if (missing(block_size)) {
    block_size <- 100
  }

  names_count <- length(taxon_names)
  taxon_info = data.frame()

  i <- 1
  while (i <= names_count) {
    if (i+block_size-1 < names_count) {
      taxon_names_block <- taxon_names[i:(i+block_size-1)]
    } else {
      taxon_names_block <- taxon_names[i:names_count]
    }
  
    taxon_names_block_str <- paste(purrr::map(taxon_names_block,
                                              function(tn) sprintf('"%s"', tn)),
                                   collapse=" ")
    query <- sprintf(query_template, taxon_names_block_str)
    result <- query_wikidata(query)
  
    taxon_info <- rbind(taxon_info, result)
    
    i <- i+block_size
  }
  taxon_info
}

download_taxon_info_from_observations <- function(observations, block_size, save) {
  if (missing(save)) {
    save <- false
  }
    
  taxa <- sort(unique(observations[,"preferred"]))
  taxon_info <- download_taxon_info(taxa, block_size)

  if (save) {
    save(taxon_info, "data/taxon_info.RData")
  }
  taxon_info
}

load_taxon_info_from_file <- function(path) {
  load("data/taxon_info.RData")
}

find_taxon_info <- function(taxon_info, taxon_name) {
  taxon_info[grep(taxon_name, taxon_info$speciesLabel),]
}
