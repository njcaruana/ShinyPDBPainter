#' Get a data frame of PDB chains from an API or local file.
#'
#' @description
#' This function takes a PDB code, checks for a local JSON file,
#' and if not found, makes an API call to retrieve chain information.
#'
#' @param pdb_code A string representing the PDB code (e.g., "7cid").
#' @param local_path The path to the directory where local PDB files are stored.
#'
#' @return A grouped `data.frame` with PDB chain information.
#' @importFrom httr GET
#' @importFrom dplyr bind_rows mutate group_by
#' @importFrom RcppSimdJson fload fparse
#' @export

# fct_get_chain_df helper function
# It requires the 'httr' and 'jsonlite' packages.

fct_get_chain_df <- function(pdb_code) {
  print(pdb_code)
  # Make the API call to get Uniprot mappings
  r <- httr::GET(paste0("https://www.ebi.ac.uk/pdbe/api/mappings/uniprot/", pdb_code))

  # Check for successful API response
  if (r$status_code != 200) {
    stop("Failed to fetch data from the API. Check your input code or internet connection.")
  }

  # Convert response content to text and parse as JSON
  r_content <- RcppSimdJson::fparse(r$content)

  # Process the data to create a data frame with chain information
  chains <- do.call(dplyr::bind_rows,
                    lapply(names(r_content[[1]][[1]]), function(uniprot) {
                      # The structure of the JSON is such that the first element of r_content
                      # is a list where each element is named by the uniprot ID.
                      # This handles cases where there are multiple uniprot IDs per PDB.
                      r_content[[1]][[1]][[uniprot]] %>%
                        data.frame() %>%
                        dplyr::mutate(uniprot = uniprot)
                    })
  ) %>%
    # Group the data by identifier, entity, and uniprot ID
    dplyr::group_by(identifier, mappings.entity_id, uniprot) %>%
    dplyr::select(uniprot, mappings.chain_id,identifier, mappings.entity_id)

  return(chains) # Return the processed data
}


#' Read a logFC CSV file uploaded by the user.
#'
#' @description
#' Reads a CSV file from a given path and returns it as a data frame.
#'
#' @param file_path The temporary file path of the uploaded CSV.
#'
#' @return A `data.frame` of the uploaded data.
#' @export
fct_read_logfc_file <- function(file_path) {
  df <- read.csv(file_path)
  return(df)
}




#' Generate hex color codes for logFC values based on a given range.
#'
#' @description
#' This function takes a data frame with a 'logfc' column and
#' generates a new column with hex color codes based on a specified range.
#'
#' @param df The data frame containing the 'logfc' values.
#' @param range A numeric vector of length 2 defining the color gradient scale.
#' @return The input data frame with a new column, 'hex_from_scales'.
#' @importFrom scales col_numeric
#' @export
fct_generate_hex_colors <- function(df, range) {
  # Clamp values to the user-selected range
  clamped_values <- pmin(pmax(df$logfc, range[1]), range[2])
  # Define the color palette
  palette_func <- scales::col_numeric(c("blue","white","red"), domain = c(range[1], range[2]))
  # Map clamped values to colors
  df$hex_from_scales <- palette_func(clamped_values)
  return(df)
}


#' Normalise logFC values for B-factor coloring
#'
#' @description
#' This function clamps logFC values into a user-selected range
#' and rescales them (if desired) to keep values consistent across
#' structures. These values can then be written into the B-factor column
#' of a .cif or .pdb file for use with NGL's `colorScheme = "bfactor"`.
#'
#' @param df A data frame containing a `logfc` column.
#' @param range A numeric vector of length 2 giving the min/max limits.
#' @param rescale Logical, whether to rescale values into [0,1]. Default = FALSE.
#'
#' @return The input data frame with an added column `bfactor_value`.
#' @export
fct_range_logfc <- function(df, range = c(-2, 2), rescale = FALSE) {
  stopifnot("logfc" %in% names(df))

  # clamp values to the selected range
  clamped <- pmin(pmax(df$logfc, range[1]), range[2])

  if (rescale) {
    # rescale into [0,1] — useful if you want consistent normalization
    normalised <- (clamped - range[1]) / (range[2] - range[1])
  } else {
    # leave them in logFC units
    normalised <- clamped
  }

  df$bfactor_value <- normalised
  return(df)
}


#' Map gene symbols to UniProt IDs using AnnotationDbi.
#'
#' @description
#' This function takes a vector of gene names and returns a
#' data frame mapping them to UniProt IDs through HGNC IDs.
#'
#' @param geneSymbol A character vector of gene names (e.g., 'SYMBOL').
#'
#' @return A `data.frame` with gene symbols and corresponding UniProt IDs.
#' @importFrom httr GET
#' @importFrom purrr map_chr possibly
#' @importFrom RcppSimdJson fload
#' @export
#'

# Download JSON version once
#url <- "https://storage.googleapis.com/public-download-files/hgnc/json/json/hgnc_complete_set.json"
#hg_data <- jsonlite::fromJSON(url, flatten = TRUE)

# fct_get_alias <- function(geneSymbol) {
#   search <- httr::GET(paste0("https://rest.genenames.org/search/symbol:",geneSymbol, "+OR+alias_symbol:", geneSymbol, "+OR+prev_symbol:", geneSymbol), httr::accept_json())
#   search_content <- jsonlite::fromJSON(rawToChar(search$content), flatten = TRUE)
#   hgncID <- search_content[[2]][[5]][["hgnc_id"]]
#   fetch <- httr::GET(paste0("https://rest.genenames.org/fetch/hgnc_id/",hgncID), httr::accept_json())
#   fetch_content <- jsonlite::fromJSON(rawToChar(fetch$content), flatten = TRUE)
#   uniprotID <- fetch_content[[2]][[4]][["uniprot_ids"]][[1]]
#   return(uniprotID)
# }

# fct_get_alias <- function(geneSymbol) {
#   search <- httr::GET(paste0("https://rest.genenames.org/search/symbol:",geneSymbol, "+OR+alias_symbol:", geneSymbol, "+OR+prev_symbol:", geneSymbol), httr::accept_json())
#   search_content <- RcppSimdJson::fparse(search$content)
#   hgncID <- search_content[[2]][[5]][["hgnc_id"]]
#   fetch <- httr::GET(paste0("https://rest.genenames.org/fetch/hgnc_id/",hgncID), httr::accept_json())
#   fetch_content <- RcppSimdJson::fparse(fetch$content)
#   uniprotID <- fetch_content[[2]][[4]][["uniprot_ids"]][[1]]
#   return(uniprotID)
# }

hg_data <- RcppSimdJson::fload("inst/app/www/hgnc_complete_set.json")

fct_get_alias <- function(geneSymbol) {
  rows <- hg_data$response$docs[hg_data$response$docs$symbol == geneSymbol |
                                  geneSymbol %in% hg_data$response$docs$alias_symbol |
                                  geneSymbol %in% hg_data$response$docs$prev_symbol, ]
  if (nrow(rows) == 0) return(NA_character_)
  uniprot_list <- rows$uniprot_ids[[1]]
  return(ifelse(length(uniprot_list) > 0, uniprot_list[1], NA_character_))
}

fct_join_logfc_chains <- function(logfc, chains) {
  safe_fct_alias <- purrr::possibly(~ fct_get_alias(.x), otherwise = NA_character_)

  merged_df <- chains %>%
    left_join(
      logfc %>%
        rowwise() %>%
        mutate(uniprotID = safe_fct_alias(genename)) %>%
        ungroup(),
      by = c("uniprot" = "uniprotID")
    )

  return(merged_df)
}
#' Combine and clean data to create a final color set for the viewer.
#'
#' @description
#' This function merges the logFC data with the chain and dictionary information,
#' and formats it for use with NGLVieweR.
#'
#' @param joined_df A dataframe from fct_join_logfc_chains (contains logfc, hex colors, chains).

#' @return A `data.frame` with 'sele' and 'colorValue' columns for coloring the viewer.
#' @importFrom dplyr mutate na_if vars
#' @export

fct_prepare_chainset <- function(joined_df) {

  # Replace missing data hex with a grey code
  namedpdblog2 <- joined_df %>%
    dplyr::mutate(hex_from_scales = dplyr::na_if(hex_from_scales, "")) %>%
    dplyr::mutate_at(dplyr::vars(hex_from_scales), ~replace_na(., '#D3D3D3'))

  # Add ':' for calling chain
  namedpdblog2$mappings.chain_id <- paste0(":", trimws(namedpdblog2$mappings.chain_id))

  # Select and rename columns for the viewer
  chainset <- namedpdblog2[c("mappings.chain_id", "hex_from_scales")]
  names(chainset) <- c("sele", "colorValue")

  return(chainset)
}


#' Prepare data for the legend plot.
#'
#' @description
#' This function takes the uploaded logFC data and the color range,
#' and returns a data frame suitable for plotting a legend.
#'
#' @param df The data frame containing the 'logfc' values.
#' @param range A numeric vector of length 2 defining the color gradient scale.
#'
#' @return A data frame with sorted logFC values and hex colors.
#' @importFrom scales col_numeric
#' @export
fct_prepare_legend_data <- function(df, range) {
  color_values <- pmin(pmax(df$logfc, range[1]), range[2])
  color_func <- scales::col_numeric(c("blue","white","red"), domain = c(range[1], range[2]))
  df$hex_from_scales <- color_func(color_values)
  df <- df[order(df$logfc, decreasing = FALSE),]
  return(df)
}
