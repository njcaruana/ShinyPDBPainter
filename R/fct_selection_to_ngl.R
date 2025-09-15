#' Selection to NGL query
#'
#' @description
#' Transform sequence input to NGL query
#'
#' @param sequence Protein sequence in string format
#' @param input NGL query
#'
#' @examples
#' selection_to_ngl("ALAAGSDFG", "1-5 OR <DFG>")
#' @import stringr
#' @importFrom dplyr case_when
#' @export
selection_to_ngl <- function(sequence, input, gene_map){

  if (is.null(input) || input == "" || input == ":") {
    return("none")
  }

  # --- Gene name mapping ---
  if (!is.null(gene_map) && input %in% gene_map$genename) {
    chains <- gene_map$mappings.chain_id[gene_map$genename == input]
    chains <- chains[!is.na(chains)]
    if (length(chains) == 0) {
      return("none")
    }
    return(chains)
  }

  #Get matches
  strings <- stringr::str_extract_all(input, "(?<=\\<)(.*?)(?=\\>)", simplify = TRUE)[1,]
  #Return input if no match
  if(length(strings) == 0 || nchar(strings) == 0){
    return(input)
  } else {

    #Find positions
    matchPositions <- stringr::str_locate_all(sequence, strings)
    matchPositions <- do.call(rbind, matchPositions)
    if(nrow(matchPositions) == 0){
      return(NULL)
    }
    matchPositions <- data.frame(matchPositions)
    matchPositions <- matchPositions %>%
      mutate_at(vars(.data$start, .data$end), as.character) %>%
      mutate(
        sequence = dplyr::case_when(
          start != end ~ paste(start,"-",end, sep = ""),
          TRUE ~start
        )
      )
    matchPositions <- paste(matchPositions$sequence, collapse=" OR ")

    input <- stringr::str_replace(input, "(?<=\\<)(.*?)(?=\\>)", matchPositions)
    input <- stringr::str_replace_all(input, "\\<|\\>", "")

    #Replace matches with positions in input
    return(input)
  }
}
