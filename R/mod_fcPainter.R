#' PDBPainter UI Module
#'
#' @description A UI module for the PDBPainter feature, including file upload
#'   and color range selection.
#'
#' @param input,output,session Internal parameters for {shiny}. DO NOT REMOVE.#'
#' @export
mod_fcPainter_ui <- function(id) {
  ns <- NS(id)
  tagList(
    menuItem("painter",
             icon = icon("paintbrush"),
             fileInput(ns("filefc"), "Upload logFC data (CSV)", accept = c(".csv")),
             sliderInput(ns("range"), "Select logFC Range", min = -5, max = 5, value = c(-2, 2)),
             actionButton(ns("runpdbpainter"), "Paint"),
             actionButton(ns("clearpaint"), "Clear Paint")
    )
  )
}


#' PDBPainter Server Module
#'
#' @description A server module for handling custom coloring of PDB structures
#'   based on user-uploaded logFC data.
#'
#' @param input,output,session Internal parameters for {shiny}. DO NOT REMOVE.
#' @param r A reactive values object from the main app server.
#'
#' @noRd
#'
#'
mod_fcPainter_server <- function(input, output, session, globalSession, r) {
  ns <- session$ns
  Viewer_proxy <- NGLVieweR_proxy("NGLVieweROutput_ui_1-structure", session = globalSession)

  # Reactive expression to get the uploaded data

  observeEvent(input$runpdbpainter, {
    r$painting <- TRUE
  })

  paintingdata <- eventReactive(input$runpdbpainter, {
    #make sure theres a PDB code loaded and theres been an upload of a CSV
    req(input$filefc)
    #req(input$inputPDB$datapath)

    #load the data
    df <- fct_read_logfc_file(input$filefc$datapath)
    #create the hex colors
    range_vals <- input$range
    hexCols <- fct_generate_hex_colors(df, range_vals)
    #get the protein chain data
    chainDf <- fct_get_chain_df(r$fileInput$name)

    # Join logFC + chains via UniProt
    joinedDf <- fct_join_logfc_chains(hexCols, chainDf)

    return(joinedDf)
  })

  observeEvent(paintingdata(), {
    r$painting <- FALSE
  })

  # This is the  reactive expression that prepares the final coloring data
  hex_colorset <- reactive({
    req(paintingdata())
    df <- paintingdata()
    df$mappings.chain_id <- paste0(":", trimws(df$mappings.chain_id))
    df %>%
      dplyr::select(sele = mappings.chain_id, colorValue = hex_from_scales)
  })

  hex_colorset_values <- reactive({
    req(hex_colorset())
    hex_colorset_values <- apply(hex_colorset(),1,as.list)
    hex_colorset_values <- unname(hex_colorset_values)
    return(hex_colorset_values)
  })

  # Compute gene_map once, when paintingdata() finishes
  observeEvent(paintingdata(), {
    req(paintingdata())
    full_map <- paintingdata()
    r$gene_map <- full_map[, c("genename", "uniprot", "mappings.chain_id")]
    r$gene_map$mappings.chain_id <- paste0(":", trimws(r$gene_map$mappings.chain_id))
    r$gene_map <- unique(r$gene_map)
    print(r$gene_map, n = 100)  # just to check
  })


  # Only do the coloring when the button is pressed
  observe({
    req(hex_colorset_values())

    for (i in seq_along(hex_colorset_values())) {
      Viewer_proxy %>% addSelection(
        type = "surface",
        param = list(
          name = "PDBPainter",
          sele = hex_colorset_values()[[i]][["sele"]],
          color = hex_colorset_values()[[i]][["colorValue"]]
        )
      )
    }
  })

  observeEvent(input$clearpaint, {
      Viewer_proxy %>%
        removeSelection(
          name = c("PDBPainter")
      )
  })



}
