#' Launches the FacileDashboard app
#' 
#' @param datadir A directory on the local file system that holds the top-level
#'   directories of the FacileDataSet objects to be used. This directory will
#'   also have a `meta.yaml` file that has configuration info for the app.
#' @param config The path to the configuration file for the app. Deafults to
#'   `file.path(datadir, "meta.yaml")`. Will be consumed by
#'   [FacileShine::facileDataSetSelectServer()], among other things.
#' @param gsdir The directory that holds the `<organism>/geneset.qs` GeneSetDb
#'   objects to use with the FacileDataSets in `datadir`
#' @param user the username of the user using the app, you can hotwire this
#'   with a USER env variable from SHINYPROXY
#' @export
run <- function(datadir = "~/workspace/facilebio/data",
                config = file.path(datadir, "meta.yaml"),
                gsdir = file.path(datadir, "_metadata"),
                user = Sys.getenv("USER"),
                app_title = "OmicsDashboard",
                debug = FALSE,
                ...) {
  checkmate::assert_directory_exists(datadir, "r")
  checkmate::assert_file_exists(config, "r", extension = "yaml")
  
  if (checkmate::test_directory(gsdir, "r")) {
    has_genesets <- length(dir(gsdir, "genesets.qs", recursive = TRUE)) > 0L
  } else {
    has_genesets <- FALSE
  }
  
  ui <- shinydashboard::dashboardPage(
    header = fd_header(title = app_title, debug = debug, ...),
    sidebar = fd_sidebar(has_genesets = has_genesets, debug = debug, ...),
    body = fd_body(has_genesets = has_genesets, debug = debug))
  
  server <- fd_server(datadir, config, user = user, has_genesets = has_genesets,
                      debug = debug, ...)
  
  shiny::shinyApp(ui, server)
}

fd_header <- function(title = "FacileDashboard", ..., debug = FALSE) {
  shinydashboard::dashboardHeader(title = title)
}

#' @noRd
#' @importFrom shinydashboard dashboardSidebar menuItem menuSubItem sidebarMenu
fd_sidebar <- function(..., has_genesets = FALSE, debug = FALSE) {
  dashboardSidebar(
    sidebarMenu(
     id = "tabs",
     menuItem(
       "Data Selection",
       tabName = "dataselectdrop",
       menuSubItem(
         "Dataset Selection",
         # icon = shiny::icon("database"),
         tabName = "dataselect"),
       if (has_genesets) {
         menuSubItem(
           "Gene Set Selection",
           tabName = "genesets")
       } else {
         NULL
       }),
     menuItem("PCA", tabName = "pca"),
     menuItem("Differential Abundance", tabName = "daa"),
     menuItem("Scatter Plot", tabName = "scatterplot"),
     menuItem("Box Plot", tabName = "boxplot")
    ))
}

#' @noRd
#' @importFrom shinydashboard tabItems tabItem
#' @importFrom shiny tags
fd_body <- function(..., has_genesets = FALSE, debug = FALSE) {
  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    # Change background color to white, we will want to do this with a css file
    # but brute forcing this for now.
    tags$head(tags$style(htmltools::HTML('
      .content-wrapper {
        background-color: #fff;
      }
    '))),
    
    tabItems(
      tabItem(
        tabName = "dataselect",
        tags$h2("Data Set Selection"),
        FacileShine::facileDataSetSelectInput("fdslist"),
        FacileShine::filteredReactiveFacileDataStoreUI("rfds")),
      
      tabItem(
        tabName = "pca",
        tags$h2("Principal Components Analysis"),
        FacileAnalysisShine::fpcaAnalysisUI("fpca")),
      
      tabItem(
        tabName = "daa",
        tags$h2("Differential Abundance Analysis"),
        if (has_genesets) {
          FacileAnalysisShine::fDgeSeaAnalysisUI("fdgeseas", debug = debug)
        } else {
          FacileAnalysisShine::fdgeAnalysisUI("fdgeseas", debug = debug)
        }),
    
      tabItem(
        tabName = "scatterplot",
        tags$h2("Scatter Plot"),
        FacileShine::facileScatterPlotUI("scatterplot")),
      
      tabItem(
        tabName = "boxplot",
        tags$h2("Box Plot"),
        FacileShine::facileBoxPlotUI("boxplot"))
    )
  )
}

fd_server <- function(datadir, config, ...,
                      user = Sys.getenv("USER"), has_genesets = FALSE,
                      debug = debug) {
  checkmate::assert_directory_exists(datadir)
  checkmate::assert_file_exists(config, "r", extension = "yaml")
  
  server <- function(input, output, session) {
    # Dataset Selection Logic --------------------------------------------------
    fdslist <- FacileShine::facileDataSetSelectServer(
      "fdslist", reactive(datadir), config)
    
    rfds <- shiny::callModule(
      FacileShine::filteredReactiveFacileDataStore,
      "rfds",
      path = fdslist$path,
      user = user)

    gdb <- reactive({
      req(FacileShine::initialized(rfds))
      fdslist$gdb()
    })
    
    # Analysis Modules ---------------------------------------------------------
    pca <- FacileAnalysisShine::fpcaAnalysisServer("fpca", rfds, ...)
    if (has_genesets) {
      daa <- FacileAnalysisShine::fDgeSeaAnalysisServer(
        "fdgeseas", rfds, gdb = gdb, debug = debug, ...)
    } else {
      daa <- FacileAnalysisShine::fdgeAnalysisServer(
        "fdgeseas", rfds, debug = debug, ...)
    }

    scatter <- shiny::callModule(
      FacileShine::facileScatterPlot,
      "scatterplot", rfds, gdb)

    boxplot <- shiny::callModule(
      FacileShine::facileBoxPlot,
      "boxplot", rfds, gdb)
  }
}

# Convenience functions for testing --------------------------------------------
simple_gdb <- function(id.type = c("entrez", "ensembl")) {
  id.type <- match.arg(id.type)
  gdb <- sparrow::getMSigGeneSetDb(
    collection = c("H", "C2"),
    species = "human",
    id.type = id.type,
    promote.subcategory.to.collection = TRUE,
    with.kegg = TRUE)
  gdb <- gdb[gdb@table$collection == "H" | gdb@table$collection == "C2_CP:KEGG"]
  gdb <- sparrow::renameCollections(gdb, c(H = "Hallmark", "C2_CP:KEGG" = "KEGG"))
  gdb
}

if (FALSE) {
  devtools::load_all("."); run()
}

if (FALSE) {
  # devtools::load_all("."); if (!exists("gdb")) gdb <- FacileDashboard:::simple_gdb("ensembl"); run(gdb = gdb)
}
