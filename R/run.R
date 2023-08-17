#' Launch the analysis dashboard.
#' 
#' This presents a dashboard of analysis modules that can be run over a variety
#' of FacileDataSets.
#' 
#' @section App Setup and Configuration:
#' The app needs a directory on the local filesystem that stores an arbitrary
#' number of FacileDataSet objects (directories), along with a `meta.yaml` file
#' and `_metadata/<species>/genesets.qs` files if you want to enable geneset
#' based exploration. Take a loot at the 
#' [FacileShine::facileDataSetSelectServer()]) and 
#' `FacileShine/inst/testdata/fds-directory[/meta.yaml]` structure [and file]
#' for more information
#' 
#' @export
#' @param datadir A parent directory that 
#' @param config the meta.yaml file that holds configuration options for this
#'   app and its modules. Refer to the "App Setup and Configuration" section
#'   for more detail
#' @param  gsdir the local file path to the directory that holds
#'   species-specific genesets
#' @param user the userid of the person logging in
#' @param app_title the title of the dashboard
#' @param debug Default `FALSE`, set to `TRUE` to add debug widgets in the UI.
#' @param default_filter_covariate the name of the sample-level covariate that
#'   is selected first in the set of filters used to slice into the datasets
#'   of the [FacileShine::facileDataSetSelectServer()] module
#' @param default_covariate you can pick a default covariate name (`"group"`
#'   is often a good idea) that other widgets will present first for selection.
#' @param default_collections the name of the collections in the GeneSetDb
#'   objects that should be activated by default.
run <- function(datadir = "~/workspace/facilebio/data",
                config = file.path(datadir, "meta.yaml"),
                gsdir = file.path(datadir, "_metadata"),
                user = Sys.getenv("USER"),
                app_title = "FacileDashboard",
                debug = FALSE,
                default_filter_covariate = "experiment_id",
                default_covariate = "group",
                default_collections = c("Hallmark", "KEGG"),
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
  
  server <- fd_server(
    datadir, config, user = user, has_genesets = has_genesets,
    default_filter_covariate = default_filter_covariate,
    default_covariate = default_covariate,
    default_collections = default_collections,
    debug = debug, ...)
  
  shiny::shinyApp(ui, server)
}


fd_header <- function(title = "OmicsDashboard", ..., debug = FALSE) {
  shinydashboard::dashboardHeader(title = title)
}

#' @noRd
#' @importFrom shinydashboard dashboardSidebar menuItem menuSubItem sidebarMenu
fd_sidebar <- function(..., has_genesets = FALSE, debug = FALSE) {
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dataset Selection", tabName = "dataselect"),
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
                      default_filter_covariate = "experiment_id",
                      default_covariate = "group",
                      default_collections = c("Hallmark", "KEGG"),
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
      default_covariate = default_filter_covariate,
      path = fdslist$path,
      user = user)
    
    gdb <- fdslist$gdb
    
    # Analysis Modules ---------------------------------------------------------
    pca <- FacileAnalysisShine::fpcaAnalysisServer("fpca", rfds, ...)
    
    if (has_genesets) {
      daaServer <- FacileAnalysisShine::fDgeSeaAnalysisServer
    } else {
      default_collections <- NULL
      daaServer <- FacileAnalysisShine::fdgeAnalysisServer
    }
    daa <- daaServer("fdgeseas", rfds, gdb = gdb, debug = debug, 
                     default_covariate = default_covariate,
                     default_collections = default_collections, ...)
    
    scatter <- shiny::callModule(
      FacileShine::facileScatterPlot,
      "scatterplot", rfds, gdb, 
      default_collections = default_collections)
    
    boxplot <- shiny::callModule(
      FacileShine::facileBoxPlot,
      "boxplot", rfds, gdb, 
      default_collections = default_collections)
    
  }
}
