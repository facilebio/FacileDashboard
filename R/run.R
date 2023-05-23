#' Launches the FacileDashboard app
run <- function(datadir = "~/workspace/facilebio/data",
                config = NULL,
                user = Sys.getenv("USER"),
                app_title = "FacileDashboard", 
                gdb = NULL, gdb_idtype = "entrez", ...) {
  checkmate::assert_directory_exists(datadir, "r")
    
  if (is.null(gdb)) {
    gdb <- simple_gdb(id.type = gdb_idtype)
  }
  
  ui <- shinydashboard::dashboardPage(
    header = fd_header(title = app_title, ...),
    sidebar = fd_sidebar(...),
    body = fd_body(...))
  
  server <- fd_server(datadir, config, user, gdb = gdb, ...)
  
  shiny::shinyApp(ui, server)
}

fd_header <- function(title = "FacileDashboard", ...) {
  shinydashboard::dashboardHeader(title = title)
}

#' @noRd
#' @importFrom shinydashboard dashboardSidebar menuItem menuSubItem sidebarMenu
fd_sidebar <- function(...) {
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
       menuSubItem(
         "Gene Set Selection",
         tabName = "genesets")),
     menuItem("PCA", tabName = "pca"),
     menuItem("Differential Abundance", tabName = "daa"),
     menuItem("Scatter Plot", tabName = "scatterplot"),
     menuItem("Box Plot", tabName = "boxplot")
    ))
}

#' @noRd
#' @importFrom shinydashboard tabItems tabItem
#' @importFrom shiny tags
fd_body <- function(...) {
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
        facileDataSetSelectInput("fdslist"),
        FacileShine::filteredReactiveFacileDataStoreUI("rfds")),
      
      tabItem(
        tabName = "pca",
        tags$h2("Principal Components Analysis"),
        FacileAnalysis::fpcaAnalysisUI("fpca")),
      
      tabItem(
        tabName = "daa",
        tags$h2("Differential Abundance Analysis"),
        FacileAnalysis::fDgeSeaAnalysisUI("fdgeseas")),
      
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

fd_server <- function(datadir = "~/workspace/facilebio/data", config = NULL,
                      user = Sys.getenv("USER"), gdb = NULL, ...) {
  
  server <- function(input, output, session) {
    fdslist <- facileDataSetSelectServer("fdslist", reactive(datadir))

    rfds <- shiny::callModule(
      FacileShine::filteredReactiveFacileDataStore,
      "rfds",
      path = fdslist$path,
      user = user)
    
    # TODO: Create a PCA Analysis module with a GSEA component to it
    pca <- shiny::callModule(
      FacileAnalysis::fpcaAnalysis, "fpca", rfds, ...)

    daa <- shiny::callModule(
      FacileAnalysis::fDgeSeaAnalysis, "fdgeseas", rfds, gdb = gdb, ...)

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
  devtools::load_all("."); if (!exists("gdb")) gdb <- simple_gdb("ensembl"); run(gdb = gdb)
}
