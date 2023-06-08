#' Lists the FacileDataSet objects in a parent directory for a user to choose
#' 
#' Enumerates the available dataset objects in `datadir` once upon invocation.
#' A `meta.yaml` file in `datadir` will tell it how to present (name) and group
#' the datasets from a dropdown select.
#' 
#' @export
#' @param id the ID of the module
#' @param datadir the directory that holds the FacileDataSet directories
#' @return A list with reactive components:
#' 1. `$path()`: The path to the FacileDataSet
#' 2. `$gdb()`: A GeneSetDb object to match the organism of the FDS at `$path`.
facileDataSetSelectServer <- function(id, datadir, metafn = "meta.yaml", ...) {

  shiny::moduleServer(id, function(input, output, session) {
    state <- shiny::reactiveValues(
      organism = "__initializing__")

    # Initialize the selectUI on startup
    dinfo <- shiny::reactive({
      parse_dataset_directory(datadir(), metafn)
    })
    
    observeEvent(dinfo(), {
      dinfo. <- req(dinfo())
      choices <- setNames(dinfo.$name, dinfo.$label)
      selected <- choices[dinfo.$default]
      updateSelectInput(session, "dataselect", choices = choices,
                        selected = selected)
    })
    
    fds.path <- shiny::reactive({
      dinfo. <- shiny::req(dinfo())
      chosen <- shiny::req(input$dataselect)
      selected <- dplyr::filter(dinfo., .data$name == .env$chosen)
      
      path <- selected$path
      if (state$organism != selected$organism) {
        state$organism <- selected$organism
      }
      
      path
    })
    
    gdb <- reactive({
      org <- state$organism
      req(org != "__initializing__")
      gspath <- file.path(datadir(), "_metadata", org, "genesets.qs")
      if (file.exists(gspath)) {
        qs::qread(gspath)
      } else {
        NULL
      }
    })
    
    list(
      path = fds.path,
      gdb = gdb) 
  })
}

#' @noRd
#' @export
facileDataSetSelectInput <- function(id, label = "Dataset",
                                     choices = NULL, selected = NULL,
                                     multiple = FALSE,
                                     selectize = TRUE, width = NULL,
                                     size = NULL, ...) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(
      ns("dataselect"), label = label, choices = choices,
      selected = selected, multiple = multiple, selectize = selectize,
      width = width, size = size))
}

#' @noRd
parse_dataset_directory <- function(datadir, metafn = "meta.yaml", ...) {
  if (FALSE) {
    datadir <- "~/workspace/facilebio/data"
  }
  assert_directory_exists(datadir, "r")
  paths <- dir(datadir, "^[a-zA-Z0-9]", full.names = TRUE)
  paths <- paths[file.info(paths)$isdir]
  if (length(paths) == 0) {
    stop("No directories found in datadir: ", datadir)
  }
  assert_directory_exists(paths, "r")
  
  ds.meta <- sapply(basename(paths), function(fname) {
    yaml::read_yaml(file.path(datadir, fname, "meta.yaml"))
  }, simplify = FALSE)
  
  info <- tibble::tibble(
    name = basename(paths),
    label = sapply(ds.meta, "[[", "name"),
    path = paths,
    organism = sapply(ds.meta, "[[", "organism"),
    meta = ds.meta)
  
  meta.fn <- file.path(datadir, metafn)
  if (file.exists(meta.fn)) {
    # This will may add a `group` column, and rearrange the order of
    # the datasets
  } else {
    info$group <- "all"
    info$default <- info$name == names(ds.meta)[1L]
  }
  
  if (nrow(info) == 0) {
    info <- NULL
  }
  
  info
}

parse_dataset_directory_meta <- function(metafn) {
  if (!file.exists(metafn)) {
    warning("No metadata exists for data directory")
    return(NULL)
  }
}