#' Lists the FacileDataSet objects in a parent directory for a user to choose
#' 
#' This currently just short circuits to the first dataset
facileDataSetSelectServer <- function(id, datadir, ...) {
  shiny::moduleServer(
    id, 
    function(input, output, session) {
      assert_directory_exists(datadir, "r")
      
      fds.path <- shiny::reactive({
        paths <- dir(datadir, full.names = TRUE)
        paths <- paths[file.info(paths)$isdir]
        if (length(paths) == 0) {
          stop("No directories found in datadir: ", datadir)
        }
        assert_directory_exists(paths, "r")
        
        chosen <- paths[1L]
        # This will ensure that the directory is one that holds the facildataset
        out <- FacileData::FacileDataSet(chosen)
        chosen
      })
      
      list(path = fds.path) 
    })
}

facileDataSetSelectInput <- function(id, ...) {
  
}