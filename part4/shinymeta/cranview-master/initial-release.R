initial_release <- function(packages) {
  min_date <- Sys.Date() - 1
  
  for (pkg in packages) {
    # api data for package. we want the initial release - the first element of the "timeline"
    pkg_data <- httr::GET(paste0("http://crandb.r-pkg.org/", pkg, "/all"))
    pkg_data <- httr::content(pkg_data)
    
    initial_release <- pkg_data$timeline[[1]]
    min_date <- min(min_date, as.Date(initial_release))    
  }
  
  min_date
}