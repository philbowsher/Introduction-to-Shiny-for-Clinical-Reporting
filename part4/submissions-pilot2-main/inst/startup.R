# Project Level Setup
R_version <- "4.2.0"																                  # set up project R version
snapshot  <- "2022-09-01" 									                          # set up snapshot date
teal_snapshot <- "2022-06-09"
repos <- c(
  paste0("https://packagemanager.rstudio.com/cran/", snapshot), 
  paste0("https://insightsengineering.github.io/depository/", teal_snapshot)  # set up repository based on snapshot
)

home      <- normalizePath(".") # set up home directory
while(! "DESCRIPTION" %in% list.files(home)){
        home <- dirname(home)
}

# A&R folder path (Do not edit information below)
path <- list(home = "")

path <- lapply(path, function(x) file.path(home, x))

# Define repo URL for project specific package installation
options(repos = repos)

# Check R Version
if(paste(R.version$major, R.version$minor, sep = ".") != R_version & interactive()){
 stop("The current R version is not the same with the current project in ", R_version)
}

# Repository
message("Current project R package repository:")
message(paste0("    ", getOption("repos")))
message(" ")

# Display R Session Status
#message("R packages were installed from repo: ", options('repo'), "\n")
message("Below R package path are searching in order to find installed R pacakges in this R session:", "\n",
        paste(paste0("    ", .libPaths()), collapse = "\n"))
message("\n")

message("The project home directory is ", home)
message("\n")

rm(home, R_version)
