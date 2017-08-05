#' Function to load in packages and install those which are missing
#'
#' @param ... A character vector of packages to be loaded/installed
#'
#' @return A list of the currently installed packages
#' @export
#'
#' @importFrom utils "install.packages" "installed.packages"
#'
#' @examples
#' \dontrun{
#' packages(c('Bchron', 'simmr'))
#' }
packages = function(...) {

  # Get the packages
  pkgs = as.vector(unlist(...))

  # Error checking
  stopifnot(all(is.character(pkgs)))

  # Try loading all packages
  inst = try(lapply(pkgs, library, character.only = TRUE),
             silent = TRUE)

  # If inst didn't work find which packages are installed
  badPkgs = which(is.na(match(pkgs,installed.packages()[,'Package'])))

  if(length(badPkgs)>0) {
    cat('Some packages missing. Installing...\n')
    cat(pkgs[badPkgs], '\n')
    lapply(pkgs[badPkgs], install.packages)
  }

  # Now properly load in the packages
  lapply(pkgs, library, character.only = TRUE)

}
