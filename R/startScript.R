#' Function to set working directory and load in packages at the start of a script
#'
#' @param wd A character string giving the desired working directory
#' @param packages A character vector of packages to be loaded/installed
#'
#' @return Nothing, just a
#' @export
#'
#' @examples
#' \dontrun{
#' startScript(wd = '~', packages = c('Bchron', 'simmr'))
#' }
startScript = function(wd = '~',
                       packages = c('tidyverse',
                                    'devtools',
                                    'viridis')) {

# Set the working directory
setwd(wd)

# Load in packages
packages(packages)

}
