#' Get Latest Version of PowerPoint Package Instructions
#'
#' @param directory String. Directory in which to copy PowerPoint file. Defaults to \code{"instructions"}.
#'
#' @returns Nothing in memory. Copies latest instructions to \code{directory} if the latest version is not already contained there. If the latest version is already present, a message is printed to the console.
#' @export
#'
get_latest_instructions <- function(directory = "instructions") {
  pptm <- rev(list.files(system.file('instructions', package = 'gaseous'),
                         pattern = '\\.pptm$', full.names = TRUE))[1]
  to <- paste0(directory, "/", basename(pptm))
  if (!file.exists(to)) {
    file.copy(from = pptm, to = to)
  } else {
    cat("Latest version of instructions already contained in", directory)
  }
}
