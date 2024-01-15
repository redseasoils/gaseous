#' Build and Prep Manual for Export
#'
#' For internal use only. Builds manual to inst/instructions and renames it to
#' match version annotation of pptm instructions.
#'
#' @return Nothing in memory.
#'
build_manual <- function() {
  devtools::build_manual(path = 'inst/instructions')
  year <- format(Sys.Date(), "%y")
  month <- format(Sys.Date(), "%m")
  from <- rev(list.files('inst/instructions', pattern = '\\.pdf$',
                         full.names = TRUE))[1]
  to <- paste0(dirname(from), '/gaseous_v', year, month, ".pdf")
  file.copy(from = from, to = to)
  file.remove(from)
}
