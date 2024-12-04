#' Print a program skeleton including header to the console
#'
#' Run this function without any arguments and it will print a template for a
#' new program header to the console. Copy-paste this into the new program and
#' fill in the relevant information. Version and date are filled in automatically.
#'
#' @param proj Name of the current project
#' @examples
#' new.header()
#'
#' @return Message to the console
#' @export
new.header <- function(
  proj = NULL
) {
  if (!is.null(proj) & !is.character(proj)) stop("Argument proj must be character")
  if (is.null(proj)) proj <- "< - PROJECT NAME - >"
  message("
#------------------------------------------------------------------------------#
#                               HEADER
#------------------------------------------------------------------------------#\n",
paste("### project:            ", proj, "\n", sep = ""),
"### program name:       < - FILENAME.r - > \n",

paste("### version/date:       V 1.00 / ", format(Sys.Date(), "%d-%b-%Y"), "\n"),

paste("### R version:          ", sessionInfo()$R.version$version.string, "\n", sep = ""),

"### purpose:            < - one line - >
### program specification:
###                     < - brief explanation of program steps - >
### prerequisites:      < - programs that need to have run before - >
### parameter / sample call:
###                     < - example of defined functions - >
### read:               ORACLE:
###                     < - snap1801.xyz - >
### write:              < - output_dir.name.file_ending - >
### validation process: < - double programming by ... / self-validation - >
### reference for documentation:
###                     < - G:\\...\\progs\\dateiname.r - >
###                     < - G:\\...\\Ablage\\SAP\\dateiname.pdf - >
### version control:
### date         version      author             change\n",

paste("### ", format(Sys.Date(), "%d%b%Y"), "    V 1.00       M. Braitmaier      initial release \n", sep = ""),

"#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#                               DIRECTORY PATHS
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#                               PACKAGES
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#                               INCLUDES
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#                               FUNCTION DEFINITONS
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#                               INPUT STEPS
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#                               CALCULATIONS
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#                               OUTPUT STEPS
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#                               FINISHED
#------------------------------------------------------------------------------#
"

)
}
