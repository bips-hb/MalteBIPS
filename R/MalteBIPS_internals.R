#' Internal function to load Oracle credentials from the SAS autoexec
#'
#' @param element Character, must be USERNAME, PASSWORD, or DBNAME
#' @noRd
#' @keywords internal
oracle.credentials <- function(
    element
  ) {

  element <- toupper(element)

  tmp <- readLines("H:/netparam/My SAS Files/V9/autoexec.sas")
  for (i in 1:length(tmp)) {
    if (!grepl("^proc pwencode.+", tmp[i])) {
      tmp[i] <- tolower(tmp[i])
    }
  }

  if (element == "USERNAME") {
    gsub(
      "(^%let user.*=\\s*)(\\w+)(\\s*;.*)",
      "\\2",
      tmp[grepl("^%let user.+", tmp)]
    )
  } else if (element == "PASSWORD") {
    gsub(
      "(^proc pwencode.+')([[:alnum:]]+)('.+)",
      "\\2",
      tmp[grepl("^proc pwencode.+", tmp)]
    )
  } else if (element == "DBNAME") {
    gsub(
      "(^%let path.*=\\s*)(\\w+)(\\s*;.*)",
      "\\2",
      tmp[grepl("^%let path.+", tmp)]
    )
  } else {
    stop("[oracle.credentials: Argument element must be USERNAME, PASSWORD, or DBNAME")
  }
}
