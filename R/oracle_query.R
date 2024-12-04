#' Send Oracle SQL code to the database and retrieve data
#'
#' This function assumes the following:
#' 1) The ROracle package is installed and loaded. It must be loaded manually,
#'    because I don't want to include it as dependency, because it needs to
#'    be set up with administrator rights.
#' 2) There is a SAS autoexec file in the usual form which contains
#'    the Oracle user credentials in the usual form. Alternatively,
#'    the credentials can be specified manually in the arguments username,
#'    password and dbname
#'
#' @param ora.code Oracle SQL code to be sent to the Oracle server
#' @param username Oracle username, defaults to NULL.
#' @param password Oracle password, defaults to NULL.
#' @param dbname Oracle database name, defaults to NULL.
#' @return Data retrieved from Oracle server, as data.frame
#' @export
oracle.query <- function(
  ora.code,
  username = NULL,
  password = NULL,
  dbname   = NULL
) {

  ### check if ROracle is installed and give a warning, if it isn't
  if (!("ROracle" %in% attr(installed.packages()[,1], "names")) &
      !("ROracle" %in% c(names(sessionInfo()$otherPkgs), names(sessionInfo()$loadedOnly)))) {
    stop("!!! oracle.query: No ROracle installation could be found.\n")
  }

  ### if any of the Oracle credentials are set to NULL, try to load them form the
  ### credentials file on the H drive
  if(is.null(username) | is.null(password) | is.null(dbname)) {

    username <- oracle.credentials("USERNAME")
    password <- oracle.credentials("PASSWORD")
    dbname   <- oracle.credentials("DBNAME")

  }

  ### set up connection to the database
  drv <- dbDriver("Oracle")
  con <- dbConnect(
    drv,
    username = username,
    password = password,
    dbname   = dbname
  )

  ### send the query to the database
  query <- dbSendQuery(
    con,
    ora.code
  )

  ### retrieve output
  output <- fetch(query)

  ### separate connection to database
  dbDisconnect(con)

  ### return output
  output
}
