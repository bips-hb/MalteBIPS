#' Send Oracle SQL code to collect names of all tables that are stored
#' on an Oracle schema
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
#' @param schema Oracle schema to check for tables
#' @param username Oracle username, defaults to NULL.
#' @param password Oracle password, defaults to NULL.
#' @param dbname Oracle database name, defaults to NULL.
#' @param only_T Logical, only query tables starting with "T_", defaults to FALSE. This argument makes sense when querying snapshots, but not when querying project schemas.
#' @returns data.frame with names of each table on the schema
#' @export
oracle.tables <- function(
  schema,
  username = NULL,
  password = NULL,
  dbname   = NULL,
  only_T   = FALSE
) {

  ### make sure the schema is in upper case
  schema <- toupper(schema)

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

  ### define the code to be submitted
  ora.code <- paste(
    "SELECT DISTINCT table_name, owner FROM all_tables WHERE OWNER = '",
    schema,
    "' ORDER BY table_name ASC",
    sep = ""
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

  ### Only keep tables that start with "T_"
  if (only_T) {
    output <- output[grepl("^T_.+", output$TABLE_NAME),]
  }

  ### drop the tables with "DROP" at the end
  output <- output[!grepl(".+_DROP.*", output)]

  ### return output
  output
}
