#' Send Oracle SQL code to collect column names of all variables in user-specified tables
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
#' @param schema Oracle schema where the tables are stored
#' @param tables Character vector containing the names of the tables to be queried
#' @param username Oracle username, defaults to NULL.
#' @param password Oracle password, defaults to NULL.
#' @param dbname Oracle database name, defaults to NULL.
#' @returns list object with one element per table containing information on schema, table name, columns, data type and length of variable
#' @export
oracle.contents <- function(
    schema,
    tables,
    username = NULL,
    password = NULL,
    dbname   = NULL
) {

  ### use upper case
  schema <- toupper(schema)
  tables <- toupper(tables)

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

  ### initialize output object
  output <- vector(mode = "list", length = length(tables))
  names(output) <- tables

  ### Loop over all tables to be queried
  for (i in 1:length(output)) {
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
      "SELECT owner, table_name, column_name, data_type, data_length FROM ALL_TAB_COLUMNS WHERE table_name = '",
      tables[i],
      "' and OWNER = '",
      schema,
      "'",
      sep = ""
    )

    ### send the query to the database
    query <- dbSendQuery(
      con,
      ora.code
    )

    ### retrieve output
    output[[i]] <- fetch(query)

    ### separate connection to database
    dbDisconnect(con)
  }

  ### return output
  output
}

