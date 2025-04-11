#' Scan a Log file for errors, warnings and suspicious messages
#'
#' Load a log file and scan it for errors, warnings and suspicious messages.
#' The function has two modi: 1) if r.sas = R, it expects an R log (e.g. the .Rout
#' file that is generated when using R from the command line) or 2) if r.sas = SAS, it
#' expects a .log file from a SAS session.
#'
#' @param logfile Path to the logfile, including file ending
#' @param r.sas Either "R" or "SAS", defaults to "SAS"
#' @return Findings as a data.frame
#' @export
logreader <- function(
  logfile = NULL,
  r.sas = "SAS"
) {
  if(is.null(logfile)) stop("Provide path to logfile")
  r.sas <- toupper(r.sas)
  if (!(r.sas %in% c("R", "SAS"))) stop("Argument r.sas must be either \"R\" or \"SAS\"")
  ### check if it is actually an R/SAS log
  if (r.sas == "R" & !(grepl(".+\\.Rout", logfile))) {
    warning("File ending \\.Rout could not be detected, are you sure this is an R log?")
  }
  if (r.sas == "SAS" & !(grepl(".+\\.log", logfile))) {
    warning("File ending \\.log could not be detected, are you sure this is a SAS log?")
  }
  ### read logfile
  if (r.sas == "SAS") {
    lines <- data.frame(readLines(logfile, encoding = "latin1"), stringsAsFactors = F)
  } else if (r.sas == "R") {
    lines <- data.frame(readLines(logfile), stringsAsFactors = F)
  }
  names(lines) <- "log"
  lines$rownumber <- 1:nrow(lines)

  ### if it is an R log
  if (r.sas == "R") {
    ### identify errors and warnings:
    lines$errors   <- as.numeric(grepl("^[Ee][Rr][Rr][Oo][Rr].*", lines$log) |
                                   grepl("^Fehler:.*", lines$log))
    lines$warnings <- as.numeric(grepl("^Warning message.*", lines$log) |
                                   grepl("^Warnmeldung.*", lines$log))

    # ### identify relevant messages:
    lines$messages <- 0
    # lines$messages <- as.numeric(
    #   grepl(".*enter suspicious text.*", lines$log)
    # )

    ### overall index for findings
    lines$fishy <- as.numeric(lines$errors == 1 | lines$warnings == 1 | lines$messages == 1)

    if (sum(lines$fishy) > 0) {
      ### prepare output of findings
      output <- lines[
        lines$errors == 1 | lines$warnings == 1 | lines$messages == 1,
        names(lines) %in% c("rownumber", "log")
      ]

      ### retrieve more information from errors and warnings:
      for (i in 1:nrow(output)) {
        tmp.lines <- lines
        if (grepl("^Error:.*", output$log[i]) | grepl("^Warning message.*", output$log[i])) {
          tmp.num <- output$rownumber[i]
          tmp.lines$this <- as.numeric(lines$rownumber > tmp.num & as.numeric(!grepl("^>.*", lines$log)))
          tmp.index <- 1
          for (j in (tmp.num+1):nrow(tmp.lines)) {
            if (tmp.lines[j,"this"] == 0) tmp.index <- 0
            if (tmp.index == 0) tmp.lines[j,"this"] <- 0
          }
          if (sum(tmp.lines$this > 0)) {
            output[i,"log"] <- paste(
              output[i,"log"],
              paste(tmp.lines[tmp.lines$this == 1,"log"], collapse = "\n "),
              sep = "\n "
            )
          }
          rm(tmp.num, tmp.index)
        }
        rm(tmp.lines)
      }
    } else {
      output <- data.frame(
        rownumber = 0,
        log = "No issues found!"
      )
    }
  } else if (r.sas == "SAS") {
    ### identify errors and warnings:
    lines$errors   <- as.numeric(grepl("^[Ee][Rr][Rr][Oo][Rr].*", lines$log))
    lines$warnings <- as.numeric(grepl("^[Ww][Aa][Rr][Nn][Ii][Nn][Gg].*", lines$log))
    ### identify suspicious notes:
    lines$log <- enc2utf8(lines$log) ### I had encoding issues and error messages regarding "multibyte strings", this seems to fix it
    lines$LOG <- toupper(lines$log)
    lines$notes    <- as.numeric(
      grepl("^NOTE.*", lines$log) &
       (grepl(".*STOPPED.*", lines$LOG) |
        grepl(".*CONVERTED.*", lines$LOG) |
        grepl(".*UNINITIALIZED.*", lines$LOG) |
        grepl(".*DIVISION BY ZERO.*", lines$LOG) |
        grepl(".*INVALID.*", lines$LOG) |
        grepl(".*MISSING.*", lines$LOG) |
        grepl(".*IS ALREADY ON THE LIBRARY.*", lines$LOG) |
        grepl(".* 0 OBSERVATIONS.*", lines$LOG) |
        grepl(".* 0 VARIABLES.*", lines$LOG) |
        grepl(".*MERGE STATEMENT.*", lines$LOG) |
        grepl(".*AT LEAST ONE W.D..*", lines$LOG) |
        grepl(".*NO STATISTICS.*", lines$LOG) |
        grepl(".*OVERWRITTEN.*", lines$LOG) |
        grepl(".*NOSPOOL.*", lines$LOG) |
        grepl(".*MATHEMATICAL.*", lines$LOG) |
        grepl(".*OUTSIDE AXIS RANGE.*", lines$LOG) |
        grepl(".*LOST CARD.*", lines$LOG) |
        grepl(".*SAS WENT TO A NEW LINE.*", lines$LOG) |
        grepl(".*REPEAT.*", lines$LOG) |
        grepl(".*MORE THAN ONE.*", lines$LOG) |
        grepl(".*UNRESOLVED.*", lines$LOG) |
        grepl(".*NOT RESOLVED.*", lines$LOG) |
        grepl(".*REFERENCED.*", lines$LOG) |
        grepl(".*EMPTY*", lines$LOG) |
        grepl(".*W.D. FORMAT*", lines$LOG))
    )

    ### extract information that is relevant for output
    if (sum(lines$errors) + sum(lines$warnings) + sum(lines$notes) > 0) {
      output <- lines[
        lines$errors == 1 | lines$warnings == 1 | lines$notes == 1,
        names(lines) %in% c("rownumber", "type", "log")
      ]
    } else {
      output <- data.frame(
        rownumber = 0,
        log = "No issues found!"
      )
    }
  }

  row.names(output) <- 1:nrow(output)

  output
}
