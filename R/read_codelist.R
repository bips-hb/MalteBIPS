#' Load a Codelist (from CSV)
#'
#' This function loads a codelist that is stored as CSV file. It assumes that the
#' general structure of codelists (e.g. from master codelists) of the Clinical
#' Epidemiology department was adhered to. The function also carries out some
#' basic plausibility checks and gives warnings if some common mistakes pop up.
#'
#' @param infile Path to the codelist (including name and file ending of codelist)
#' @param rm.special Logical; Should special characters be removed from the code column (this only looks at the columns ATC, EBM, ICD, OPS). Defaults to FALSE and gives a warning, if special characters are present.
#' @return Codelist as data.frame
#' @export
read.codelist <- function(
    infile,
    remove.special.characters = FALSE
) {

  ### read in file
  tmp <- read.csv(
    file = file.path(infile),
    sep = ";",
    header = T,
    as.is = T
  )
  ### get classes of columns
  tmp.classes <- rep(NA, ncol(tmp))
  for (i in 1:length(tmp.classes)) {
    tmp.classes[i] <- class(tmp[,i])
  }
  tmp.classes[names(tmp) %in% c("ATC", "EBM", "ICD", "OPS")] <- "character"

  ### read in the file again, but without removing leading 0's from codes
  tmp <- read.csv(
    file = file.path(infile),
    sep = ";",
    header = T,
    as.is = T,
    colClasses = tmp.classes,
    encoding = "latin1"
  )

  ### trim leading and trailing white space from all character variables
  colnames <- names(tmp)
  index <- rep(NA, length(colnames))
  for (i in 1:length(colnames)) {
    index[i] <- is.character(tmp[,colnames[i]])
  }
  charnames <- colnames[index]
  for (i in charnames) {
    tmp[,i] <- trimws(tmp[,i])
  }

  ### add domain and rename code variable
  if (grepl(".*ATC.*", toupper(infile))) {
    domain <- "ATC"
  } else if (grepl(".*EBM.*", toupper(infile))) {
    domain <- "EBM"
  } else if (grepl(".*ICD.*", toupper(infile))) {
    domain <- "ICD"
  } else if (grepl(".*OPS.*", toupper(infile))) {
    domain <- "OPS"
  } else {
    domain <- "???"
    warning("I couldn't guess the domain (ATC, EBM, ICD, OPS) from the filename...")
  }
  tmp$domain <- domain
  tmp$code <- tmp[,domain]
  tmp <- tmp[,!(names(tmp) %in% domain)]

  ### should special characters (apostrophes, !, -, ., ...) be removed? only use this option,
  ### when actually working with the codes. when writing the DMP and checking the codelists,
  ### never remove special characters, since it's necessary to explicitly write in the DMP that
  ### these symbols are present and must be removed.
  if (remove.special.characters) {
    tmp$code <- gsub("[[:punct:][:blank:]]", "", tmp$code)
  } else {
    ### detect, if special characters are present and, if so, give a warning message with instructions
    if (sum(grepl("[[:punct:][:blank:]]", tmp$code)) > 0) {
      spec.char <- unique(unlist(strsplit(paste(gsub("[^[:punct:][:blank:]]", "", tmp$code), collapse = ""), "")))
      spec.char <- paste("\"", spec.char, "\"", sep = "")
      warning(
        "The following special characters were detected in the codes: ",
        paste(spec.char, collapse = ", ")
      )
    }
  }

  ### write VAR_GRP and VAR_NAME in upper case letters
  if (sum("VAR_GRP" %in% names(tmp)) == 1) {
    tmp$VAR_GRP <- toupper(tmp$VAR_GRP)
  }
  if (sum("VAR_NAME" %in% names(tmp)) == 1) {
    tmp$VAR_NAME <- toupper(tmp$VAR_NAME)
  }

  ### check if there are validity times with an end year before a begin year (this happens
  ### when there are copy-paste error due to differing structures of master codelists)
  ###
  ### do this for the combinations (VAL_BEG_HOSP, VAL_END_HOSP), (VAL_BEG_AMB, VAL_BEG_AMB),
  ### and (VAL_BEG, VAL_END)
  val_beg_names <- c("VAL_BEG_HOSP", "VAL_BEG_AMB", "VAL_BEG")
  val_end_names <- c("VAL_END_HOSP", "VAL_END_AMB", "VAL_END")
  for (i in 1:length(val_beg_names)) {
    if (
      val_beg_names[i] %in% toupper(names(tmp)) &
      val_end_names[i] %in% toupper(names(tmp))
    ) {

      ### check if all dates are given to the same level of accuracy (i.e. either all
      ### as years or all as exact dates, otherwise this would need to be corrected by
      ### hand)
      val_table <- table(c(nchar(tmp[,val_beg_names[i]]), nchar(tmp[,val_end_names[i]])))
      if (length(unlist(attr(val_table, "dimnames"))[!(unlist(attr(val_table, "dimnames")) %in% "0")]) > 1) {
        warning(
          "The validity dates in ",
          val_beg_names[i],
          " and ",
          val_end_names[i],
          " seem to be given with differing levels of accuracy, please correct this.")
      }
      rm(val_table)

      ### in case both beginning and end time are given, check if some end dates are
      ### before the begin date
      if (sum(!is.na(tmp[,val_beg_names[i]]) & !(is.na(tmp[,val_end_names[i]]))) > 0) {
        val_tmp <- tmp[!is.na(tmp[,val_beg_names[i]]) & !(is.na(tmp[,val_end_names[i]])),]
        if (sum(val_tmp[,val_beg_names[i]] > val_tmp[,val_end_names[i]]) > 0) {
          warning(
            "There are ",
            val_beg_names[i],
            " values that are larger than their corresponding ",
            val_end_names[i],
            " values!"
          )
        }
        rm(val_tmp)
      }
    }
  }

  ### in case that some values were copy-pasted into a new column without name, R gives this
  ### column the name "X"
  if ("X" %in% names(tmp)) {
    if (length(table(tmp$X)) > 0) {
      warning("There is a non-empty, unnamed column \"X\", please make sure no values were copy-pasted into the wrong column.")
    }
  }

  ### Sort list
  tmp <- tmp[order(tmp$VAR_GRP, tmp$VAR_NAME, tmp$code),]

  ### return list
  tmp
}
