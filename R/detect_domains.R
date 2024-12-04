#' Check which domains (ATC, EBM, ICD, OPS) occur in each VAR_NAME
#'
#' Expects concatenated codelists (one data.frame) resulting from the function
#' output of read.codelist()
#'
#' @param code.frame data.frame containing the relevant code groups and a
#' column indicating which domains are to be used (can simply be the
#' concatenated codelists)
#' @param grp.vars Vector of column names for grouping of variables (defaults
#' to c("VAR_GRP", "VAR_NAME"))
#' @param domain.var Character string indicating column name of column giving
#' domain information (defaults to "domain")
#' @return data.frame
#' @export
detect.domains <- function(
    code.frame,
    grp.vars = c("VAR_GRP", "VAR_NAME"),
    domain.var = "domain"
) {

  ### prepare variable combining VAR_GRP and VAR_NAME (or other, specified variables)
  tmp <- code.frame
  for (i in 1:length(grp.vars)) {
    if (i == 1) {
      tmp$keyvar <- code.frame[,grp.vars[i]]
    } else {
      tmp$keyvar <- paste(tmp$keyvar, code.frame[,grp.vars[i]])
    }
  }

  ### loop over all unique keyvar values and obtain relevant domain information
  for (i in unique(tmp$keyvar)) {
    tmp[tmp$keyvar == i, "relevant.domains"] <-
      paste(
        sort(tmp[tmp$keyvar == i, domain.var][!duplicated(tmp[tmp$keyvar == i, domain.var])]),
        collapse = ", ")
  }

  ### initialize output with one entry per VAR_GRP-VAR_NAME combination
  out <- tmp[,names(tmp) %in% c(grp.vars, "keyvar", "relevant.domains")]
  out <- out[!duplicated(out$keyvar),]
  out <- out[,!(names(out) %in% c("keyvar"))]

  ### sort output
  out <- out[order(out$relevant.domains),]

  ### return output
  out
}
