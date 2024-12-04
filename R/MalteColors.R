#' Function to return a color palette of length n
#'
#' Returns a character vector of length n containing Hex values for a color palette
#'
#' @param n Length of desired color vector
#' @examples
#' MalteColors(4)
#' @return Character vector
#' @export
MalteColors <- function(n = NULL) {
  if (is.null(n)) stop("Specify numeric argument \"n\" in MalteColors function!")
  if (!is.numeric(n)) stop("Argument \"n\" in MalteColors function must be numeric!")
  n <- as.integer(n)
  if (n <= 6) {
    c("#082a54", "#f0c571", "#cecece", "#e02b35", "#59a89c", "#a559aa")[1:n]
  } else {
    c(
      c("#082a54", "#f0c571", "#cecece", "#e02b35", "#59a89c", "#a559aa"),
      scales::hue_pal()(n-6)
    )
  }
}
