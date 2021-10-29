
#' @title Get the Best Response in ADSL Data
#'
#' @description TODO: add this later
#' @param x an ADSL dataset for a single patient with RSRESP.
#' @return The best response for the patient.
#' @importFrom dplyr arrange mutate select desc %>%
#' @importFrom stats na.omit
#' @export
get_best_resp <- function(x) {
  ret <- x %>%
    select(RSRESP) %>%
    na.omit() %>%
    mutate(rsresp_num = as.integer(RSRESP)) %>%
    arrange(desc(rsresp_num))
  ret$RSRESP[1]
}

#' @title Get the Worst AE
#'
#' @description TODO: add this later
#' @param x an AE data set for a single patient with AESEV.
#' @return The worst ae for the patient.
#' @export
get_worst_ae <- function(x) {
  na_ret <- factor(NA,
                   levels = c("Mild", "Moderate", "Severe",
                              "Life threatening", "Fatal"))

  if (is.null(x) || nrow(x) == 0) {
    return(na_ret)
  }
  ret <- x$AESEV[which.max(as.integer(x$AESEV))]
  if (length(ret)  == 0) {
    ret <- na_ret
  }
  ret
}
