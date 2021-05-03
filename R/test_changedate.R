#' simulate update for testing
#'
#' @param dataset dataset to simulate update for testing
#' @export
#'
#' @import dplyr
#' @examples
#' dataset <- data.frame(matrix(ncol = 7, nrow = 0))
#' colnames(dataset) <- c("ido", "id", "orderupdo", "uptdwheno", "check_timestampo", "check_timestamp", "slugo")
#' test_date(dataset)
test_date <- function(dataset = ldndata){

  ldndata <- dataset

  ldndata$uptdwheno2 <- ldndata$uptdwheno
  ldndata$check_timestampo2 <- ldndata$check_timestampo

  if(nrow(ldndata) != 0){

  #modify update dates for testing
  ldndata$uptdwheno[ldndata$slugo %in% c("domestic-energy-efficiency-ratings-borough",
                                         "gcse-results-by-borough",
                                         "hbai-poverty")] <- ldndata$uptdwheno[ldndata$slugo %in%
                                                                                 c("domestic-energy-efficiency-ratings-borough",
                                                                                   "gcse-results-by-borough",
                                                                                   "hbai-poverty")] - as.difftime(5,unit = "days")
  ldndata$check_timestampo[ldndata$slugo %in% c("domestic-energy-efficiency-ratings-borough",
                                                "gcse-results-by-borough",
                                                "hbai-poverty")] <- ldndata$check_timestampo[ldndata$slugo %in%
                                                                                               c("domestic-energy-efficiency-ratings-borough",
                                                                                                 "gcse-results-by-borough",
                                                                                                 "hbai-poverty")] - as.difftime(5,unit = "days")
  }
  return(ldndata)
}

#ldndata2 <- test_date()
