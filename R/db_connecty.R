#' create db connection
#'
#' @return conSuper returning connection for use in other functions
#' @export
#'
#' @examples
#' db_connect()
db_connect <- function(){
cnfg <- yaml::yaml.load_file(here::here("rconfig.yml"))
conSuper <- RPostgres::dbConnect( RPostgres::dbDriver("Postgres"),
                                  dbname   = cnfg$dbname,
                                  host     = cnfg$host,
                                  port     = cnfg$port,
                                  password = cnfg$password,
                                  user     = cnfg$user
                                )
                              return(conSuper)
                            }
