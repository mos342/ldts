#' Scan London Data store for updated resources
#' @name scan_ld
#' @return df_dplyr dataset for analysis
#' @param test  1 if simulate update event, 0 otherwise
#' @export
#' @import xlsx
#' @import dplyr
#' @import tidyr
#' @import ldndatar
#' @import stringr
#' @import mailR
#' @import RPostgres
#' @examples scan_ld()
options(scipen = 999)

scan_ld <- function(test = 1) {

  cnfg <- yaml::yaml.load_file(here::here("rconfig.yml"))

  conSuper <- db_connect()

  #extract slugs currently on dashboard (select from mtd table)
  slugs_long <- RPostgres::dbGetQuery(conSuper, "SELECT dataset,
                                                        link,
                                                        replace(replace(outcome,' ', '-'), ',', '') as outcome,
                                                        replace(subcat,' ', '-') as subcat
                                                 FROM mtd
                                                 WHERE link LIKE 'https://data.london.gov.uk/dataset/%'", n = Inf )%>%
                                                 separate(link, c("NA","NA1", "NA2","NA3", "slug"), sep = "/")

  slugs <- unique(slugs_long$slug)

  #scan London Data Store for all slug resources

  slug_res <-data.frame(matrix(ncol = 15, nrow = 0))
  colnames(slug_res) <- c("id","slug","title","updatedAt","maintainer_email","format","order","check_timestamp",
                           "resource_title","resource_id","url","update_frequency", "london_smallest_geography",
                           "temporal_coverage_from", "temporal_coverage_to")
  for(i in slugs){

        #print(glue::glue("slug is ", {i}))

        temp <- lds_meta_dataset(slug = {i}, api_key = Sys.getenv("api_key"), inc_tables = TRUE) %>%
          select(id, slug, title, updatedAt, maintainer_email, format, order, check_timestamp, resource_title,
                 resource_id, url, update_frequency, london_smallest_geography, contains("temporal_coverage"))
#        print(paste0(ncol(temp), ncol(slug_res)))
        slug_res <- rbind_diff_col(slug_res, temp)
      }

  slug_res$orderupd <- as.integer(slug_res$order )
  slug_res$uptdwhen <- (slug_res$updatedAt )

  ldndata_uptd <- slug_res[
    order( slug_res[,2], slug_res[,7] ),
  ]

  #Apply pattern to create and test resources' urls
  ldndata_uptd$filename <- stringr::str_extract(ldndata_uptd$url, "[^/]+$")
  ldndata_uptd$url <- paste(Sys.getenv("lds_url"), "download", ldndata_uptd$slug, ldndata_uptd$resource_id, ldndata_uptd$filename, sep = "/")

  ldndata_uptd$check <- sapply(ldndata_uptd$url, url_exists, USE.NAMES = FALSE)

  #select ldndata currently on dashboard database
  ldndata <- dbGetQuery(conSuper, "SELECT id AS ido, id, orderupd AS orderupdo,
                                          uptdwhen AS uptdwheno, check_timestamp AS check_timestampo, check_timestamp,
                                          slug AS slugo
                                   FROM ldndata")

  if(test == 1){
    ldndata <- test_date(ldndata)
  }

  df_dplyr <- merge(x = ldndata,
                    y = select(ldndata_uptd, c("id", "uptdwhen", "orderupd", "check_timestamp", "slug", "check", "url", "filename", "format", "update_frequency", "london_smallest_geography")),
                    by = c("id", "check_timestamp"), all = TRUE )


  #return(df_dplyr)
  # Issue: the postgres is truncating time by a small fraction of a second. To do: change datetime formatting
  # if uptdwhen > uptdwheno: select where check_timestamp > uptdwheno;
  updated <- df_dplyr %>%
    filter(difftime(uptdwhen, uptdwheno,         units = "secs") > 1) %>%
    filter(difftime(check_timestamp, uptdwheno,  units = "secs") > 1)

  if(nrow(updated) != 0){

  updated <- updated %>% replace_na(list(update_frequency  = "not available", london_smallest_geography = "not available"))

  #attach file location
  updated <- merge(x=updated, y=select(slugs_long, c('slug', "outcome", "subcat")), by.x='slug', by.y='slug')
  updated <- unique(updated)
  updated$location <- file.path(Sys.getenv('rootdir'), updated$outcome, updated$subcat)

  #write.xlsx(updated, file = 'test_prod.xlsx')
  #mylist <- unique(updated$slug)


for (sl in  unique(updated$slug)){

  #print(sl)

  myslug <- updated %>% filter(slug == sl)

  uptdwhen = as.Date(unique(myslug$uptdwhen))
  uptdwheno = as.Date(unique(myslug$uptdwheno))


  Body <- glue::glue("Dear,
            <br>",
               "The slug ",{sl}, " has been updated on London Datastore.<br>",
               "Date of update: ", {format(uptdwhen, "%A, %B %d, %Y") }, "<br>",
               "Previously updated: ",{format(uptdwheno, "%A, %B %d, %Y")}, "<br> <br>")


  for (row in 1:nrow(myslug)){
    slug <- myslug[row, "slug"]
    url <- myslug[row, "url"]
    resource_id <- myslug[row, "resource_id"]
    filename <- myslug[row, "filename"]
    format = myslug[row, "format"]
    check = myslug[row, "check"]
    uptdwhen = as.Date(myslug[row, "uptdwhen"])
    uptdwheno = myslug[row, "uptdwheno"]
    frequency = myslug[row, "update_frequency"]
    london_smallest_geography = myslug[row, "london_smallest_geography"]
    location = myslug[row, "location"]


    if(format %in% c("spreadsheet", "csv") & check == TRUE){
      download.file(url, glue::glue({location},'/',{filename}), mode = "wb")

      if(format %in% c("spreadsheet")){
      wb <- loadWorkbook(glue::glue({location},'/',{filename}))
      sheetnames <- names(getSheets(wb))
      sheet_num <- length(sheetnames)
         }

      Body_sub <- glue::glue(Body,
                       "Added File: ", {filename}, "<br>",
                       "Update Frequency: ", {frequency}, "<br>",
                       "Number of Sheets: ", {length(sheetnames)}, "<br>",
                       "Sheet Names: ", {paste0(sheetnames, collapse = ", ")}, "<br>" ,
                       "File Location: ", glue::glue({location},'/',{filename}), "<br>" ,
                       "Data Store download:", "<a href='", {url},"' > {filename} </a>","<br>","<br>"
                       )
      Body <- Body_sub
    }
    else if(format %in% c("csv") & check == TRUE){

      Body_sub <- glue::glue(Body,
                       "Added File: ", {filename}, "<br>",
                       "Update Frequency: ", {frequency}, "<br>",
                       "click to download:", "<a href='", {url},"' > {filename} </a>","<br>","<br>"  )
      Body <- Body_sub

      }

    }

  Body <- glue::glue(Body, "<br>", "<br>", "Best", "<br>", "The Dashboard")

  cnfg <- yaml::yaml.load_file(here::here("rconfig.yml"))

  mailR::send.mail(from = "Monika.Sieniawska@london.gov.uk",
            to = c("Monika.Sieniawska@london.gov.uk"),
            subject = "London DataStore has been updated",
            body = Body,
            html = TRUE,
            inline = TRUE,
            authenticate = TRUE,
            smtp = list(host.name = cnfg$hostnname,
                        user.name = cnfg$username,
                        passwd = cnfg$passwd))

}

  #Register updated files on database

  fileregister <- select(updated, c("check_timestamp", 	"filename", 	"slug", 	"subcat", 	"outcome", 	"url",
                                    "location", "format",  "update_frequency"))
  fileregister$check_timestamp <- as.Date(fileregister$check_timestamp)
  colnames(fileregister) <- c("dateupdated", "filename", "slug", "subcategory", "outcome", "url",
                              "location", "format","updatefreq")

  # extract files on the register
  #extract slugs currently on dashboard (select from mtd table)
  fileregister_o <- RPostgres::dbGetQuery(conSuper, "SELECT * FROM fileregister")

  if(nrow(fileregister_o) !=0){

    #append the new rows and de-duplicate
    fileregister <- rbind_diff_col(fileregister_o, fileregister)
    fileregister <- fileregister[!(duplicated(fileregister) | duplicated(fileregister, fromLast = TRUE)), ]
  }

  #write.xlsx(fileregister, file = file.path(Sys.getenv('rootdir'), 'fileregister.xlsx'))

  if(nrow(fileregister) !=0){
  values <- paste0(apply(fileregister, 1, function(x) paste0("('", paste0(x, collapse = "', '"), "')")), collapse = ", ")

  #3. Correct "NULL" notation
  values <- gsub("'NULL'", "NULL", values)
  values <- gsub("'NA'", "NULL", values)

  dbSendQuery(conSuper, paste0("INSERT INTO fileregister",  " VALUES ", values, ";"), n = Inf )
  }
}else{print("No new updates")}
}











