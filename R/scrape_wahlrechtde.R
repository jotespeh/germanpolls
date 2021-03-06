  #' A function to scrape Wahlrecht.de
  #' 
  #' germanpolls() is a function that gets polling data from [Wahlrecht.de](http://www.wahlrecht.de) for seven polling organisations and ten parties.
  #' @importFrom magrittr %>%
  #' @importFrom rvest html_table
  #' @importFrom xml2 read_html
  #' @importFrom dplyr filter
  #' @importFrom dplyr rename
  #' @importFrom dplyr mutate
  #' @importFrom dplyr select
  #' @importFrom purrr map
  #' @importFrom purrr map_at
  #' 
  #' @examples
  #' polling_data <- germanpolls() 
  #' 
  #' @export
  #'

  germanpolls <- function() {
    
  library("purrr")
  library("rvest")
  library("XML")
  library("magrittr")
  library("dplyr")
  library("tidyr")
  library("zoo")


  # scrape data
  institute <- c("allensbach", "emnid", "forsa", "politbarometer", "gms", "dimap", "insa")
  parteien <- c("cdu/csu", "spd", "grüne", "fdp", "linke", "afd", "sonstige", "nw_un", "fw")
  header <- c("CDU/CSU", "SPD", "GRÜNE", "FDP", "LINKE", "AfD", "Sonstige", "Nichtwähler/Unentschl.", "FW", "Befragte", "Zeitraum")

  df <-
    map_df(institute, function(i) {
      # keep calm! Fortschrittsbalken
      cat(".")
      path <- paste0("http://www.wahlrecht.de/umfragen/", i, ".htm")
      page <- read_html(path, encoding = "utf-8")
      
      rdf <- list()
      table <- html_node(page, ".wilko")
      table_head <- html_nodes(table, xpath = "//thead//th") %>% html_text()

      rdf[["datum"]] <- html_nodes(page, xpath = "//table//tbody//tr//td[1]") %>% html_text()
      rdf[["institut"]] <- rep(i, length = length(rdf[["datum"]]))

      for(head_el in seq_along(header)){
        vec <- vector()
        hindex <- match(header[head_el], table_head)
        # fuck up, extra loop because table of politbarometer is strange structured (td-element instead of th-element)
        if(i == "politbarometer" && (header[head_el] == "Befragte" || header[head_el] =="Zeitraum")){
          hindex = hindex + 1
        }
        checkvec <- html_nodes(page, xpath = paste0("//table//tbody//tr//td[", hindex,"]")) %>% html_text()

        if (length(checkvec) > 0){
          vec <- checkvec
          rdf[[header[head_el]]] <- vec
        }
      }
      as.data.frame(do.call(cbind, as.list(rdf)), stringsAsFactor = F)
    })

  # clean data
  names(df) %<>% tolower()

  df %<>%
    # filter(grepl("%", `cdu/csu`)) %>%
    filter(!grepl("Bundestagswahl", befragte)) %>%
    rename(nw_un = `nichtwähler/unentschl.`) %>%
      select(institut, datum, befragte, zeitraum, `cdu/csu`, spd, `grüne`, fdp, linke, afd, sonstige, nw_un,fw) %>%
    mutate(typ = ifelse(
      grepl("O • ", befragte), "online",
      ifelse(grepl("T • ", befragte), "telefon", "keineangabe")))

  # gsub
  df %<>% map(gsub, pattern = " %|≈|O • |T • |[?]", replacement = "")
  df %<>% map(gsub, pattern = ",", replacement = ".")
  df %<>% map_at("befragte", gsub, pattern = "[.]", replacement = "")

  # classes
  df %<>% map_at(parteien, as.numeric)
  df %<>% map_at("befragte", as.numeric)
  df$datum <- as.Date(df$datum, "%d.%m.%Y")

  df %<>% as.data.frame()
  df <- filter(df, !is.na(df$befragte))

  # transform data set to longform
  df_l <- df %>% gather(partei, anteil, -institut, -datum, -befragte, -zeitraum, -typ)
  df_l$anteil <- df_l$anteil/100

  df


    
  }
