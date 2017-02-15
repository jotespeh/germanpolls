scrape_laender <- function(list = FALSE) {
  library(lubridate)
  library(magrittr)
  library(rvest)
  library(stringr)
  library(tidyverse)
  
  #TODO: make AfD regex work if AfD ist not at beginning of string, fix befragte for inconsistent entries (s. HH newest, T • , O •), write some documentation
  
  # Scrape Laender polls page, select second table, which contains poll data then coerce into dataframe
  lwa <- "http://www.wahlrecht.de/umfragen/laender.htm" %>%
    read_html() %>%
    html_nodes("table") %>%
    .[[2]] %>%
    html_table(fill = TRUE)
  
  # Find header rows for the 16 Laender using regex, clean up columns
  lwa %<>%
    mutate(
      header   = ifelse(grepl("(Bundestagswahl)", CDU), TRUE, FALSE),
      institut = gsub("\\(.*$", "",  x = `Institut(Datum)`),
      # Somewhat ugly fix for Berlin polling
      datum    = dmy(ifelse(
        grepl("\\(", x = `Institut(Datum)`),
        gsub(".*\\((.*)\\).*", "\\1", x = `Institut(Datum)`),
        str_sub(`Institut(Datum)`, -10, -1)
      )),
      befragte = as.numeric(ifelse(
        str_sub(BefragteZeitraum, 1, 1) <= 2,
        gsub("[.]", "", str_sub(BefragteZeitraum, 1, 5)),
        str_sub(BefragteZeitraum, 1, 3)
      )),
      afd      = as.numeric(ifelse(
        grepl("AfD", Sonstige) | grepl("\\%AfD", Sonstige),
        gsub("^[^AfD]*AfD(*[^%]+).*", "\\1", Sonstige),
        NA
      )),
      sonstige = ifelse(
        grepl("Sonst", Sonstige),
        str_extract(Sonstige, "\\. (\\d{1})"),
        Sonstige
      )
    ) %>%
    select(
      auftraggeber = `Auftrag-geber`,
      header,
      datum,
      institut,
      befragte,
      cdu = CDU,
      spd = SPD,
      gru = `GRÜNE`,
      lin = LINKE,
      fdp = FDP,
      afd,
      sonstige
    ) %>%
    filter(!grepl("Bundestagswahl", .$institut), !grepl("Institut", .$institut))
  # Another Berlin Fix
  lwa %<>% mutate(institut = ifelse(
    auftraggeber == "BerlinerZeitung" & datum > "01-01-2014",
    "Forsa",
    institut
  ))
  
  # Percentage-string to numeric, loses precision, but no poll > 2013 has decimal
  lwa %<>% map_at(c("cdu", "spd", "gru", "lin", "fdp", "fdp", "sonstige"),
                  str_extract, pattern = "\\d{1,2}") %>%
    map_at(c("cdu", "spd", "gru", "lin", "fdp", "fdp", "sonstige"),
           as.numeric) %>%
    as.data.frame()
  
  # Get vector of rownumbers for splitting 
  # Splitting is necessary, as all laender are in one table
  lae_rn <- rownames(lwa[lwa$header == TRUE,])
  # Split dataframe into list, using rownumbers 
  tab_bw <- split(lwa, cumsum(1:nrow(lwa) %in% lae_rn))
  
  # Delete first df which only contains info that we don't need, then assign names
  tab_bw[1] <- NULL
  names(tab_bw) <-
    c(
      "BW",
      "BY",
      "BE",
      "BB",
      "HB",
      "HH",
      "HE",
      "MV",
      "NI",
      "NW",
      "RP",
      "SL",
      "SN",
      "ST",
      "SH",
      "TH"
    )
  
  tab_bw %<>%
    map(~filter(., .$header == FALSE)) %>%
    map(~select(., -header)) %>%
    map(~filter(., !is.na(.$cdu)))
  
  if (list == FALSE) {
    # Bind into one dataframe
    tab_bw %<>% bind_rows(.id = "land")
    return(tab_bw)
  }
  else
    return(tab_bw)
}
