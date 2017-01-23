library(lubridate)
library(rvest)
library(stringr)
library(tidyverse)

#TODO: string->numeric for parties, fix datum for Berlin >2013, filter sonstiges


# Scrape Laender polls page, select second table, which contains poll data then
# coerce into dataframe
lwa <- "http://www.wahlrecht.de/umfragen/laender.htm" %>%
  read_html() %>%
  html_nodes("table") %>%
  .[[2]] %>%
  html_table(fill = TRUE)

# Find Headers for the 16 Laender using regex, clean up columns
lwa %<>%
  mutate(
    header   = ifelse(grepl("(Bundestagswahl)", CDU), TRUE, FALSE),
    institut = gsub( "\\(.*$", "",  x = `Institut(Datum)`),
    datum    = dmy(gsub( ".*\\((.*)\\).*", "\\1", x = `Institut(Datum)`)),
    befragte = as.numeric(gsub( "[.]", "", str_sub(BefragteZeitraum, 1, 5))),
    afd      = ifelse(grepl("AfD", Sonstige),
                        gsub("^[^AfD]*AfD([^%]+).*", "\\1", Sonstige)
                        , NA)
  ) %>%
  select(
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
    sonstige = Sonstige
  )


lwa %<>% map_at(c("cdu", "spd", "gru", "lin", "fdp", "fdp", "afd"), pct2num)


# Get list of rownumbers for splitting
lae_rn <- rownames(lwa[lwa$header == TRUE, ])

# Split dataframe into list
tab_bw <- split(lwa, cumsum(1:nrow(lwa) %in% lae_rn))

# Delete first df which only contains info that we don't need, then assign names
tab_bw[1] <- NULL
names(tab_bw) <- c("BW", "BY", "BE", "BB", "HB", "HH", "HE", "MV", "NI", "NW", "RP", "SL", "SN", "ST", "SH", "TH")

# Bind into one dataframe, add identifier and filter garbage
tab_bw %<>% bind_rows(.id = "land")
tab_bw %<>% filter(befragte < 2001 & !grepl("Bundestagswahl", institut))

