# Export list of dois for ODDPup

geo_ressources <- read_excel("dashboard/data/geo_ressources.xlsx", 
                             sheet = "geo_journals")


dois <- geo_ressources |>
  filter(!is.na(doi)) |>
  filter(genre == "journal-article") |>
  select(doi)
write_csv(dois, "geosciences_dois_2016_2021.csv")