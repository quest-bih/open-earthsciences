#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Functions for dashboard ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Open access status of journals ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Data preparation
oa_status_colors <- c("gold", "hybrid", "green", "bronze", "closed", "no doi")
color <- c("#F4C244", "#A0CBDA", "#4FAC5B", "#D85DBF", "#2C405E", "#5F7036")

# geo_ressources <- read_excel("dashboard/data/geo_ressources.xlsx", 
#                              sheet = "geo_journals")

geo_ressources <- read_excel("data/geo_ressources.xlsx", 
                             sheet = "geo_journals")

data_oa_journals <- function(total_perc) {
  
  if (total_perc == FALSE) {
  
  data <- geo_ressources |>
    drop_na(institut_ubib) |>
    mutate(
      institut_ubib = case_when(
        institut_ubib == "Institut für Weltraumwissenschaften" ~ "Institut für Meteorologie (WE 3)",
        TRUE ~ institut_ubib
      )
    ) |>
    count(year, oa_status, institut_ubib) |>
    filter(year >= 2016) |>
    mutate(institut_ubib = str_remove(institut_ubib, "Institut für")) |>
    mutate(institut_ubib = str_trim(institut_ubib)) |>
    mutate(institut_ubib = factor(
      institut_ubib,
      levels = c(
        "Geologische Wissenschaften (WE 1)",
        "Geographische Wissenschaften (WE 2)",
        "Meteorologie (WE 3)"
      )
    )) |>
    tidyr::complete(institut_ubib, year, oa_status, fill = list(n = 0)) |>
    mutate(oa_status = factor(oa_status, levels = oa_status_colors)) |>
    mutate(institut_short = str_extract_all(institut_ubib,  "(?<=\\().+?(?=\\))")) |>
    mutate(institut_short = factor(institut_short, levels = c("WE 1", "WE 2", "WE 3"))) |>
    pivot_wider(names_from = oa_status, values_from = n)
  
  } else {
    
    data <- geo_ressources |>
      drop_na(institut_ubib) |>
      mutate(
        institut_ubib = case_when(
          institut_ubib == "Institut für Weltraumwissenschaften" ~ "Institut für Meteorologie (WE 3)",
          TRUE ~ institut_ubib
        )
      ) |>
      count(year, oa_status, institut_ubib) |>
      filter(year >= 2016) |>
      mutate(institut_ubib = str_remove(institut_ubib, "Institut für")) |>
      mutate(institut_ubib = str_trim(institut_ubib)) |>
      mutate(institut_ubib = factor(
        institut_ubib,
        levels = c(
          "Geologische Wissenschaften (WE 1)",
          "Geographische Wissenschaften (WE 2)",
          "Meteorologie (WE 3)"
        )
      )) |>
      tidyr::complete(institut_ubib, year, oa_status, fill = list(n = 0)) |>
      group_by(institut_ubib, year) |>
      mutate(perc = n/sum(n)) |>
      ungroup() |>
      mutate(oa_status = factor(oa_status, levels = oa_status_colors)) |>
      mutate(institut_short = str_extract_all(institut_ubib,  "(?<=\\().+?(?=\\))")) |>
      mutate(institut_short = factor(institut_short, levels = c("WE 1", "WE 2", "WE 3"))) |>
      pivot_wider(id_cols = -n, names_from = oa_status, values_from = perc)
  }
}

plot_oa_journals <- function(data, min, max, total_perc, institut) { #min, max
  
  # data %>%
  #   filter(institut_ubib %in% input$checkGroup)
  
  if (total_perc == FALSE) {
  data |>
    filter(year >= min & year <= max) |> 
    filter(institut_short %in% institut) |>
    plot_ly(
      x = ~ list(year, institut_short)) |>
    add_bars(y = ~ gold,
             marker = list(color = color[1]),
             name = "gold") |>
    add_bars(y = ~ hybrid,
             marker = list(color = color[2]),
             name = "hybrid") |>
    add_bars(y = ~ green,
             marker = list(color = color[3]),
             name = "green") |>
    add_bars(y = ~ bronze,
             marker = list(color = color[4]),
             name = "bronze") |>
    add_bars(y = ~ closed,
             marker = list(color = color[5]),
             name = "closed") |>
    add_bars(y = ~ `no doi`,
             marker = list(color = color[6]),
             name = "no doi") |>
    layout(barmode = "relative",
           xaxis = list(title = FALSE),
           yaxis = list(title = FALSE),
           paper_bgcolor = "#DCE3E5",
           plot_bgcolor = "#DCE3E5") |>
      config(displayModeBar = FALSE)
  }
  else {
    data |>
      filter(year >= min & year <= max) |> 
      filter(institut_short %in% institut) |>
      plot_ly(
        x = ~ list(year, institut_short)) |>
      add_bars(y = ~ gold,
               marker = list(color = color[1]),
               name = "gold") |>
      add_bars(y = ~ hybrid,
               marker = list(color = color[2]),
               name = "hybrid") |>
      add_bars(y = ~ green,
               marker = list(color = color[3]),
               name = "green") |>
      add_bars(y = ~ bronze,
               marker = list(color = color[4]),
               name = "bronze") |>
      add_bars(y = ~ closed,
               marker = list(color = color[5]),
               name = "closed") |>
      add_bars(y = ~ `no doi`,
               marker = list(color = color[6]),
               name = "no doi") |>
      layout(barmode = "relative",
             xaxis = list(title = FALSE),
             yaxis = list(title = FALSE,
                          tickformat = ".0%",
                          range = c(0, 1)),
             paper_bgcolor = "#DCE3E5",
             plot_bgcolor = "#DCE3E5") |>
      config(displayModeBar = FALSE)
  }
  
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Licenses of journals ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

plot_journals_licenses <- function(total_perc) {
  
  # journals_licenses <- geo_ressources |>
  #   drop_na(institut_ubib) |>
  #   mutate(
  #     institut_ubib = case_when(
  #       institut_ubib == "Institut für Weltraumwissenschaften" ~ "Institut für Meteorologie (WE 3)",
  #       TRUE ~ institut_ubib
  #     )
  #   ) |>
  #   mutate(institut_ubib = str_remove(institut_ubib, "Institut für")) %>%
  #   mutate(institut_ubib = str_trim(institut_ubib)) %>%
  #   mutate(institut_ubib = factor(institut_ubib, levels = c("Geologische Wissenschaften (WE 1)", 
  #                                                           "Geographische Wissenschaften (WE 2)",  
  #                                                           "Meteorologie (WE 3)"))) |>
  #   mutate(license = case_when(!str_detect(license, "^cc.*") ~ "other license",
  #                              is.na(license) ~ "no license",
  #                              TRUE ~ license)) |>
  #   count(license, institut_ubib, oa_status) |>
  #   tidyr::complete(institut_ubib, license, oa_status, fill = list(n = 0)) |>
  #   mutate(license = factor(license, 
  #                           levels = c("cc-by",
  #                                      "cc-by-sa",
  #                                      "cc-by-nc",
  #                                      "cc-by-nc-sa",
  #                                      "cc-by-nd",
  #                                      "cc-by-nc-nd",
  #                                      "cc0",
  #                                      "other license",
  #                                      "no license"))) |>
  #   mutate(oa_status = factor(oa_status, levels = oa_status_colors)) |>
  #   mutate(institut_short = str_extract_all(institut_ubib,  "(?<=\\().+?(?=\\))")) |>
  #   mutate(institut_short = factor(institut_short, levels = c("WE 1", "WE 2", "WE 3"))) |>
  #   pivot_wider(id_cols = -n, names_from = oa_status, values_from = perc)
  # 
  
  journals_licenses <- geo_ressources |>
    drop_na(institut_ubib) |>
    mutate(license = case_when(!str_detect(license, "^cc.*") ~ "other license",
                               is.na(license) ~ "no license",
                               TRUE ~ license)) |>
    count(license, oa_status) |>
    mutate(license = factor(license, 
                            levels = c("cc-by",
                                       "cc-by-sa",
                                       "cc-by-nc",
                                       "cc-by-nc-sa",
                                       "cc-by-nd",
                                       "cc-by-nc-nd",
                                       "cc0",
                                       "other license",
                                       "no license"))) |>
    mutate(oa_status = factor(oa_status, levels = oa_status_colors)) |>
    tidyr::complete(license, oa_status, fill = list(n = 0)) |>
    #pivot_wider(id_cols = -n, names_from = oa_status, values_from = perc)
    group_by(license) |>
    mutate(perc = n/sum(n)) |>
    ungroup()
  
  
  if (total_perc == FALSE) {
  journals_licenses |>
    plot_ly(x = ~ license,
            y = ~ n,
            color = ~ oa_status,
            colors = color) |>
    add_bars() |>
    layout(barmode = "stack",
           xaxis = list(title = "Lizenz"),
           yaxis = list(title = FALSE),
           paper_bgcolor = "#DCE3E5",
           plot_bgcolor = "#DCE3E5") |>
      config(displayModeBar = FALSE)
  } else{
    journals_licenses |>
      plot_ly(x = ~ license,
              y = ~ perc,
              color = ~ oa_status,
              colors = color) |>
      add_bars() |>
      layout(barmode = "stack",
             xaxis = list(title = "Lizenz"),
             yaxis = list(title = FALSE,
                          tickformat = ".0%"),
             paper_bgcolor = "#DCE3E5",
             plot_bgcolor = "#DCE3E5") |>
      config(displayModeBar = FALSE)
  }
  
  
  # journals_licenses |>
  #   plot_ly(x = ~ perc,
  #           y = ~ institut_ubib,
  #           color = ~ license) |>
  #   add_bars() |>
  #   layout(barmode = "stack",
  #          title = "Lizenzen der ZS-Artikel 2016-2021",
  #          xaxis = list(title = FALSE,
  #                       tickformat = ".0%"),
  #          yaxis = list(title = FALSE,
  #                       autorange = "reversed"),
  #          legend = list(traceorder = "normal"))
  
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Other ressources ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Other ressources oa status ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

  

path1 <- "data/we2_alt_publ3.xlsx"  # Specifying the path for file
sheet1 = excel_sheets(path1) # Accessing all the sheets
# Applying sheet names to dataframe names
we2_resources <- lapply(setNames(sheet1, sheet1), 
                        function(x) read_excel("data/we2_alt_publ3.xlsx", sheet = x))
# Combine each file separately
we2_resources <- bind_rows(we2_resources, .id="Sheet")

# Attaching all dataframes together of the first institut
we2_resources <- we2_resources |> 
  select(-c(starts_with(".."), "DOI", "Website"))

# Specifying the path for file
path2 <- "data/we3_alt_publ.xlsx"  
# Accessing all the sheets
sheet2 = excel_sheets(path2) 
# Applying sheet names to dataframe names
we3_resources <- lapply(setNames(sheet2, sheet2), 
                        function(x) read_excel("data/we3_alt_publ.xlsx", sheet = x))
# Combine each file separately
we3_resources <- bind_rows(we3_resources, .id="Sheet")
# Attaching all dataframes together of the first institut
we3_resources <- we3_resources |> 
  select(-c(starts_with(".."))) #"DOI", "Website"

# Convert atributes to the correct formats
we3_resources$oa_date <- as.POSIXct(we3_resources$oa_date, tz = "UTC")
we3_resources$year <- as.numeric(we3_resources$year)
we3_resources$year_ubib <- as.numeric(we3_resources$year_ubib)

# Join two dataframes together of both institutes
data_other <- full_join(we2_resources, we3_resources)
  # Clean data
  
  
  data_oa_other <- function(total_perc) {
    if (total_perc == FALSE) {
    data <- data_other |>
      mutate(
          oa_status = case_when(
            str_detect(oa_status, "^closed.*") ~ "closed",
            str_detect(oa_status, "^gold.*") ~ "gold",
            is.na(oa_status) ~ "closed",
            TRUE ~ oa_status
          )
        ) |>
        mutate(oa_status = factor(oa_status, levels = oa_status_colors[1:5])) |>
        mutate(genre = case_when(str_detect(genre, "book-chapter|School book chapter") ~ "Book chapter",
                                 str_detect(genre, "[mM]onografie|[bB]uch|book") ~ "Book",
                                 #str_detect(genre, "[jJ]ournal-article") ~ "Journal article",
                                 str_detect(genre, "conference-paper") ~ "Conference paper",
                                 str_detect(genre, "conference-abstract") ~ "Conference abstract",
                                 TRUE ~ "Other")) |>
        count(year, oa_status, institut_ubib, genre) |>
        filter(year >= 2016) |>
        mutate(institut_ubib = str_remove(institut_ubib, "Institut für")) |>
        mutate(institut_ubib = str_trim(institut_ubib)) |>
        mutate(institut_ubib = factor(
          institut_ubib,
          levels = c(
            "Geologische Wissenschaften (WE 1)",
            "Geographische Wissenschaften (WE 2)",
            "Meteorologie (WE 3)"
          )
        )) |>
        tidyr::complete(institut_ubib, year, oa_status, genre, fill = list(n = 0)) |>
        mutate(oa_status = factor(oa_status, levels = oa_status_colors)) |>
        mutate(institut_short = str_extract_all(institut_ubib,  "(?<=\\().+?(?=\\))")) |>
        mutate(institut_short = factor(institut_short, levels = c("WE 1", "WE 2", "WE 3"))) |>
        pivot_wider(names_from = oa_status, values_from = n)
      
    } else {
      
      data <- data_other |>
        mutate(
            oa_status = case_when(
              str_detect(oa_status, "^closed.*") ~ "closed",
              str_detect(oa_status, "^gold.*") ~ "gold",
              is.na(oa_status) ~ "closed",
              TRUE ~ oa_status
            )
          ) |>

        mutate(oa_status = factor(oa_status, levels = oa_status_colors[1:5])) |>
        mutate(genre = case_when(str_detect(genre, "book-chapter|School book chapter") ~ "Book chapter",
                                   str_detect(genre, "[mM]onografie|[bB]uch|book") ~ "Book",
                                   #str_detect(genre, "[jJ]ournal-article") ~ "Journal article",
                                   str_detect(genre, "conference-paper") ~ "Conference paper",
                                   str_detect(genre, "conference-abstract") ~ "Conference abstract",
                                   TRUE ~ "Other")) |>
        count(year, oa_status, institut_ubib, genre) |>
        filter(year >= 2016) |>
        mutate(institut_ubib = str_remove(institut_ubib, "Institut für")) |>
        mutate(institut_ubib = str_trim(institut_ubib)) |>
        mutate(institut_ubib = factor(
          institut_ubib,
          levels = c(
            "Geologische Wissenschaften (WE 1)",
            "Geographische Wissenschaften (WE 2)",
            "Meteorologie (WE 3)"
          )
        )) |>
        tidyr::complete(institut_ubib, year, oa_status, genre, fill = list(n = 0)) |>
        group_by(institut_ubib, year, genre) |>
        mutate(perc = as.numeric(n)/sum(n)) |>
        ungroup() |>
        mutate(oa_status = factor(oa_status, levels = oa_status_colors)) |>
        mutate(institut_short = str_extract_all(institut_ubib,  "(?<=\\().+?(?=\\))")) |>
        mutate(institut_short = factor(institut_short, levels = c("WE 1", "WE 2", "WE 3"))) |>
        pivot_wider(id_cols = -n, names_from = oa_status, values_from = perc)
    }
  }
  



plot_oa_other <- function(data, min, max, total_perc, institut) {
  # print(genre)
  if (total_perc == FALSE) {
  data |>
    filter(year >=min & year <= max) |>
    filter(institut_short %in% institut) |>
    plot_ly(x = ~ list(year, genre, institut_short)) |> 
  add_bars(y = ~ gold,
           marker = list(color = color[1]),
           name = "gold") |>
  add_bars(y = ~ hybrid,
           marker = list(color = color[2]),
           name = "hybrid") |>
  add_bars(y = ~ green,
           marker = list(color = color[3]),
           name = "green") |>
  add_bars(y = ~ bronze,
           marker = list(color = color[4]),
           name = "bronze") |>
  add_bars(y = ~ closed,
           marker = list(color = color[5]),
           name = "closed") |>
  layout(barmode = "relative",
         xaxis = list(title = FALSE),
         yaxis = list(title = FALSE),
         paper_bgcolor = "#DCE3E5",
         plot_bgcolor = "#DCE3E5") |>
  config(displayModeBar = FALSE)
  
  } else {
      
   data |>
      filter(year >=min & year <= max) |>
      filter(institut_short %in% institut) |>
      plot_ly(x = ~ list(year, genre, institut_short)) |> 
      add_bars(y = ~ gold,
               marker = list(color = color[1]),
               name = "gold") |>
      add_bars(y = ~ hybrid,
               marker = list(color = color[2]),
               name = "hybrid") |>
      add_bars(y = ~ green,
               marker = list(color = color[3]),
               name = "green") |>
      add_bars(y = ~ bronze,
               marker = list(color = color[4]),
               name = "bronze") |>
      add_bars(y = ~ closed,
               marker = list(color = color[5]),
               name = "closed") |>
      layout(barmode = "relative",
             xaxis = list(title = FALSE),
             yaxis = list(title = FALSE,
                          tickformat = ".0%",
                          range = c(0, 1)),
             paper_bgcolor = "#DCE3E5",
             plot_bgcolor = "#DCE3E5") |>
      config(displayModeBar = FALSE)
    
    }
  
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Other ressources licenses ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

plot_other_licenses <- function(total_perc) {
  
  other_licenses <- data_other |>
    mutate(license = case_when(!str_detect(license, "^cc.*") ~ "other license",
                               is.na(license) ~ "no license",
                               TRUE ~ license)) |>
    select(-genre, -year) |>
    count(license, oa_status) |>
      mutate(license = factor(
        license,
        levels = c(
          "cc-by",
          "cc-by-sa",
          "cc-by-nc",
          "cc-by-nc-sa",
          "cc-by-nd",
          "cc-by-nc-nd",
          "cc0",
          "no license"
        )
      )) |>
    mutate(oa_status = factor(oa_status, levels = oa_status_colors)) |>
    tidyr::complete(license, oa_status, fill = list(n = 0)) |>
    group_by(license) |>
    mutate(perc = n/sum(n)) |>
    ungroup()
  
  if (total_perc == FALSE) {
    other_licenses |>
      plot_ly(x = ~ license,
              y = ~ n,
              color = ~ oa_status,
              colors = color) |>
      add_bars() |>
      layout(barmode = "stack",
             xaxis = list(title = "Lizenz"),
             yaxis = list(title = FALSE),
             paper_bgcolor = "#DCE3E5",
             plot_bgcolor = "#DCE3E5") |>
      config(displayModeBar = FALSE)
  } else{
    other_licenses |>
      plot_ly(x = ~ license,
              y = ~ perc,
              color = ~ oa_status,
              colors = color) |>
      add_bars() |>
      layout(barmode = "stack",
             xaxis = list(title = "Lizenz"),
             yaxis = list(title = FALSE,
                          tickformat = ".0%"),
             paper_bgcolor = "#DCE3E5",
             plot_bgcolor = "#DCE3E5") |>
      config(displayModeBar = FALSE)
  }
  
}