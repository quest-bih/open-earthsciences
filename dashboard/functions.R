#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Functions for dashboard ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Open access status of journals ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Data preparation
oa_status_colors <- c("gold", "hybrid", "green", "bronze", "closed")#, "no doi")
color <- c("#F4C244", "#A0CBDA", "#4FAC5B", "#D85DBF", "#2C405E") #, "#5F7036")

color_palette <- c("#B6B6B6", "#879C9D", "#F1BA50", "#AA493A",
                   "#303A3E", "#007265", "#634587", "#000000",   #363457 #533A71 #011638 #634587
                   "#DCE3E5")

#show_year <- "2022"
show_year <- as.integer("2022")

color_oa <- brewer.pal(9,"Greens") 
#"#F7FCF5" "#E5F5E0" "#C7E9C0" "#A1D99B" "#74C476" "#41AB5D" "#238B45" "#006D2C" "#00441B"
color_closed <-  brewer.pal(9,"GnBu")
#"#F7FCF0" "#E0F3DB" "#CCEBC5" "#A8DDB5" "#7BCCC4" "#4EB3D3" "#2B8CBE" "#0868AC" "#084081"
color_license <- brewer.pal(9,"RdYlGn")
#"#D73027" "#F46D43" "#FDAE61" "#FEE08B" "#FFFFBF" "#D9EF8B" "#A6D96A" "#66BD63" "#1A9850"


geo_ressources <- read.csv("./data/journal_articles.csv",
                           sep = ",",
                           header = T,
                           quote = "\"")

data_oa_journals <- function(data, min, max, total_perc, institut, color) { 
  
  if (total_perc == FALSE) {
  
  data <- geo_ressources |> # 1363
    #drop_na(institut_short) |> 
    #drop_na(oa_status_manual) |>
    count(year, oa_status_manual, institut_short) |>
    filter(year >= 2016) |>
    tidyr::complete(institut_short, year, oa_status_manual, fill = list(n = 0)) |>
    mutate(oa_status_manual = factor(oa_status_manual, levels = oa_status_colors)) |>
    pivot_wider(names_from = oa_status_manual, values_from = n)
  
  } else {
    
    data <- geo_ressources |>
      #drop_na(institut_short) |>
      #drop_na(oa_status_manual) |>
      count(year, oa_status_manual, institut_short) |>
      filter(year >= 2016) |>
      tidyr::complete(institut_short, year, oa_status_manual, fill = list(n = 0)) |>
      group_by(institut_short, year) |> #group_by(year, oa_status, institut_ubib) |>
      mutate(perc = n/sum(n)) |>
      ungroup() |>
      mutate(oa_status_manual = factor(oa_status_manual, levels = oa_status_colors)) |>
      pivot_wider(id_cols = -n, names_from = oa_status_manual, values_from = perc)
  }
}


plot_oa_journals <- function(data, min, max, total_perc, institut, color_seq) { #min, max
  
 if (total_perc == FALSE) {

    data |>
      filter(year >= min & year <= max) |>
      filter(institut_short %in% institut) |>
      plot_ly(
        x = ~ list(institut_short, year)) |>
      add_bars(y = ~ gold,
               marker = list(color = color_oa[8]),
               # market = list(colorscale = list(c(0, 0.35, 1), color_seq),
               #               cmin = 0,
               #               cmid = 0.35,
               #               cmax = 1,
               #               colors = gold,
               #               showscale = TRUE,
               #               line = list(color = color_palette[9], width = 0.5)),
               name = "gold") |>
      add_bars(y = ~ hybrid,
               marker = list(color = color_oa[7]),
               name = "hybrid") |>
      add_bars(y = ~ green,
               marker = list(color = color_oa[6]),
               name = "green") |>
      add_bars(y = ~ bronze,
               marker = list(color = color_closed[8]),
               name = "bronze") |>
      add_bars(y = ~ closed,
               marker = list(color = color_closed[9]),
               name = "closed") |>
      layout(barmode = "relative",
             legend = list(orientation = "h",
                           y = 1.2),
             xaxis = list(title = FALSE),
             yaxis = list(title = "Number of publications", 
                          range = c(0, 200)),
             paper_bgcolor = "#DCE3E5",
             plot_bgcolor = "#DCE3E5") |>
      config(displayModeBar = FALSE)
  }
  else {
    data |>
      filter(year >= min & year <= max) |> 
      filter(institut_short %in% institut) |>
      plot_ly(
        x = ~ list(institut_short, year)) |>
      add_bars(y = ~ gold,
               marker = list(color = color_oa[8]),
              
               name = "gold") |>
      add_bars(y = ~ hybrid,
               marker = list(color = color_oa[7]),
               name = "hybrid") |>
      add_bars(y = ~ green,
               marker = list(color = color_oa[6]),
               name = "green") |>
      add_bars(y = ~ bronze,
               marker = list(color = color_closed[8]),
               name = "bronze") |>
      add_bars(y = ~ closed,
               marker = list(color = color_closed[9]),
               name = "closed") |>
      layout(barmode = "relative",
             legend = list(orientation = "h",
                           y = 1.2), 
             xaxis = list(title = FALSE),
             yaxis = list(title = "Publications",
                          tickformat = ".0%",
                          range = c(0, 1)),
             paper_bgcolor = "#DCE3E5",
             plot_bgcolor = "#DCE3E5") |>
      config(displayModeBar = FALSE)
  }
  
}




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Licenses of journals v1 (bar chart) ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
#colors <- c("#1ba100", "#9dcf48","#ffff00", "#fdd303", "#f5292a", "#8d040e")


data_journals_licenses <- function(data, total_perc, institut, min, max) {
  if (total_perc == FALSE) {
    
    data <- geo_ressources |> #1363
      #drop_na(institut_short) |>
      filter(oa_status_manual != "closed" & oa_status_manual != "bronze") |> # 766
      filter(license_manual != "no CC license") |> # 635
      count(year, license_manual, institut_short) |> #oa_status,
      filter(year >= 2016) |>
      tidyr::complete(institut_short, year, license_manual, fill = list(n = 0)) |> #oa_status, 
      mutate(license_manual = factor(license_manual, levels = c("cc-by", 
                                                                "cc-by-sa",
                                                                "cc-by-nc",
                                                                "cc-by-nc-sa",
                                                                "cc-by-nd", 
                                                                "cc-by-nc-nd"))) |> #"cc0", "copyright", "no license"
      pivot_wider(names_from = license_manual, values_from = n)
    
  } else {
    data <- geo_ressources |> # 1363
      #drop_na(institut_short) |>
      filter(oa_status_manual != "closed" & oa_status_manual != "bronze") |> # 766
      filter(license_manual != "no CC license") |> # 635
      count(year, license_manual, institut_short) |> #oa_status, 
      filter(year >= 2016) |>
      tidyr::complete(institut_short, year, license_manual, fill = list(n = 0)) |> # oa_status,
      group_by(institut_short, year) |> #, license_manual
      mutate(total_count = sum(n, na.rm = T)) |>
      filter(total_count != 0) |>
      mutate(perc = n / total_count) |>
      ungroup() |>
      mutate(license_manual = factor(license_manual, levels = c("cc-by", 
                                                                "cc-by-sa",
                                                                "cc-by-nc",
                                                                "cc-by-nc-sa", 
                                                                "cc-by-nd", 
                                                                "cc-by-nc-nd"))) |> #"copyright", "no license"
      pivot_wider(id_cols = -n, names_from = license_manual, values_from = perc)
    
  }
}



plot_journals_licenses <- function(data, min, max, total_perc, institut) { 
  if (total_perc == FALSE) {
    data |>
    filter(year >=min & year <= max) |>
    filter(institut_short %in% institut) |>
      plot_ly(
        x = ~ list(institut_short, year)) |>
      add_bars(y = ~ `cc-by`,
               marker = list(color = color_license[9]),
               name = "cc-by") |>
      add_bars(y = ~ `cc-by-sa`,
               marker = list(color = color_license[8]),
               name = "cc-by-sa") |>
      add_bars(y = ~ `cc-by-nd`,
               marker = list(color = color_license[7]),
               name = "cc-by-nd") |>
      add_bars(y = ~ `cc-by-nc`,
               marker = list(color = color_license[4]),
               name = "cc-by-nc") |>
      add_bars(y = ~ `cc-by-nc-sa`,
               marker = list(color = color_license[3]),
               name = "cc-by-nc-sa") |>
      add_bars(y = ~ `cc-by-nc-nd`,
               marker = list(color = color_license[2]),
               name = "cc-by-nc-nd") |>
      layout(barmode = "relative",
             legend = list(orientation = "h",
                           y = 1.2), 
             xaxis = list(title = "License"),
             yaxis = list(title = "Number of publications (OA)"),
             paper_bgcolor = "#DCE3E5",
             plot_bgcolor = "#DCE3E5") |>
      config(displayModeBar = FALSE)

  } else {
    data |>
      filter(year >=min & year <= max) |>
      filter(institut_short %in% institut) |>
      plot_ly(
        x = ~ list(institut_short, year)) |>
      add_bars(y = ~ `cc-by`,
               marker = list(color = color_license[9]),
               name = "cc-by") |>
      add_bars(y = ~ `cc-by-sa`,
               marker = list(color = color_license[8]),
               name = "cc-by-sa") |>
      add_bars(y = ~ `cc-by-nd`,
               marker = list(color = color_license[7]),
               name = "cc-by-nd") |>
      add_bars(y = ~ `cc-by-nc`,
               marker = list(color = color_license[4]),
               name = "cc-by-nc") |>
      add_bars(y = ~ `cc-by-nc-sa`,
               marker = list(color = color_license[3]),
               name = "cc-by-nc-sa") |>
      add_bars(y = ~ `cc-by-nc-nd`,
               marker = list(color = color_license[2]),
               name = "cc-by-nc-nd") |>
      layout(barmode = "relative",
             legend = list(orientation = "h",
                           y = 1.2), 
             xaxis = list(title = "License"),
             yaxis = list(title = "Number of publications (OA)",
                          tickformat = ".0%",
                          range = c(0, 1)),
             paper_bgcolor = "#DCE3E5",
             plot_bgcolor = "#DCE3E5") |>
      config(displayModeBar = FALSE)
  }
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Non-journal-article outputs----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Non-journal-article outputs OA status ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

#data_other <- read_excel("data/data_other.xlsx") 

data_other <- read.csv("./data/non_journal_article_outputs.csv",
                           sep = ",",
                           header = T,
                           quote = "\"")
  
data_oa_other <- function(data, min, max, total_perc, institut) {
  
  if (total_perc == FALSE) {
    data <- data_other |> # 824
        count(year, oa_status_manual, institut_short, genre_manual) |>
        filter(year >= 2016) |>
        tidyr::complete(institut_short, year, oa_status_manual, genre_manual, fill = list(n = 0)) |>
        mutate(oa_status_manual = factor(oa_status_manual, levels = oa_status_colors)) |>
        pivot_wider(names_from = oa_status_manual, values_from = n)
    
    total_data <- data_other |>
      count(year, oa_status_manual, genre_manual) |> # institut_short, 
      filter(year >= 2016) |>
      tidyr::complete(year, oa_status_manual, genre_manual, fill = list(n = 0)) |> #institut_short, 
      mutate(oa_status_manual = factor(oa_status_manual, levels = oa_status_colors)) |>
      pivot_wider(names_from = oa_status_manual, values_from = n)
      
    } else {
      
      data <- data_other |>
        count(year, oa_status_manual, institut_short, genre_manual) |>
        filter(year >= 2016) |>
        tidyr::complete(institut_short, year, oa_status_manual, genre_manual, fill = list(n = 0)) |>
        group_by(year, genre_manual) |> #oa_status, institut_ubib, 
        mutate(total_count = sum(n)) |>
        filter(total_count != 0) |>
        mutate(perc = n / total_count) |>
        ungroup() |>
        mutate(oa_status_manual = factor(oa_status_manual, levels = oa_status_colors)) |>
        pivot_wider(id_cols = -n, names_from = oa_status_manual, values_from = perc)
      
      total_data_perc <- data_other |>
        count(year, oa_status_manual, genre_manual) |> # institut_short, 
        filter(year >= 2016) |>
        tidyr::complete(year, oa_status_manual, genre_manual, fill = list(n = 0)) |> #institut_short, 
        group_by(year, genre_manual) |> #oa_status, institut_ubib, 
        mutate(total_count = sum(n)) |>
        filter(total_count != 0) |>
        mutate(perc = n / total_count) |>
        ungroup() |>
        mutate(oa_status_manual = factor(oa_status_manual, levels = oa_status_colors)) |>
        pivot_wider(id_cols = -n, names_from = oa_status_manual, values_from = perc)
 
    }
  }
  

plot_oa_other <- function(data, min, max, total_perc, institut) {
  if (total_perc == FALSE) {
    data |>
      filter(year >=min & year <= max) |>
      #filter(institut_short %in% institut) |>
        plot_ly(
          x = ~ list(genre_manual, year)) |> #, institut_short
      add_bars(y = ~ gold,
               marker = list(color = color_oa[4]),
               name = "gold") |>
      add_bars(y = ~ hybrid,
               marker = list(color = color_oa[5]),
               name = "hybrid") |>
      add_bars(y = ~ green,
               marker = list(color = color_oa[6]),
               name = "green") |>
      add_bars(y = ~ bronze,
               marker = list(color = color_closed[8]),
               name = "bronze") |>
      add_bars(y = ~ closed,
               marker = list(color = color_closed[9]),
               name = "closed") |>
        layout(barmode = "relative",
               legend = list(orientation = "h",
                            y = 1.2),
               xaxis = list(title = FALSE),
               yaxis = list(title = "Number of non-journal-article outputs",
                            range = c(0, 70)),
               paper_bgcolor = "#DCE3E5",
               plot_bgcolor = "#DCE3E5") |>
        config(displayModeBar = FALSE)

  } else {

   data |>
      filter(year >=min & year <= max) |>
      #filter(institut_short %in% institut) |>
      plot_ly(x = ~ list(genre_manual, year)) |> #, institut_short
      add_bars(y = ~ gold,
               marker = list(color = color_oa[4]),
               name = "gold") |>
      add_bars(y = ~ hybrid,
               marker = list(color = color_oa[5]),
               name = "hybrid") |>
      add_bars(y = ~ green,
               marker = list(color = color_oa[6]),
               name = "green") |>
      add_bars(y = ~ bronze,
               marker = list(color = color_closed[8]),
               name = "bronze") |>
      add_bars(y = ~ closed,
               marker = list(color = color_closed[9]),
               name = "closed") |>
      layout(barmode = "relative",
             legend = list(orientation = "h",
                           y = 1.2),
             xaxis = list(title = FALSE),
             yaxis = list(title = "Non-journal-article outputs",
                          tickformat = ".0%",
                          range = c(0, 1)),
             paper_bgcolor = "#DCE3E5",
             plot_bgcolor = "#DCE3E5") |>
      config(displayModeBar = FALSE)

    }
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Non-journal-article persistent identifiers ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_other_pid <- function(data, min, max, total_perc, institut) { #has_pid_choice
  if (total_perc == FALSE) {
    data <- data_other |>
      drop_na(institut_short) |>
      mutate(pid_binary = is.na(pid)) |>
      filter(year >=2016) |>
      #pivot_longer(cols = everything()) |>
      count(year, pid_binary, institut_short) |>
      tidyr::complete(institut_short, year, pid_binary, fill = list(n = 0)) |>
      mutate(pid_binary = factor(pid_binary, levels = c("TRUE", "FALSE"))) |>
      pivot_wider(names_from = pid_binary, values_from = n)

  } else {
    data <- data_other |>
      drop_na(institut_short) |>
      mutate(pid_binary = is.na(pid)) |>
      filter(year >=2016) |>
      count(year, pid_binary, institut_short) |>
      tidyr::complete(institut_short, year, pid_binary, fill = list(n = 0)) |>
      group_by(institut_short, year) |>
      mutate(nn = sum(n)) |>
      ungroup() |>
      mutate(perc = n / nn) |>
      mutate(pid_binary = factor(pid_binary, levels = c("TRUE", "FALSE"))) |>
      pivot_wider(id_cols = -n, names_from = pid_binary, values_from = perc)

  }
}
    
plot_other_pid <- function(data, min, max, total_perc, institut) {
  if (total_perc == FALSE) {
    data |>
      filter(year >=min & year <= max) |>
      filter(institut_short %in% institut) |>
      plot_ly(x = ~ list(institut_short, year)) |> 
      add_bars(y = ~ `TRUE`,
               marker = list(color = color[1]),
               name = "No PID") |>
      add_bars(y = ~ `FALSE`,
               marker = list(color = color[2]),
               name = "Has PID") |>
      layout(barmode = "relative",
             legend = list(orientation = "h",
                           y = 1.2),
             xaxis = list(title = "Has PID"),
             yaxis = list(title = "Number of non-journal-article outputs"), #, range = c(0, 70)),
             legend = list(x = 0.55, y = 0.95),
             paper_bgcolor = "#DCE3E5",
             plot_bgcolor = "#DCE3E5") |>
      config(displayModeBar = FALSE)
  } else {

    data |>
      filter(year >=min & year <= max) |> 
      filter(institut_short %in% institut) |>
      plot_ly(x = ~ list(institut_short, year)) |>
      add_bars(y = ~ `TRUE`,
               marker = list(color = color[1]),
               name = "Has PID") |>
      add_bars(y = ~ `FALSE`,
               marker = list(color = color[2]),
               name = "No PID") |>
      # layout(barmode = "relative",
      #        xaxis = list(title = "Has PID"),
      #        yaxis = list(title = FALSE),
      #        paper_bgcolor = "#DCE3E5",
      #        plot_bgcolor = "#DCE3E5") |>
      config(displayModeBar = FALSE) |>
      layout(barmode = "relative",
             legend = list(orientation = "h",
                           y = 1.2),
             xaxis = list(title = "Has PID"),
             yaxis = list(title = "Non-journal-article outputs",
                          tickformat = ".0%",
                          range = c(0, 1)),
             paper_bgcolor = "#DCE3E5",
             plot_bgcolor = "#DCE3E5") |>
      config(displayModeBar = FALSE)
  }
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Open Data journal articles v1 (diverging chart) ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
combined_journal_oddpub <- read.csv("./data/combined_journal_oddpub.csv",
                                    header = TRUE, 
                                    sep = ";", 
                                    na.strings = "NA", 
                                    quote = "\"",
                                    fill=TRUE,
                                    comment.char = "")

data_od <- function(total_perc) { #min, max,
  if (total_perc == FALSE) {
    combined_journal_oddpub$is_open_data <- ifelse(combined_journal_oddpub$is_open_data == "TRUE", 1, -1)
    
    data <- combined_journal_oddpub |> 
      group_by(institut_ubib, is_open_data) |>
      count(name = "n_answers") |>
      group_by(institut_ubib) |>
      #mutate(counts_OD = sum(n_answers)) |>
      ungroup() |>
      mutate(n_answers = if_else(is_open_data %in% "1", n_answers, -n_answers)) |>
      mutate(n_answers_label = abs(n_answers)) #|>
      #tidyr::complete(institut_ubib, is_open_data, fill = list(n = 0)) |>
      #pivot_wider(names_from = institut_ubib, values_from = n_answers)
    
    
  } else {
    combined_journal_oddpub$is_open_data <- ifelse(combined_journal_oddpub$is_open_data == "TRUE", 1, -1)
    
    data <- combined_journal_oddpub |> 
      tidyr::complete(institut_ubib, is_open_data, fill = list(n = 0)) |>
      group_by(institut_ubib, is_open_data) |>
      count(name = "n_answers") |>
      group_by(institut_ubib) |>
      mutate(counts_OD = sum(n_answers)) |>
      mutate(perc = n_answers/ counts_OD ) |>
      ungroup() |>
      mutate(n_answers = if_else(is_open_data %in% "1", n_answers, -n_answers)) |>
      mutate(n_answers_label = abs(n_answers)) 
    
    
    # drop_na(institut_short) |>
    #   count(year, license, institut_short) |>
    #   filter(year >=2016) |>
    #   tidyr::complete(institut_short, year, license, fill = list(n = 0)) |>
    #   group_by(year, license) |>  
    #   mutate(total_count = sum(n, na.rm = T)) |>
    #   filter(total_count != 0) |>
    #   mutate(perc = n / total_count) |>
    #   ungroup() |>
    #   pivot_wider(id_cols = -n, names_from = institut_short, values_from = perc)
  }
}

plot_od <- function(data, total_perc) { #, min, max,
  
  if (total_perc == FALSE) {
    data  |>
      ggplot(
        aes(x = institut_ubib,
            y = n_answers,
            fill = is_open_data)) +
      geom_col() +
      geom_text(aes(label = n_answers_label),
                position = position_stack(vjust = 0.5),
                color = "black",
                fontface = "bold") +
      coord_flip() +
      #scale_x_discrete() +
      #scale_fill_viridis_d() +
      labs(title = "Open Data detection",
           x = NULL,
           fill = NULL) +
      #theme_minimal() +
      # theme_set(theme_bw())  +
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            panel.grid = element_blank(),
            legend.position = "top")
    
    } else {
    
    data |>
      ggplot(
        aes(x = institut_ubib,
            y = n_answers,
            fill = is_open_data)) +
      geom_bar(stat = "identity", 
               position = "fill") +
      geom_text(aes(label = scales::percent(perc),
                    y = perc + 0.02), 
                position = position_fill(vjust = 0.5),
                color = "black",
                fontface = "bold") +
      coord_flip() +
      #scale_x_discrete() +
      #scale_fill_viridis_d() +
      labs(title = "Open Data detection in %",
           x = NULL,
           fill = NULL) +
      #theme_minimal() +
      # theme_set(theme_bw())  +
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            panel.grid = element_blank(),
            legend.position = "top")
    }
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Open Data journal articles v1 (line chart) ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
combined_journal_oddpub <- read.csv("./data/combined_journal_oddpub.csv",
                                    header = TRUE, 
                                    sep = ";", 
                                    na.strings = "NA", 
                                    quote = "\"",
                                    fill=TRUE,
                                    comment.char = "")

data_open_data_journals_line <- function() { #total_perc
  #if (total_perc == FALSE) {
    data <- combined_journal_oddpub |>
      drop_na(institut_ubib) |>
      mutate(
        institut_ubib = case_when(
          institut_ubib == "Institut für Weltraumwissenschaften" ~ "Institut für Meteorologie (WE 3)",
          TRUE ~ institut_ubib
        )
      ) |>
      mutate(year = as.numeric(year)) |>
      count(is_open_data, year, institut_ubib) |> 
      filter(year >=2016) |>
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
      tidyr::complete(institut_ubib, year, is_open_data, fill = list(n = 0)) |> 
      mutate(institut_short = str_extract_all(institut_ubib,  "(?<=\\().+?(?=\\))")) |>
      mutate(institut_short = factor(institut_short, levels = c("WE 1", "WE 2", "WE 3"))) |>
      mutate(is_open_data = factor(is_open_data, levels = c("FALSE", "TRUE"))) |>
      pivot_wider(names_from = is_open_data, values_from = n) |>
      filter(`FALSE` > 0 | `TRUE` > 0)
}
#   } else {
#     #data <- od_journal |>
#     data <- combined_journal_oddpub |>
#       drop_na(institut_ubib) |>
#       mutate(
#         institut_ubib = case_when(
#           institut_ubib == "Institut für Weltraumwissenschaften" ~ "Institut für Meteorologie (WE 3)",
#           TRUE ~ institut_ubib
#         )
#       ) |>
#       mutate(year = as.numeric(year)) |>
#       count(is_open_data, year, institut_ubib) |> 
#       filter(year >=2016) |>
#       mutate(institut_ubib = str_remove(institut_ubib, "Institut für")) |>
#       mutate(institut_ubib = str_trim(institut_ubib)) |>
#       mutate(institut_ubib = factor(
#         institut_ubib,
#         levels = c(
#           "Geologische Wissenschaften (WE 1)",
#           "Geographische Wissenschaften (WE 2)",
#           "Meteorologie (WE 3)"
#         )
#       )) |>
#       tidyr::complete(institut_ubib, year, is_open_data, fill = list(n = 0)) |> 
#       group_by(institut_ubib, is_open_data) |> 
#       mutate(perc = n/sum(n)) |>
#       ungroup() |>
#       mutate(institut_short = str_extract_all(institut_ubib,  "(?<=\\().+?(?=\\))")) |>
#       mutate(institut_short = factor(institut_short, levels = c("WE 1", "WE 2", "WE 3"))) |>
#       mutate(is_open_data = factor(is_open_data, levels = c("FALSE", "TRUE"))) |>
#       pivot_wider(id_cols = -n, names_from = is_open_data, values_from = perc)
#   }   
#   
# }


# plot_open_data_journal_line <- function(data, min, max,  institut) { #total_perc,
#   color <- c("#4FAC5B", "#377EB8")
#   
# #  if (total_perc == FALSE) {
#     filtered_data <- data |>
#       filter(year >= min & year <= max)
# 
#     
#     if (!is.null(institut)) {
#       filtered_data <- filtered_data |>
#         filter(institut_short %in% institut)
#     }
#     
#     
#     summed_data <- filtered_data |>
#       group_by(year) |>
#       summarise(`TRUE` = sum(`TRUE`),
#                 `FALSE` = sum(`FALSE`), 
#                 .groups = "drop")
#     
#     summed_data |>
#       plot_ly(x = ~ year, mode = 'lines') |>
#       add_trace(
#         #y = ~total_open_data,
#         y = ~`TRUE`,
#         name = "Open Data",
#         line = list(shape = "spline",
#                     width = 8,
#                     color = color[1]),
#         type = 'scatter', 
#         mode = 'markers', 
#         marker = list(color = color[1], size = 12),
#         legendgroup = "Legend",
#         showlegend = TRUE) |>
#       add_trace(
#         y = ~`FALSE`,
#         name = "No data or no Open Data",
#         line = list(shape = "spline",
#                     width = 4,
#                     color = color[2]),
#         type = 'scatter', 
#         mode = 'markers', 
#         marker = list(color = color[2], size = 10),
#         legendgroup = "Legend",
#         showlegend = TRUE) |>
#       layout(
#         barmode = "relative",
#         xaxis = list(title = "Year"),
#         yaxis = list(title = "Count of data sharing cases"),
#         paper_bgcolor = "#DCE3E5",
#         plot_bgcolor = "#DCE3E5",
#         legend = list(
#           x = 1,
#           y = 1,
#           bgcolor = "#F2F2F2",
#           bordercolor = "#CCCCCC",
#           borderwidth = 1,
#           tracegroupgap = 10,
#           orientation = "v"
#         ) 
#       ) |>
#       config(displayModeBar = FALSE)
# }
 # } else {
 #    filtered_data <- data |>
 #      filter(year >= min & year <= max)
 #    
 #    if (!is.null(institut)) {
 #      filtered_data <- filtered_data |>
 #        filter(institut_short %in% institut)
 #    }
 #    
 #    summed_data <- filtered_data |>
 #      group_by(year) |>
 #      summarise(`TRUE` = sum(`TRUE`),
 #                `FALSE` = sum(`FALSE`))
 #    
 #    summed_data |>
 #      plot_ly(x = ~year, mode = 'lines') |> # type = 'scatter',
 #      layout(
 #        xaxis = list(title = "Year"),
 #        yaxis = list(title = "Count of data"),
 #        paper_bgcolor = "#DCE3E5",
 #        plot_bgcolor = "#DCE3E5"
 #      ) |>
 #      config(displayModeBar = FALSE)
 #  }
#}
     
#     ### Separate plots for open and closed data
#     open_data_trace <- filtered_data |>
#       group_by(year) |>
#       summarise(total_open_data = sum(`TRUE`)) |>
#       plot_ly(x = ~year, y = ~total_open_data, mode = 'lines') |> #type = 'scatter', 
#       add_trace(
#         name = "Open Data",
#         line = list(shape = "spline", 
#                     width = 8, 
#                     color = color[1]),
#         legendgroup = "data",
#         showlegend = TRUE
#       )
#     
#     closed_data_trace <- filtered_data |>
#       group_by(year) |>
#       summarise(total_closed_data = sum(`FALSE`)) |>
#       plot_ly(x = ~year, y = ~total_closed_data, mode = 'lines') |> #type = 'scatter', 
#       add_trace(
#         name = "Closed Data",
#         line = list(shape = "spline", 
#                     width = 4, 
#                     color = color[2]),
#         legendgroup = "data",
#         showlegend = TRUE
#       )
#     
#     subplot(open_data_trace, closed_data_trace) |>
#       layout(
#         barmode = "relative",
#         xaxis = list(title = "Year"),
#         yaxis = list(title = "Count of data"),
#         paper_bgcolor = "#DCE3E5",
#         plot_bgcolor = "#DCE3E5",
#         legend = list(
#           x = 1,
#           y = 1,
#           bgcolor = "#F2F2F2",
#           bordercolor = "#CCCCCC",
#           borderwidth = 1,
#           tracegroupgap = 10,
#           orientation = "v"
#         )
#       ) |>
#       config(displayModeBar = FALSE)
#   } else {
#     filtered_data <- data |>
#       filter(year >= min & year <= max)
#     
#     if (!is.null(institut)) {
#       filtered_data <- filtered_data |>
#         filter(institut_short %in% institut)
#     }
#     
#     filtered_data |>
#       plot_ly(x = ~year, mode = 'lines') |> #type = 'scatter', 
#       layout(
#         xaxis = list(title = "Year"),
#         yaxis = list(title = "Count of data"),
#         paper_bgcolor = "#DCE3E5",
#         plot_bgcolor = "#DCE3E5"
#       ) |>
#       config(displayModeBar = FALSE)
#   }
# }



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Open Data journal articles v2 (bar chart) ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++


data_open_data_journals_bar <- function(institut) { #total_perc
  #if (total_perc == FALSE) {
  data <- combined_journal_oddpub |>
    drop_na(is_open_data) |>
    drop_na(institut_ubib) |>
    mutate(
      institut_ubib = case_when(
        institut_ubib == "Institut für Weltraumwissenschaften" ~ "Institut für Meteorologie (WE 3)",
        TRUE ~ institut_ubib
      )
    ) |>
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
    mutate(institut_short = str_extract_all(institut_ubib,  "(?<=\\().+?(?=\\))")) |>
    mutate(institut_short = factor(institut_short, levels = c("WE 1", "WE 2", "WE 3"))) |>
    mutate(year = as.numeric(year)) |>
    # mutate(open_data_category_priority = (open_data_category |> (function(x)
    #   x |> str_split(",") |> map_chr(head, 1)))) |>
    #filter(institut_ubib %in% institut) |>
    group_by(year, institut_short) |> #, , is_open_data, .groups = "drop"
    summarize(is_open_data_count = sum(is_open_data == "TRUE", na.rm = TRUE),
              open_data_disc = sum(open_data_category_priority == "field-specific repository",
                                   na.rm = TRUE),
              open_data_gener = sum(open_data_category_priority == "general-purpose repository",
                                    na.rm = TRUE),
              open_data_supplement = sum(open_data_category_priority == "supplement", na.rm = TRUE),
              total = sum(!is.na(is_open_data))) |>
    #group_by(year, .groups = "drop") |>
    mutate(is_open_data_perc = round(is_open_data_count/total * 100, 1),
              open_data_disc_perc = round(open_data_disc/total * 100, 1),
              open_data_gener_perc = round(open_data_gener/total * 100, 1),
              open_data_supplement_perc = round(open_data_supplement/total * 100, 1))|>
              
  

    #count(year, institut_ubib, publisher) |> #is_open_data, 
    #filter(is_open_data == TRUE) |>
    filter(year >=2016) #|>
    #tidyr::complete(institut_ubib, year, publisher, fill = list(n = 0)) |> #, is_open_data
    #mutate(institut_short = str_extract_all(institut_ubib,  "(?<=\\().+?(?=\\))")) |>
    #mutate(institut_short = factor(institut_short, levels = c("WE 1", "WE 2", "WE 3"))) #|>
    #mutate(is_open_data = factor(is_open_data, levels = c("FALSE", "TRUE"))) |>
    #pivot_wider(names_from = institut, values_from = n) 
    #filter(`FALSE` > 0 | `TRUE` > 0)
    
    #print(data)
    
    #return(data)
}
#   } else {
#     #data <- od_journal |>
#     data <- combined_journal_oddpub |>
#       drop_na(institut_ubib) |>
#       mutate(
#         institut_ubib = case_when(
#           institut_ubib == "Institut für Weltraumwissenschaften" ~ "Institut für Meteorologie (WE 3)",
#           TRUE ~ institut_ubib
#         )
#       ) |>
#       mutate(year = as.numeric(year)) |>
#       count(is_open_data, year, institut_ubib) |> 
#       filter(year >=2016) |>
#       mutate(institut_ubib = str_remove(institut_ubib, "Institut für")) |>
#       mutate(institut_ubib = str_trim(institut_ubib)) |>
#       mutate(institut_ubib = factor(
#         institut_ubib,
#         levels = c(
#           "Geologische Wissenschaften (WE 1)",
#           "Geographische Wissenschaften (WE 2)",
#           "Meteorologie (WE 3)"
#         )
#       )) |>
#       tidyr::complete(institut_ubib, year, is_open_data, fill = list(n = 0)) |> 
#       group_by(institut_ubib, is_open_data) |> 
#       mutate(perc = n/sum(n)) |>
#       ungroup() |>
#       mutate(institut_short = str_extract_all(institut_ubib,  "(?<=\\().+?(?=\\))")) |>
#       mutate(institut_short = factor(institut_short, levels = c("WE 1", "WE 2", "WE 3"))) |>
#       mutate(is_open_data = factor(is_open_data, levels = c("FALSE", "TRUE"))) |>
#       pivot_wider(id_cols = -n, names_from = is_open_data, values_from = perc)
#   }   
#   
# }


# Open Data
plot_open_data_journal_bar <- function(data, color, institut) #, zoom_in, show_supplements)
{
  # print(data)
  # 
  # if(zoom_in) {
  #   yrange <- c(0, 20)
  # } else {
  #   yrange <- c(0, 50)
  # }
  
 # plot <- 
    data |> 
    filter(institut_short %in% institut) |>
    plot_ly(x = ~year, y = ~open_data_disc_perc,
                         name = "disciplinary repository <br>or website", type = 'bar',
                         marker = list(color = color[1],
                                       #line = list(color = color[1], width = 1.0))) |>
                                       line = list(color = 'rgb(0,5,5)', width = 1.0))) |>
    add_trace(y = ~open_data_gener_perc,
              name = 'general-purpose repository <br>or website',
              marker = list(color = color[2])) |> #,
                            #line = list(color = 'rgb(0,0,0)',
                            #            width = 1.0))) |>
    add_trace(y = ~open_data_supplement_perc,
              name = 'supplement in the article',
              marker = list(color = color[3]),
                            #line = list(color = 'rgb(0,0,0)',
                              #          width = 1.0)),
              visible = "legendonly") |>
    layout(barmode = 'stack',
           legend = list(xanchor = "left"),
           yaxis = list(title = '<b>Publications</b>',
                        #range = yrange,
                        ticksuffix = "%"),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = "#DCE3E5",
           plot_bgcolor = "#DCE3E5") |>
    config(displayModeBar = FALSE)
}

# plot_open_data_journal_horiz <- function(data, institut) { #total_perc,  min, max,
#   
#   data |>
#     #filter(year >= min & year <= max) |>
#     filter(institut_short %in% institut) |>
#     plot_ly(x = ~open_data_gener, 
#             y = ~ year,
#             mode = 'bar',
#             orientation = 'h',
#             marker = list(color = color[1],
#                           line = list(color = 'rgb(0,5,5)', width = 1.0))) |>
#     add_trace(y = ~open_data_disc,
#               name = 'general-purpose repository <br>or website',
#               marker = list(color = color[2])) |> 
# 
#     add_trace(y = ~open_data_supplement,
#               name = 'supplement in the article',
#               marker = list(color = color[3]),
#               #line = list(color = 'rgb(0,0,0)',
#               #          width = 1.0)),
#               visible = "legendonly") |>
#     layout(barmode = 'relative',
#            legend = list(xanchor = "left"),
#            yaxis = list(title = '<b>Year</b>'),
#            xaxis = list(title = '<b>Number of publications</b>'), #, dtick = 1),
#            paper_bgcolor = "#DCE3E5",
#            plot_bgcolor = "#DCE3E5") |>
#     config(displayModeBar = FALSE)
# }



data_open_data_journals_horiz <- function(institut, color) {
  data <- combined_journal_oddpub |>
    drop_na(institut_ubib) |>
    mutate(
      institut_ubib = case_when(
        institut_ubib == "Institut für Weltraumwissenschaften" ~ "Institut für Meteorologie (WE 3)",
        TRUE ~ institut_ubib
      )
    ) |>
    mutate(year = as.numeric(year)) |>
    mutate(is_open_data = factor(is_open_data, levels = c("TRUE","FALSE"))) |>
    mutate(is_open_data = ifelse(is_open_data == "TRUE", 1, -1)) |>
    count(is_open_data, institut_ubib, year) |> #
    filter(year >=2016) |>
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
    tidyr::complete(year,institut_ubib, is_open_data, fill = list(n = 0)) |>  
    mutate(institut_short = str_extract_all(institut_ubib,  "(?<=\\().+?(?=\\))")) |>
    mutate(institut_short = factor(institut_short, levels = c("WE 1", "WE 2", "WE 3"))) |>
    
    pivot_wider(names_from = is_open_data, values_from = n) |>
    #filter(`FALSE` > 0 | `TRUE` > 0) |>
    mutate(year = as.character(year)) # Convert year to character to avoid issues in the plot_ly function
}
plot_open_data_journal_horiz <- function(data, institut) {
  
  # separator_data <- data |>
  #   filter(year == max(year)) |>
  #   mutate(`-1` = 0, `1` = 0)
  # 
  # combined_data <- bind_rows(data, separator_data)
  data |>
    plot_ly(x = ~`-1`, y = ~year, type = 'funnel', #orientation = 'h',
            name = "Open Data FALSE",
            #textinfo = "percent initial+text",
            marker = list(color = color[1])) |> 
    add_trace(x = ~`1`, name = "Open Data TRUE",
              #orientation = 'h',
              #textinfo = "percent initial+text",
              marker = list(color = color[2])) |>
    layout(#barmode = 'overlay',
           #legend = list(xanchor = "left"),
           xaxis = list(title = '<b>Open Data Counts</b>',
                        range = c(-1, 1)),
           yaxis = list(title = '<b>Year</b>', dtick = 1, autorange = "reversed"),
           paper_bgcolor = "#DCE3E5",
           plot_bgcolor = "#DCE3E5") |>
    config(displayModeBar = FALSE)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Open Data journal articles v3 (publisher) ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++


data_open_data_journals_pub <- function(institut) { #total_perc
  #if (total_perc == FALSE) {
  
  #journals <- data |> distinct(publisher)
  
  data <- combined_journal_oddpub |>
    drop_na(institut_ubib) |>
    mutate(
      institut_ubib = case_when(
        institut_ubib == "Institut für Weltraumwissenschaften" ~ "Institut für Meteorologie (WE 3)",
        TRUE ~ institut_ubib
      )
    ) |>
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
    mutate(institut_short = str_extract_all(institut_ubib,  "(?<=\\().+?(?=\\))")) |>
    mutate(institut_short = factor(institut_short, levels = c("WE 1", "WE 2", "WE 3"))) |>
    mutate(year = as.numeric(year)) |>
    group_by(year, institut_short, publisher) |>
    summarize(count_open_data = sum(is_open_data == "TRUE", na.rm = TRUE)) |>

    # summarize(publisher_count = sum(is_open_data == "TRUE", na.rm = TRUE),
    #           open_data_disc = sum(open_data_category_priority == "field-specific repository",
    #                                na.rm = TRUE),
    #           open_data_gener = sum(open_data_category_priority == "general-purpose repository",
    #                                 na.rm = TRUE),
    #           open_data_supplement = sum(open_data_category_priority == "supplement", na.rm = TRUE),
    #           total = sum(!is.na(is_open_data), na.rm = TRUE)) |>
    # 
    # mutate(is_open_data_perc = round(is_open_data_count/total * 100, 1),
    #        open_data_disc_perc = round(open_data_disc/total * 100, 1),
    #        open_data_gener_perc = round(open_data_gener/total * 100, 1),
    #        open_data_supplement_perc = round(open_data_supplement/total * 100, 1))|>
    
    
    
    #count(year, institut_ubib, publisher) |> #is_open_data, 
    #filter(is_open_data == TRUE) |>
    filter(year >=2016) |>
    filter(count_open_data >= 3)
  
  
  return(data)
}

plot_open_data_journal_pub <- function(data, color, institut) #min, max,  institut) { #total_perc,
{

  data |>
    filter(institut_short %in% institut) |>
  plot_ly(x = ~year, y = ~count_open_data, type = 'bar',
          color = ~publisher,
          #name = "disciplinary repository <br>or website", type = 'bar',
          marker = list(line = list(color = 'rgb(0,5,5)',
                                    width = 1.0))) |>

    layout(barmode = "relative",
           xaxis = list(title = "Publisher"),
           yaxis = list(title = FALSE), #,range = c(0, 33)
           paper_bgcolor = "#DCE3E5",
           plot_bgcolor = "#DCE3E5") |>
    config(displayModeBar = FALSE)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Open Code journal articles v1 (bar chart) ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++


data_open_code_journals_bar <- function(institut) { #total_perc
  #if (total_perc == FALSE) {
  data_2 <- combined_journal_oddpub |>
    drop_na(institut_ubib) |>
    drop_na(is_open_code) |>
    mutate(
      institut_ubib = case_when(
        institut_ubib == "Institut für Weltraumwissenschaften" ~ "Institut für Meteorologie (WE 3)",
        TRUE ~ institut_ubib
      )
    ) |>
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
    mutate(institut_short = str_extract_all(institut_ubib,  "(?<=\\().+?(?=\\))")) |>
    mutate(institut_short = factor(institut_short, levels = c("WE 1", "WE 2", "WE 3"))) |>
    mutate(year = as.numeric(year)) |>
    
    group_by(year, institut_short) |>
    summarize(
      is_open_code_count = sum(is_open_code == "TRUE", na.rm = TRUE),
      total_count = sum(!is.na(is_open_code))
    ) |>
    mutate(is_open_code_perc = round(is_open_code_count / total_count * 100, 1)) |>
  
    
    #count(year, institut_ubib, publisher) |> #is_open_data, 
    #filter(is_open_data == TRUE) |>
    filter(year >=2016) #|>
  #tidyr::complete(institut_ubib, year, publisher, fill = list(n = 0)) |> #, is_open_data
  #mutate(institut_short = str_extract_all(institut_ubib,  "(?<=\\().+?(?=\\))")) |>
  #mutate(institut_short = factor(institut_short, levels = c("WE 1", "WE 2", "WE 3"))) #|>
  #mutate(is_open_data = factor(is_open_data, levels = c("FALSE", "TRUE"))) |>
  #pivot_wider(names_from = publisher, values_from = n) 
  #filter(`FALSE` > 0 | `TRUE` > 0)
  
  #print(data_2)
  return(data_2)
}
#   } else {
#     #data <- od_journal |>
#     data <- combined_journal_oddpub |>
#       drop_na(institut_ubib) |>
#       mutate(
#         institut_ubib = case_when(
#           institut_ubib == "Institut für Weltraumwissenschaften" ~ "Institut für Meteorologie (WE 3)",
#           TRUE ~ institut_ubib
#         )
#       ) |>
#       mutate(year = as.numeric(year)) |>
#       count(is_open_data, year, institut_ubib) |> 
#       filter(year >=2016) |>
#       mutate(institut_ubib = str_remove(institut_ubib, "Institut für")) |>
#       mutate(institut_ubib = str_trim(institut_ubib)) |>
#       mutate(institut_ubib = factor(
#         institut_ubib,
#         levels = c(
#           "Geologische Wissenschaften (WE 1)",
#           "Geographische Wissenschaften (WE 2)",
#           "Meteorologie (WE 3)"
#         )
#       )) |>
#       tidyr::complete(institut_ubib, year, is_open_data, fill = list(n = 0)) |> 
#       group_by(institut_ubib, is_open_data) |> 
#       mutate(perc = n/sum(n)) |>
#       ungroup() |>
#       mutate(institut_short = str_extract_all(institut_ubib,  "(?<=\\().+?(?=\\))")) |>
#       mutate(institut_short = factor(institut_short, levels = c("WE 1", "WE 2", "WE 3"))) |>
#       mutate(is_open_data = factor(is_open_data, levels = c("FALSE", "TRUE"))) |>
#       pivot_wider(id_cols = -n, names_from = is_open_data, values_from = perc)
#   }   
#   
# }


# Open Data
plot_open_code_journal_bar <- function(data_2, color_2, institut) #, zoom_in, show_supplements)
{
  # print(data)
  # 
  # if(zoom_in) {
  #   yrange <- c(0, 20)
  # } else {
  #   yrange <- c(0, 50)
  # }
  
  # plot <- 
  data_2 |> 
    filter(institut_short %in% institut) |>
    plot_ly(x = ~year, y = ~is_open_code_perc, type = 'bar',
            color = ~institut_short,
            marker = list(line = list(color = 'rgb(0,5,5)',
                                      width = 1.0))) |>
    layout(barmode = 'stack',
           legend = list(xanchor = "left"),
           yaxis = list(title = '<b>Count of data sharing cases</b>',
                        #range = yrange,
                        ticksuffix = "%"),
           xaxis = list(title = '<b>Year</b>',
                        dtick = 1),
           paper_bgcolor = "#DCE3E5",
           plot_bgcolor = "#DCE3E5") |>
    config(displayModeBar = FALSE)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Open Code journal articles v2 (line chart) ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

# use combined_journal_oddpub data

data_open_code_journals_line <- function() { #total_perc
  #if (total_perc == FALSE) {
  data <- combined_journal_oddpub |>
    drop_na(institut_ubib) |>
    mutate(
      institut_ubib = case_when(
        institut_ubib == "Institut für Weltraumwissenschaften" ~ "Institut für Meteorologie (WE 3)",
        TRUE ~ institut_ubib
      )
    ) |>
    mutate(year = as.numeric(year)) |>
    count(is_open_code, year, institut_ubib) |> 
    filter(year >=2016) |>
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
    tidyr::complete(institut_ubib, year, is_open_code, fill = list(n = 0)) |> 
    mutate(institut_short = str_extract_all(institut_ubib,  "(?<=\\().+?(?=\\))")) |>
    mutate(institut_short = factor(institut_short, levels = c("WE 1", "WE 2", "WE 3"))) |>
    mutate(is_open_code = factor(is_open_code, levels = c("FALSE", "TRUE"))) |>
    pivot_wider(names_from = is_open_code, values_from = n) |>
    filter(`FALSE` > 0 | `TRUE` > 0)
}
#   } else {
#     #data <- od_journal |>
#     data <- combined_journal_oddpub |>
#       drop_na(institut_ubib) |>
#       mutate(
#         institut_ubib = case_when(
#           institut_ubib == "Institut für Weltraumwissenschaften" ~ "Institut für Meteorologie (WE 3)",
#           TRUE ~ institut_ubib
#         )
#       ) |>
#       mutate(year = as.numeric(year)) |>
#       count(is_open_data, year, institut_ubib) |> 
#       filter(year >=2016) |>
#       mutate(institut_ubib = str_remove(institut_ubib, "Institut für")) |>
#       mutate(institut_ubib = str_trim(institut_ubib)) |>
#       mutate(institut_ubib = factor(
#         institut_ubib,
#         levels = c(
#           "Geologische Wissenschaften (WE 1)",
#           "Geographische Wissenschaften (WE 2)",
#           "Meteorologie (WE 3)"
#         )
#       )) |>
#       tidyr::complete(institut_ubib, year, is_open_data, fill = list(n = 0)) |> 
#       group_by(institut_ubib, is_open_data) |> 
#       mutate(perc = n/sum(n)) |>
#       ungroup() |>
#       mutate(institut_short = str_extract_all(institut_ubib,  "(?<=\\().+?(?=\\))")) |>
#       mutate(institut_short = factor(institut_short, levels = c("WE 1", "WE 2", "WE 3"))) |>
#       mutate(is_open_data = factor(is_open_data, levels = c("FALSE", "TRUE"))) |>
#       pivot_wider(id_cols = -n, names_from = is_open_data, values_from = perc)
#   }   
#   
# }


# plot_open_code_journal_line <- function(data, institut) { #total_perc, , min, max
#   color <- c("#4FAC5B", "#377EB8")
#   
#   #  if (total_perc == FALSE) {
#   # filtered_data <- data |>
#   #   filter(year >= min & year <= max)
#   # 
#   
#   if (!is.null(institut)) {
#     filtered_data <- data |>
#       filter(institut_short %in% institut)
#   }
#   
#   
#   summed_data <- filtered_data |>
#     group_by(year) |>
#     summarise(`TRUE` = sum(`TRUE`),
#               `FALSE` = sum(`FALSE`), .groups = "drop")
#   
#   summed_data |>
#     plot_ly(x = ~ year, mode = 'lines') |>
#     add_trace(
#       #y = ~total_open_data,
#       y = ~`TRUE`,
#       name = "Open Code",
#       line = list(shape = "spline",
#                   width = 8,
#                   color = color[1]),
#       type = 'scatter', 
#       mode = 'markers', 
#       marker = list(color = color[1], size = 12),
#       legendgroup = "Legend",
#       showlegend = TRUE) |>
#     add_trace(
#       y = ~`FALSE`,
#       name = "No Open Code",
#       line = list(shape = "spline",
#                   width = 4,
#                   color = color[2]),
#       type = 'scatter', 
#       mode = 'markers', 
#       marker = list(color = color[2], size = 10),
#       legendgroup = "Legend",
#       showlegend = TRUE) |>
#     layout(
#       barmode = "relative",
#       xaxis = list(title = "Year"),
#       yaxis = list(title = "Count of code sharing cases"),
#       paper_bgcolor = "#DCE3E5",
#       plot_bgcolor = "#DCE3E5",
#       legend = list(
#         x = 1,
#         y = 1,
#         bgcolor = "#F2F2F2",
#         bordercolor = "#CCCCCC",
#         borderwidth = 1,
#         tracegroupgap = 10,
#         orientation = "v"
#       ) 
#     ) |>
#     config(displayModeBar = FALSE)
# }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Calculations of the numeric values inside of plots ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Open Access journal articles
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
make_OA_plot_data <- function(data_table)
{
  OA_plot_data <- data_table |>
    group_by(year, oa_status_manual) |>
    summarize(count = n(), .groups = "drop") |>
    calculate_OA_percentages(c("gold", "green", "hybrid")) |> # "bronze"
    rename(category = oa_status_manual)
  
  return(OA_plot_data)
}

calculate_OA_percentages <- function(OA_data, categories)
{
  publ_all <- OA_data |>
    group_by(year) |>
    summarise(all = sum(count), 
              .groups = "drop")
  
  publ_color_oa <- OA_data |>
    filter(oa_status_manual %in% categories) |>
    group_by(oa_status_manual, year) |>
    summarise(OA = sum(count), 
              .groups = "drop")
  
  OA_perc <- publ_color_oa |>
    left_join(publ_all, by = join_by(year)) |>
    mutate(perc = round(OA/all *100, 1)) |>
    ungroup()
  
  return(OA_perc)
}

OA_data <- geo_ressources |>
  drop_na(institut_short) |>
  make_OA_plot_data() |>
  filter(category != "bronze") |>
  filter(category != "closed") |>
  group_by(year) |>
  summarize(OA_perc = sum(perc), 
            .groups = "drop")

# Open licenses journal articles
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
make_OL_plot_data <- function(data_table)
{
  OL_plot_data <- data_table |> #geo_ressources
    group_by(year, license_manual) |>
    summarize(count = n(), .groups = "drop") |>
    calculate_OL_percentages(c("cc-by", 
                               "cc-by-sa",
                               "cc-by-nc",
                               "cc-by-nc-sa", 
                               "cc-by-nd", 
                               "cc-by-nc-nd")) |>
    rename(category = license_manual)
  
  return(OL_plot_data)
}

calculate_OL_percentages <- function(OL_data, categories)
{
  publ_all <- OL_data |>
    group_by(year) |>
    summarise(all = sum(count), 
              .groups = "drop")
  
  publ_color_ol <- OL_data |>
    filter(license_manual %in% categories) |>
    group_by(year, license_manual) |>
    summarise(OL = sum(count), 
              .groups = "drop")
  
  OL_perc <- publ_color_ol |>
    left_join(publ_all, by = join_by(year)) |>
    mutate(perc = round(OL/all *100, 1)) |>
    ungroup()
  
  return(OL_perc)
}

OL_data <- geo_ressources |>
  drop_na(institut_short) |> 
  filter(oa_status_manual != "closed" & 
           oa_status_manual != "bronze") |>
  make_OL_plot_data() |>
  group_by(year) |>
  summarize(OL_perc = sum(perc), 
            .groups = "drop")

# Open Access non-journal-article outputs
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
make_OA_plot_other <- function(data_table)
{
  OA_plot_other <- data_table |>
    group_by(year, oa_status_manual) |>
    summarize(count = n(), .groups = "drop") |>
    calculate_OA_percentages_other(c("gold", "green", "hybrid")) |> #, "bronze"
    rename(category = oa_status_manual)
  
  return(OA_plot_other)
}

calculate_OA_percentages_other <- function(OA_other, categories)
{
  publ_all <- OA_other |>
    #group_by(year) |>
    summarise(all = sum(count), 
              .groups = "drop")
  
  publ_color_oa <- OA_other |>
    filter(oa_status_manual %in% categories) |>
    group_by(oa_status_manual) |> # , year
    summarise(OA = sum(count))#, .groups = "drop")
  
  OA_perc_other <- publ_color_oa |>
    left_join(publ_all, by = character()) |> #join_by(year)) |> 
    mutate(perc = round(OA/all *100, 1)) #|>
    #ungroup()
  
  return(OA_perc_other)
}

OA_other <- data_other |>
  drop_na(institut_short) |>
  make_OA_plot_other() |>
  filter(category != "bronze") |>
  filter(category != "closed") |>
  #group_by(year) |>
  summarize(OA_perc_other = sum(perc))#, .groups = "drop")


# PIDs non-journal-article outputs
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
make_PID_plot_other <- function(data_table)
{
  PID_plot_other <- data_other |>
    filter(year >= 2016) |>
    mutate(pid_binary = is.na(pid)) |>
    group_by(year, pid_binary) |>
    summarize(count = n(), .groups = "drop") |>
    calculate_PID_percentages_other(c("TRUE", "FALSE")) |>
    rename(category = pid_binary)
  
  return(PID_plot_other)
}

calculate_PID_percentages_other <- function(PID_other, categories) #, categories
{
  publ_all <- PID_other |>
    #group_by(year) |>
    summarise(all = sum(count))

  publ_color_pid <- PID_other |>
    filter(pid_binary %in% categories) |>
    group_by(pid_binary) |> #, year
    summarise(PID = sum(count))

  PID_perc_other <- publ_color_pid |>
    left_join(publ_all, by =  character()) |> #join_by(year)) |>
    mutate(perc = round(PID/all *100, 1)) 
    #ungroup()
    
    return(PID_perc_other)

}



PID_other <- data_other |>
  filter(year >= 2016) |>
  mutate(pid_binary = is.na(pid)) |>
  drop_na(institut_short) |> 
  make_PID_plot_other() |>
  filter(category != "TRUE") |>
  summarize(PID_perc_other = sum(perc))
  #calculate_PID_percentages_other()


# 
# PID_other <- data_other |>
#   make_PID_plot_other() |>
#   group_by(year) |>
#   summarize(PID_perc_other = sum(perc), .groups = "drop")
