#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Functions for dashboard ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Data preparation
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

show_year <- as.integer("2025")

# OA colors labels
oa_status_colors <- c("gold", "hybrid", "green", "bronze", "closed")

# OA colors
color_palette <- c("#8F989D",
                   "#CD7F32",
                   "#FFD966",
                   "#4472C4",
                   "#303A3E",
                   "#2E933C",
                   "#4472C4")


# licenses colors
licenses_colors <- brewer.pal(9, "RdYlGn")

# Colors to check
# colors <- c("#1ba100", "#9dcf48","#ffff00", "#fdd303", "#f5292a", "#8d040e")
# "#D73027" "#F46D43" "#FDAE61" "#FEE08B" "#FFFFBF" "#D9EF8B" "#A6D96A" "#66BD63" "#1A9850"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Open access status of journals ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Loading journal articles
geo_ressources <- read.csv2(
  here("data", "journal_articles_v5.csv"),
  sep = ";",
  header = T,
  quote = "\"",
  encoding = "UTF-8"
) |>
  rename(year_release = year)

data_oa_journals <- function(data, min, max, total_perc, institut, color) {
  if (total_perc == FALSE) {
    data <- geo_ressources |> # 2186
      drop_na(institut_short) |>
      drop_na(oa_status_mod) |>
      mutate(oa_status_mod = case_when(oa_status_mod == "diamond" ~ "gold",
                                       TRUE ~ oa_status_mod)) |>
      count(year_release, oa_status_mod, institut_short) |>
      filter(year_release >= 2016, year_release <= 2025) |>
      tidyr::complete(institut_short, year_release, oa_status_mod, fill = list(n = 0)) |>
      mutate(
        n = as.numeric(n),
        oa_status_mod = factor(oa_status_mod, levels = oa_status_colors)
      ) |>
      pivot_wider(
        names_from = oa_status_mod,
        values_from = n,
        values_fn = sum
      )
    
  } else {
    data <- geo_ressources |>
      drop_na(institut_short) |>
      drop_na(oa_status_mod) |>      
      mutate(oa_status_mod = case_when(oa_status_mod == "diamond" ~ "gold",
                                       TRUE ~ oa_status_mod)) |>
      count(year_release, oa_status_mod, institut_short) |>
      filter(year_release >= 2016, year_release <= 2025) |>
      tidyr::complete(institut_short, year_release, oa_status_mod, fill = list(n = 0)) |>
      mutate(n = as.numeric(n)) |>
      group_by(institut_short, year_release) |>
      mutate(perc = n / sum(n)) |>
      ungroup() |>
      mutate(oa_status_mod = factor(oa_status_mod, levels = oa_status_colors)) |>
      pivot_wider(
        id_cols = c(institut_short, year_release),
        names_from = oa_status_mod,
        values_from = perc,
        values_fn = mean
      )
  }
}

plot_oa_journals <- function(data,
                             min,
                             max,
                             total_perc,
                             institut,
                             color_seq) {
  if (total_perc == FALSE) {
    data_abs <- data |>
      mutate(cumulative_oa = gold + green + hybrid) |>
      filter(year_release >= min & year_release <= max) |>
      filter(institut_short %in% institut)
    
    x_categories <- unique(data_abs$year_release)
    
    trendline_values <- data_abs |>
      # filter(year_release >= min & year_release <= max) |>
      # filter(institut_short %in% institut) |>
      group_by(institut_short) |>
      do({
        model <- lm(cumulative_oa ~ year_release, data = .)
        data.frame(
          year_release = .$year_release,
          cumulative_oa = .$cumulative_oa,
          trendline = predict(model)
        )
      }) |>
      ungroup()
    
    data_abs <- data_abs |>
      left_join(trendline_values,
                by = c("year_release", "institut_short", "cumulative_oa"))
    
    plot <- data_abs |>
      filter(year_release >= min & year_release <= max) |>
      filter(institut_short %in% institut) |>
      plot_ly(x = ~ list(institut_short, year_release)) |>
      add_trace(
        y = ~ gold,
        type = "bar",
        marker = list(
          color = color_palette[3],
          opacity = 1,
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "gold",
        visible = TRUE,
        legendrank = 1
      ) |>
      add_trace(
        y = ~ hybrid,
        type = "bar",
        marker = list(
          color = color_palette[7],
          opacity = 1,
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "hybrid",
        visible = TRUE,
        legendrank = 2
      ) |>
      add_trace(
        y = ~ green,
        type = "bar",
        marker = list(
          color = color_palette[6],
          opacity = 1,
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "green",
        visible = TRUE,
        legendrank = 3
      ) |>
      add_trace(
        y = ~ bronze,
        type = "bar",
        opacity = 0.4,
        marker = list(
          color = color_palette[2],
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "bronze",
        visible = TRUE,
        legendrank = 4
      ) |>
      add_trace(
        y = ~ closed,
        type = "bar",
        opacity = 0.4,
        marker = list(
          color = color_palette[1],
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "closed",
        visible = TRUE,
        legendrank = 5
      )
    
    for (i in seq_along(institut)) {
      inst <- institut[i]
      plot <- plot |>
        add_lines(
          data = data_abs |>
            filter(institut_short == inst),
          y = ~ cumulative_oa,
          name = if (i == 1)
            "Open Access Trendline"
          else
            NULL,
          line = list(
            width = 2,
            dash = "dash",
            color = "black"
          ),
          legendrank = 6,
          showlegend = i == 1
        )
    }
    
    plot <- plot |>
      layout(
        barmode = "relative",
        legend = list(
          orientation = "h",
          x = 0.5,
          y = 1.15 ,
          xanchor = "center",
          yanchor = "bottom"
        ),
        xaxis = list(title = FALSE),
        yaxis = list(title = "Number of journal articles"),
        paper_bgcolor = "#DCE3E5",
        plot_bgcolor = "#DCE3E5",
        margin = list(t = 100)
      ) |>
      config(displayModeBar = FALSE)
    
    return(plot)
    
  } else {
    data_perc <- data |>
      filter(year_release >= min & year_release <= max) |>
      filter(institut_short %in% institut) |>
      
      group_by(year_release, institut_short) |>
      mutate(total_pubs = gold + hybrid + green + bronze + closed) |>
      ungroup() |>
      mutate(across(
        c(gold, hybrid, green, bronze, closed),
        ~ .x / total_pubs,
        .names = "perc_{col}"
      )) |>
      mutate(cumulative_oa_perc = perc_gold + perc_green + perc_hybrid)
    
    x_categories <- unique(data_perc$year_release)
    
    trendline_values_perc <- data_perc |>
      # filter(year_release >= min & year_release <= max) |>
      # filter(institut_short %in% institut) |>
      group_by(institut_short) |>
      do({
        model <- lm(cumulative_oa_perc ~ year_release, data = .)
        data.frame(
          year_release = .$year_release,
          cumulative_oa_perc = .$cumulative_oa_perc,
          trendline = predict(model)
        )
      }) |>
      ungroup()
    
    data_perc <- data_perc |>
      left_join(
        trendline_values_perc,
        by = c("year_release", "institut_short", "cumulative_oa_perc")
      )
    
    plot <- data_perc |>
      filter(year_release >= min & year_release <= max) |>
      filter(institut_short %in% institut) |>
      plot_ly(x = ~ list(institut_short, year_release)) |>
      add_bars(
        y = ~ perc_gold,
        marker = list(
          color = color_palette[3],
          opacity = 1,
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "gold",
        visible = TRUE,
        legendrank = 1
      ) |>
      add_bars(
        y = ~ perc_hybrid,
        marker = list(
          color = color_palette[7],
          opacity = 1,
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "hybrid",
        visible = TRUE,
        legendrank = 2
      ) |>
      add_bars(
        y = ~ perc_green,
        marker = list(
          color = color_palette[6],
          opacity = 1,
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "green",
        visible = TRUE,
        legendrank = 3
      ) |>
      add_bars(
        y = ~ perc_bronze,
        opacity = 0.4,
        marker = list(
          color = color_palette[2],
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "bronze",
        visible = TRUE,
        legendrank = 4
      ) |>
      add_bars(
        y = ~ perc_closed,
        opacity = 0.4,
        marker = list(
          color = color_palette[1],
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "closed",
        visible = TRUE,
        legendrank = 5
      )
    
    for (i in seq_along(institut)) {
      inst <- institut[i]
      plot <- plot |>
        add_lines(
          data = data_perc |>
            filter(institut_short == inst),
          y = ~ cumulative_oa_perc,
          name = if (i == 1)
            "Open Access Trendline"
          else
            NULL,
          line = list(
            width = 2,
            dash = "dash",
            color = "black"
          ),
          legendrank = 6,
          showlegend = i == 1
        )
    }
    plot <- plot |>
      layout(
        barmode = "relative",
        legend = list(
          orientation = "h",
          x = 0.5,
          y = 1.15,
          xanchor = "center",
          yanchor = "bottom"
        ),
        xaxis = list(title = FALSE),
        yaxis = list(
          title = "% of journal articles",
          tickformat = ".0%",
          range = c(0, 1)
        ),
        paper_bgcolor = "#DCE3E5",
        plot_bgcolor = "#DCE3E5",
        margin = list(t = 100)
      ) |>
      config(displayModeBar = FALSE)
  }
  return(plot)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Licenses of journals ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_journals_licenses <- function(data, total_perc, institut, min, max) {
  if (total_perc == FALSE) {
    data <- geo_ressources |> # 2186
      drop_na(institut_short) |>
      filter(oa_status_mod != "closed" &
               oa_status_mod != "bronze") |> # 1455
      filter(license_mod != "no cc license") |> # 1330
      filter(license_mod != "copyright") |> # 1305
      count(year_release, license_mod, institut_short) |>
      filter(year_release >= 2016) |>
      filter(year_release <= 2025) |>
      tidyr::complete(institut_short, year_release, license_mod, fill = list(n = 0)) |>
      mutate(license_mod = factor(
        license_mod,
        levels = c(
          "cc-by",
          "cc-by-sa",
          "cc-by-nc",
          "cc-by-nc-sa",
          "cc-by-nd",
          "cc-by-nc-nd"
        )
      )) |>
      pivot_wider(
        names_from = license_mod,
        values_from = n,
        values_fn = sum
      )
    
  } else {
    data <- geo_ressources |>
      drop_na(institut_short) |>
      filter(oa_status_mod != "closed" &
               oa_status_mod != "bronze") |> # 1455
      filter(license_mod != "no cc license") |> # 1330
      filter(license_mod != "copyright") |> # 1305
      count(year_release, license_mod, institut_short) |>
      filter(year_release >= 2016) |>
      filter(year_release <= 2025) |>
      tidyr::complete(institut_short, year_release, license_mod, fill = list(n = 0)) |>
      group_by(institut_short, year_release) |>
      mutate(total_count = sum(n, na.rm = T)) |>
      mutate(perc = ifelse(total_count == 0, 0, n / total_count)) |>
      ungroup() |>
      mutate(license_mod = factor(
        license_mod,
        levels = c(
          "cc-by",
          "cc-by-sa",
          "cc-by-nc",
          "cc-by-nc-sa",
          "cc-by-nd",
          "cc-by-nc-nd"
        )
      )) |>
      pivot_wider(id_cols = -n,
                  names_from = license_mod,
                  values_from = perc)
    
  }
}



plot_journals_licenses <- function(data, min, max, total_perc, institut) {
  if (total_perc == FALSE) {
    data |>
      filter(year_release >= min & year_release <= max) |>
      filter(institut_short %in% institut) |>
      plot_ly(x = ~ list(institut_short, year_release)) |>
      add_bars(
        y = ~ `cc-by`,
        marker = list(
          color = licenses_colors[9],
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "cc-by"
      ) |>
      add_bars(
        y = ~ `cc-by-sa`,
        marker = list(
          color = licenses_colors[8],
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "cc-by-sa"
      ) |>
      add_bars(
        y = ~ `cc-by-nd`,
        marker = list(
          color = licenses_colors[7],
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "cc-by-nd"
      ) |>
      add_bars(
        y = ~ `cc-by-nc`,
        marker = list(
          color = licenses_colors[4],
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "cc-by-nc"
      ) |>
      add_bars(
        y = ~ `cc-by-nc-sa`,
        marker = list(
          color = licenses_colors[3],
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "cc-by-nc-sa"
      ) |>
      add_bars(
        y = ~ `cc-by-nc-nd`,
        marker = list(
          color = licenses_colors[2],
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "cc-by-nc-nd"
      ) |>
      layout(
        barmode = "relative",
        legend = list(orientation = "h", y = 1.2),
        xaxis = list(title = F),
        yaxis = list(title = "Number of journal articles (OA)"),
        paper_bgcolor = "#DCE3E5",
        plot_bgcolor = "#DCE3E5"
      ) |>
      config(displayModeBar = FALSE)
    
  } else {
    data |>
      filter(year_release >= min & year_release <= max) |>
      filter(institut_short %in% institut) |>
      plot_ly(x = ~ list(institut_short, year_release)) |>
      add_bars(
        y = ~ `cc-by`,
        marker = list(
          color = licenses_colors[9],
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "cc-by"
      ) |>
      add_bars(
        y = ~ `cc-by-sa`,
        marker = list(
          color = licenses_colors[8],
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "cc-by-sa"
      ) |>
      add_bars(
        y = ~ `cc-by-nd`,
        marker = list(
          color = licenses_colors[7],
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "cc-by-nd"
      ) |>
      add_bars(
        y = ~ `cc-by-nc`,
        marker = list(
          color = licenses_colors[4],
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "cc-by-nc"
      ) |>
      add_bars(
        y = ~ `cc-by-nc-sa`,
        marker = list(
          color = licenses_colors[3],
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "cc-by-nc-sa"
      ) |>
      add_bars(
        y = ~ `cc-by-nc-nd`,
        marker = list(
          color = licenses_colors[2],
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "cc-by-nc-nd"
      ) |>
      layout(
        barmode = "relative",
        legend = list(orientation = "h", y = 1.2),
        xaxis = list(title = F),
        yaxis = list(
          title = "% of journal articles",
          tickformat = ".0%",
          range = c(0, 1)
        ),
        paper_bgcolor = "#DCE3E5",
        plot_bgcolor = "#DCE3E5"
      ) |>
      config(displayModeBar = FALSE)
  }
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Non-journal-article outputs - genres ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Loading non-journal articles
data_other <- read.csv2(
  here("data", "non_journal_article_outputs_v5.csv"),
  header = T,
  quote = "\"",
  encoding = "UTF-8"
) |>
  filter(genre_manual != "conference_abstract") |>
  filter(genre_manual != "other")


data_oa_other <- function(data, min, max, total_perc, institut) {
  if (total_perc == FALSE) {
    data <- data_other |>
      count(year_release,
            oa_status_manual,
            institut_short,
            genre_manual) |>
      filter(year_release >= 2016) |>
      tidyr::complete(institut_short,
                      year_release,
                      oa_status_manual,
                      genre_manual,
                      fill = list(n = 0)) |>
      mutate(oa_status_manual = factor(oa_status_manual, levels = oa_status_colors)) |>
      pivot_wider(names_from = oa_status_manual, values_from = n)
    
    return(data)
  } else {
    data <- data_other |>
      count(year_release,
            oa_status_manual,
            institut_short,
            genre_manual) |>
      filter(year_release >= 2016) |>
      tidyr::complete(institut_short,
                      year_release,
                      oa_status_manual,
                      genre_manual,
                      fill = list(n = 0)) |>
      group_by(institut_short, year_release, genre_manual) |>
      mutate(total_count = sum(n)) |>
      mutate(perc = ifelse(total_count == 0, 0, n / total_count)) |>
      ungroup() |>
      mutate(oa_status_manual = factor(oa_status_manual, levels = oa_status_colors)) |>
      pivot_wider(id_cols = -n,
                  names_from = oa_status_manual,
                  values_from = perc)
    
    return(data)
  }
}

plot_oa_other <- function(data,
                          min,
                          max,
                          total_perc,
                          institut,
                          active_dropdown = 0) {
  genres <- unique(data$genre_manual)
  
  for (i in seq_along(genres)) {
    genre <- genres[i]
    visibility <- rep(FALSE, length(genres) * 5)
    
    start_idx <- (i - 1) * 5 + 1
    visibility[start_idx:(start_idx + 4)] <- TRUE
  }
  
  plot <- plot_ly()
  if (total_perc == FALSE) {
    genre <- genres[i]
    filtered <- data |> filter(genre_manual == genre) |>
      filter(institut_short %in% institut) |>
      filter(year_release >= min & year_release <= max)
    plot <- plot |>
      add_bars(
        data = filtered,
        x = ~ list(institut_short, year_release),
        y = ~ gold,
        type = "bar",
        marker = list(
          color = color_palette[3],
          opacity = 1,
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "gold",
        visible = ifelse(i == 1, TRUE, FALSE),
        legendrank = 1
      ) |>
      add_bars(
        data = filtered,
        x = ~ list(institut_short, year_release),
        y = ~ hybrid,
        type = "bar",
        marker = list(
          color = color_palette[7],
          opacity = 1,
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "hybrid",
        visible = ifelse(i == 1, TRUE, FALSE),
        legendrank = 2
      ) |>
      add_bars(
        data = filtered,
        x = ~ list(institut_short, year_release),
        y = ~ green,
        type = "bar",
        marker = list(
          color = color_palette[6],
          opacity = 1,
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "green",
        visible = ifelse(i == 1, TRUE, FALSE),
        legendrank = 3
      ) |>
      add_bars(
        data = filtered,
        x = ~ list(institut_short, year_release),
        y = ~ bronze,
        type = "bar",
        opacity = 0.4,
        marker = list(
          color = color_palette[2],
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "bronze",
        visible = ifelse(i == 1, TRUE, FALSE),
        legendrank = 4
      ) |>
      add_bars(
        data = filtered,
        x = ~ list(institut_short, year_release),
        y = ~ closed,
        type = "bar",
        opacity = 0.4,
        marker = list(
          color = color_palette[1],
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "closed",
        visible = ifelse(i == 1, TRUE, FALSE),
        legendrank = 5
      )
    
    plot <- plot |>
      layout(
        barmode = "relative",
        legend = list(
          orientation = "h",
          x = 0.6,
          y = 1.1,
          xanchor = "center",
          yanchor = "bottom"
        ),
        xaxis = list(title = FALSE, category = "multicategory"),
        yaxis = list(title = "Number of non-journal-article outputs"),
        paper_bgcolor = "#DCE3E5",
        plot_bgcolor = "#DCE3E5",
        margin = list(t = 100)
      ) |>
      config(displayModeBar = FALSE)
  } else {
    genre <- genres[i]
    filtered <- data |> filter(genre_manual == genre) |>
      filter(institut_short %in% institut) |>
      filter(year_release >= min & year_release <= max)
    plot <- plot |>
      add_bars(
        data = filtered,
        x = ~ list(institut_short, year_release),
        y = ~ gold,
        type = "bar",
        marker = list(
          color = color_palette[3],
          opacity = 1,
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "gold",
        visible = ifelse(i == 1, TRUE, FALSE),
        legendrank = 1
      ) |>
      add_bars(
        data = filtered,
        x = ~ list(institut_short, year_release),
        y = ~ hybrid,
        type = "bar",
        marker = list(
          color = color_palette[7],
          opacity = 1,
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "hybrid",
        visible = ifelse(i == 1, TRUE, FALSE),
        legendrank = 2
      ) |>
      add_bars(
        data = filtered,
        x = ~ list(institut_short, year_release),
        y = ~ green,
        type = "bar",
        marker = list(
          color = color_palette[6],
          opacity = 1,
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "green",
        visible = ifelse(i == 1, TRUE, FALSE),
        legendrank = 3
      ) |>
      add_bars(
        data = filtered,
        x = ~ list(institut_short, year_release),
        y = ~ bronze,
        type = "bar",
        opacity = 0.4,
        marker = list(
          color = color_palette[2],
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "bronze",
        visible = ifelse(i == 1, TRUE, FALSE),
        legendrank = 4
      ) |>
      add_bars(
        data = filtered,
        x = ~ list(institut_short, year_release),
        y = ~ closed,
        type = "bar",
        opacity = 0.4,
        marker = list(
          color = color_palette[1],
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "closed",
        visible = ifelse(i == 1, TRUE, FALSE),
        legendrank = 5
      )
    
    plot <- plot |>
      layout(
        barmode = "relative",
        legend = list(
          orientation = "h",
          x = 0.6,
          y = 1.1,
          xanchor = "center",
          yanchor = "bottom"
        ),
        xaxis = list(title = FALSE, category = "multicategory"),
        yaxis = list(
          title = "% of non-journal-article outputs",
          tickformat = ".0%",
          range = c(0, 1)
        ),
        paper_bgcolor = "#DCE3E5",
        plot_bgcolor = "#DCE3E5"
      ) |>
      config(displayModeBar = FALSE)
  }
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Non-journal-article persistent identifiers ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_other_pid <- function(data, min, max, total_perc, institut) {
  if (total_perc == FALSE) {
    data <- data_other |>
      drop_na(institut_short) |>
      mutate(pid_binary = is.na(pid)) |>
      filter(year_release >= 2016) |>
      count(year_release, pid_binary, institut_short) |>
      tidyr::complete(institut_short, year_release, pid_binary, fill = list(n = 0)) |>
      mutate(pid_binary = factor(pid_binary, levels = c("TRUE", "FALSE"))) |>
      pivot_wider(names_from = pid_binary, values_from = n)
    
  } else {
    data <- data_other |>
      drop_na(institut_short) |>
      mutate(pid_binary = is.na(pid)) |>
      filter(year_release >= 2016) |>
      count(year_release, pid_binary, institut_short) |>
      tidyr::complete(institut_short, year_release, pid_binary, fill = list(n = 0)) |>
      group_by(institut_short, year_release) |>
      mutate(total_count = sum(n)) |>
      ungroup() |>
      mutate(perc = ifelse(total_count == 0, 0, n / total_count)) |>
      mutate(pid_binary = factor(pid_binary, levels = c("TRUE", "FALSE"))) |>
      pivot_wider(id_cols = -n,
                  names_from = pid_binary,
                  values_from = perc)
    
  }
}

plot_other_pid <- function(data, min, max, total_perc, institut) {
  data <- data |>
    filter(year_release >= min & year_release <= max) |>
    filter(institut_short %in% institut) |>
    filter(!is.na(`FALSE`) & !is.na(`TRUE`)) |>
    filter(!is.nan(`FALSE`) & !is.nan(`TRUE`))
  
  if (total_perc == FALSE) {
    data |>
      filter(year_release >= min & year_release <= max) |>
      filter(institut_short %in% institut) |>
      plot_ly(x = ~ list(institut_short, year_release)) |>
      add_bars(
        y = ~ `FALSE`,
        marker = list(
          color = color_palette[6],
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "Has PID"
      ) |>
      add_bars(
        y = ~ `TRUE`,
        marker = list(
          color = color_palette[3],
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "Has no PID"
      ) |>
      layout(
        barmode = "relative",
        legend = list(orientation = "h", y = 1.2),
        xaxis = list(title = F),
        yaxis = list(title = "Number of non-journal-article outputs"),
        legend = list(x = 0.55, y = 0.95),
        paper_bgcolor = "#DCE3E5",
        plot_bgcolor = "#DCE3E5"
      ) |>
      config(displayModeBar = FALSE)
  } else {
    data |>
      filter(year_release >= min & year_release <= max) |>
      filter(institut_short %in% institut) |>
      plot_ly(x = ~ list(institut_short, year_release)) |>
      add_bars(
        y = ~ `FALSE`,
        marker = list(
          color = color_palette[6],
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name = "Has PID"
      ) |>
      add_bars(
        y = ~ `TRUE`,
        marker = list(
          color = color_palette[3],
          line = list(color = "rgb(0,0,0)", width = 1.2)
        ),
        name =  "Has no PID"
      ) |>
      layout(
        barmode = "relative",
        legend = list(orientation = "h", y = 1.2),
        xaxis = list(title = F, category = "multicategory"),
        yaxis = list(
          title = "% of non-journal-article outputs",
          tickformat = ".0%",
          range = c(0, 1)
        ),
        paper_bgcolor = "#DCE3E5",
        plot_bgcolor = "#DCE3E5"
      ) |>
      config(displayModeBar = FALSE)
  }
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Open Data journal articles ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
geo_ressources_od <- read.csv2(
  here("data", "open_data_earth_sciences_fu_all_journal_articles_processed_2016_2024_v1.csv"),
  header = T,
  quote = "\"",
  encoding = "UTF-8"
) |>
  rename(year_release = year)

data_od_total_articles <- function(total_perc, institut) {
  if (total_perc == FALSE) {
    data <- geo_ressources_od |>
      filter(year_release >= 2016, year_release <= 2024) |>
      count(institut_short,
            year_release,
            open_data_confirmed_processed,
            name = "n") |>
      tidyr::complete(institut_short,
                      year_release,
                      open_data_confirmed_processed,
                      fill = list(n = 0)) |>
      pivot_wider(
        names_from = open_data_confirmed_processed,
        values_from = n,
        values_fill = 0
      ) |>
      group_by(institut_short, year_release) |>
      rename(
        open_data = 'TRUE',
        no_open_data = 'FALSE',
        not_screened = 'NA'
      )
    
  } else {
    data <- geo_ressources_od |>
      filter(year_release >= 2016, year_release <= 2024) |>
      count(institut_short,
            year_release,
            open_data_confirmed_processed) |>
      tidyr::complete(institut_short,
                      year_release,
                      open_data_confirmed_processed,
                      fill = list(n = 0)) |>
      group_by(institut_short, year_release) |>
      mutate(total_count = sum(n)) |>
      ungroup() |>
      mutate(perc = ifelse(total_count == 0, 0, n / total_count)) |>
      mutate(open_data_confirmed_processed =
               factor(
                 open_data_confirmed_processed,
                 levels = c("TRUE", "FALSE", NA)
               )) |>
      pivot_wider(id_cols = -n,
                  names_from = open_data_confirmed_processed,
                  values_from = perc) |>
      rename(
        open_data = 'TRUE',
        no_open_data = 'FALSE',
        not_screened = 'NA'
      )
  }
}

plot_od_total_articles <- function(data, total_perc, institut, min, max) {
  if (total_perc == FALSE) {
    plot <- data |>
      filter(year_release >= min & year_release <= max) |>
      filter(institut_short %in% institut) |>
      plot_ly(x = ~ list(institut_short, year_release)) |>
      add_trace(
        y = ~ open_data,
        name = "open data",
        visible = TRUE,
        legendrank = 1,
        opacity = 1,
        type = "bar",
        marker = list(
          color = "#f0ba51",
          line = list(color = "rgb(0,0,0)", width = 1.5)
        )
      ) |>
      # add_trace(y = ~reuse_data,
      #           name = "reused data", type = "bar",
      #           visible = TRUE, legendrank = 1,
      #           marker = list(color = "#007265",
      #                         line = list(color = "rgb(0,0,0)",
      #                                     width = 1.5))) |>
      # add_trace(y = ~open_and_reuse_data,
      #           name = "open and reused data", type = "bar",
      #           visible = TRUE, legendrank = 1,
      #           marker = list(color = "#644587",
      #                         line = list(color = "rgb(0,0,0)",
      #                                     width = 1.5))) |>
      add_trace(
        y = ~ no_open_data,
        name = "no open data",
        type = "bar",
        opacity = 1,
        visible = TRUE,
        legendrank = 2,
        marker = list(
          color = "#303a3f",
          line = list(color = "rgb(0,0,0)", width = 1.5)
        )
      ) |>
      add_trace(
        y = ~ not_screened,
        name = "article not screened",
        type = "bar",
        opacity = 0.4,
        visible = TRUE,
        legendrank = 3,
        marker = list(
          color = "#b6b6b6",
          line = list(color = "rgb(0,0,0)", width = 1.5)
        )
      ) |>
      layout(
        barmode = "stack",
        legend = list(
          orientation = "h",
          x = 0.5,
          y = 1.15,
          xanchor = "center",
          yanchor = "bottom",
          traceorder = "normal"
        ),
        xaxis = list(title = FALSE),
        yaxis = list(title = "Number of journal articles"),
        paper_bgcolor = "#DCE3E5",
        plot_bgcolor = "#DCE3E5",
        margin = list(t = 100)
      ) |>
      config(displayModeBar = FALSE)
    
    
    
  } else {
    plot <- data |>
      filter(year_release >= min & year_release <= max) |>
      filter(institut_short %in% institut) |>
      plot_ly(x = ~ list(institut_short, year_release)) |>
      add_trace(
        y = ~ open_data,
        name = "open data",
        visible = TRUE,
        legendrank = 1,
        opacity = 1,
        type = "bar",
        marker = list(
          color = "#f0ba51",
          line = list(color = "rgb(0,0,0)", width = 1.5)
        )
      ) |>
      # add_trace(y = ~reuse_data,
      #           name = "reused data (%)", type = "bar",
      #           visible = TRUE, legendrank = 1,
      #           marker = list(color = "#007265",
      #                         line = list(color = "rgb(0,0,0)",
      #                                     width = 1.5))) |>
      # add_trace(y = ~open_and_reuse_data,
      #           name = "open and reused data (%)", type = "bar",
      #           visible = TRUE, legendrank = 1,
      #           marker = list(color = "#644587",
      #                         line = list(color = "rgb(0,0,0)",
      #                                     width = 1.5))) |>
      add_trace(
        y = ~ no_open_data,
        name = "no open data",
        type = "bar",
        opacity = 1,
        visible = TRUE,
        legendrank = 2,
        marker = list(
          color = "#303a3f",
          line = list(color = "rgb(0,0,0)", width = 1.5)
        )
      ) |>
      add_trace(
        y = ~ not_screened,
        name = "article not screened",
        type = "bar",
        opacity = 0.4,
        visible = TRUE,
        legendrank = 3,
        marker = list(
          color = "#b6b6b6",
          line = list(color = "rgb(0,0,0)", width = 1.5)
        )
      ) |>
      layout(
        barmode = "relative",
        legend = list(
          orientation = "h",
          x = 0.5,
          y = 1.15,
          xanchor = "center",
          yanchor = "bottom",
          traceorder = "normal"
        ),
        xaxis = list(title = FALSE),
        yaxis = list(
          title = "% of journal articles",
          tickformat = ".0%",
          range = c(0, 1)
        ),
        paper_bgcolor = "#DCE3E5",
        plot_bgcolor = "#DCE3E5",
        margin = list(t = 100)
      ) |>
      config(displayModeBar = FALSE)
  }
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Open data by repository type ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

od_dataset_level <- read.csv2(
  here("data", "open_data_earth_sciences_fu_dataset_level_processed_2016_2024_v1.csv"),
  header = T,
  quote = "\"",
  encoding = "UTF-8"
) # 392

data_od_treemap_repos <- function(data) {
  od_data <- od_dataset_level |>
    filter(open_data_assessment_manual_Numbat == "open_data")
  
  n_total <- nrow(od_data)
  
  treemap_plot_data_head_1 <- od_data |>
    summarise(
      labels = "All datasets",
      parents = "",
      n = n(),
      perc = 100
    )
  
  treemap_plot_data_head_2 <- od_data |>
    count(repository_category_processed, name = "n") |>
    transmute(
      labels = case_when(
        repository_category_processed == "disciplinary repository" ~
          "Disciplinary repositories",
        repository_category_processed == "general-purpose repository" ~
          "General-purpose repositories",
        TRUE ~ as.character(repository_category_processed)
      ),
      parents = "All datasets",
      n = n,
      perc = n / n_total * 100
    )
  
  treemap_plot_data <- od_data |>
    count(repository_category_processed,
          repository_clean_manual_Numbat,
          name = "n") |>
    transmute(
      labels = repository_clean_manual_Numbat,
      parents = case_when(
        repository_category_processed == "disciplinary repository" ~
          "Disciplinary repositories",
        repository_category_processed == "general-purpose repository" ~
          "General-purpose repositories",
        TRUE ~ as.character(repository_category_processed)
      ),
      n = n,
      perc = n / n_total * 100
    )
  
  treemap_plot_data_labeled <-
    bind_rows(treemap_plot_data_head_1,
              treemap_plot_data_head_2,
              treemap_plot_data) |>
    mutate(
      treemap_type = case_when(
        labels == "Disciplinary repositories" ~
          "Disciplinary_group",
        
        labels == "General-purpose repositories" ~
          "General-purpose_group",
        
        parents == "Disciplinary repositories" ~
          "Disciplinary_leaf",
        
        parents == "General-purpose repositories" ~
          "General-purpose_leaf",
        
        labels == "All datasets" ~
          "root",
        
        TRUE ~ "other"
      ),
      
      color = case_when(
        treemap_type == "Disciplinary_group" ~ "#f8dca5",
        treemap_type == "Disciplinary_leaf" ~ "#f0ba51",
        treemap_type == "General-purpose_group" ~ "#6cbaba",
        treemap_type == "General-purpose_leaf" ~ "#2A8A7D",
        treemap_type == "root" ~ "#D6EAE8",
        TRUE ~ "#DCE3E5"
      )
    )
  
  return(treemap_plot_data_labeled)
}

plot_od_treemap_repos <- function(data) {
  #, total_perc
  
  hovertext <- paste0(
    "<b>{labels}</b><span style='font-family:courier'>",
    "<br>n = {n}",
    "<br>% of total = {round(perc, 1)}%",
    "</span>"
  )
  
  plot <-
    data |> plot_ly(
      labels = ~ labels,
      parents = ~ parents,
      values = ~ n,
      type = "treemap",
      branchvalues = "total",
      textinfo = "label",
      hoverinfo = "text",
      hovertext = ~ str_glue(hovertext),
      marker = list(
        colors = ~ color,
        colorscale = NULL,
        showscale = FALSE,
        line = list(color = "#2F3B4A", width = 1),
        pad = list(
          b = 2,
          l = 2,
          r = 2,
          t = 25
        )
      ),
      pathbar = list(visible = FALSE),
      outsidetextfont = list(size = 20, color = "#263635"),
      insidetextfont = list(size = 20, color = "#263635")
    ) |>
    layout(paper_bgcolor = "#DCE3E5",
           plot_bgcolor = "#DCE3E5") |>
    config(displayModeBar = FALSE)
  
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Calculations of the numeric values inside of plots ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Open Access journal articles
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
make_OA_plot_data <- function(data_table)
{
  OA_plot_data <- data_table |>
    group_by(year_release, oa_status_mod) |>
    summarize(count = n(), .groups = "drop") |>
    calculate_OA_percentages(c("gold", "green", "hybrid")) |> # "bronze"
    rename(category = oa_status_mod)
  
  return(OA_plot_data)
}

calculate_OA_percentages <- function(OA_data, categories)
{
  publ_all <- OA_data |>
    group_by(year_release) |>
    summarise(all = sum(count), .groups = "drop")
  
  publ_color_oa <- OA_data |>
    filter(oa_status_mod %in% categories) |>
    group_by(oa_status_mod, year_release) |>
    summarise(OA = sum(count), .groups = "drop")
  
  OA_perc <- publ_color_oa |>
    left_join(publ_all, by = join_by(year_release)) |>
    mutate(perc = round(OA / all * 100, 1)) |>
    ungroup()
  
  return(OA_perc)
}

OA_data <- geo_ressources |>
  drop_na(institut_short) |>
  make_OA_plot_data() |>
  filter(category != "bronze") |>
  filter(category != "closed") |>
  group_by(year_release) |>
  summarize(OA_perc = sum(perc), .groups = "drop")

# Open licenses journal articles
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
make_OL_plot_data <- function(data_table)
{
  OL_plot_data <- data_table |>
    group_by(year_release, license_mod) |>
    summarize(count = n(), .groups = "drop") |>
    calculate_OL_percentages(c(
      "cc-by",
      "cc-by-sa",
      "cc-by-nc",
      "cc-by-nc-sa",
      "cc-by-nd",
      "cc-by-nc-nd"
    )) |>
    rename(category = license_mod)
  
  return(OL_plot_data)
}

calculate_OL_percentages <- function(OL_data, categories)
{
  publ_all <- OL_data |>
    group_by(year_release) |>
    summarise(all = sum(count), .groups = "drop")
  
  publ_color_ol <- OL_data |>
    filter(license_mod %in% categories) |>
    group_by(year_release, license_mod) |>
    summarise(OL = sum(count), .groups = "drop")
  
  OL_perc <- publ_color_ol |>
    left_join(publ_all, by = join_by(year_release)) |>
    mutate(perc = round(OL / all * 100, 1)) |>
    ungroup()
  
  return(OL_perc)
}

OL_data <- geo_ressources |>
  drop_na(institut_short) |>
  filter(oa_status_mod != "closed" &
           oa_status_mod != "bronze") |>
  make_OL_plot_data() |>
  group_by(year_release) |>
  summarize(OL_perc = sum(perc), .groups = "drop")

# Open Access non-journal-article outputs
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
make_OA_plot_other <- function(data_table)
{
  OA_plot_other <- data_table |>
    group_by(year_release, oa_status_manual) |>
    summarize(count = n(), .groups = "drop") |>
    calculate_OA_percentages_other(c("gold", "green", "hybrid")) |> #, "bronze"
    rename(category = oa_status_manual)
  
  return(OA_plot_other)
}

calculate_OA_percentages_other <- function(OA_other, categories)
{
  publ_all <- OA_other |>
    #group_by(year_release) |>
    summarise(all = sum(count), .groups = "drop")
  
  publ_color_oa <- OA_other |>
    filter(oa_status_manual %in% categories) |>
    group_by(oa_status_manual) |>
    summarise(OA = sum(count))
  
  OA_perc_other <- publ_color_oa |>
    left_join(publ_all, by = character()) |>
    mutate(perc = round(OA / all * 100, 1)) #|>
  #ungroup()
  
  return(OA_perc_other)
}

OA_other <- data_other |>
  drop_na(institut_short) |>
  make_OA_plot_other() |>
  filter(category != "bronze") |>
  filter(category != "closed") |>
  #group_by(year_release) |>
  summarize(OA_perc_other = sum(perc))#, .groups = "drop")


# PIDs non-journal-article outputs
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
make_PID_plot_other <- function(data_table)
{
  PID_plot_other <- data_other |>
    filter(year_release >= 2016) |>
    mutate(pid_binary = is.na(pid)) |>
    group_by(year_release, pid_binary) |>
    summarize(count = n(), .groups = "drop") |>
    calculate_PID_percentages_other(c("TRUE", "FALSE")) |>
    rename(category = pid_binary)
  
  return(PID_plot_other)
}

calculate_PID_percentages_other <- function(PID_other, categories)
  #, categories
{
  publ_all <- PID_other |>
    #group_by(year_release) |>
    summarise(all = sum(count))
  
  publ_color_pid <- PID_other |>
    filter(pid_binary %in% categories) |>
    group_by(pid_binary) |> #, year
    summarise(PID = sum(count))
  
  PID_perc_other <- publ_color_pid |>
    left_join(publ_all, by =  character()) |> #join_by(year)) |>
    mutate(perc = round(PID / all * 100, 1))
  #ungroup()
  
  return(PID_perc_other)
  
}

PID_other <- data_other |>
  filter(year_release >= 2016) |>
  mutate(pid_binary = is.na(pid)) |>
  drop_na(institut_short) |>
  make_PID_plot_other() |>
  filter(category != "TRUE") |>
  summarize(PID_perc_other = sum(perc))
#calculate_PID_percentages_other()

# Open Data by repository for journal articles
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
OD_repo_perc <-  geo_ressources_od |>
  filter(year_release >= 2016) |>
  filter(open_data_confirmed_processed == "TRUE") |>
  filter(!is.na(repo_type_processed)) |>
  filter(repo_type_processed %in% c("disciplinary repository", "general-purpose repository")) |>
  count(repo_type_processed, name = "n") |>
  mutate(perc = round(n / sum(n) * 100, 1))

# Open Data on article level journal articles
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
make_OD_plot_data <- function(data_table)
{
  OD_plot_data <- data_table |>
    group_by(year_release, open_data_confirmed_processed) |>
    summarize(count = n(), .groups = "drop") |>
    calculate_OD_percentages(c("TRUE")) |>
    rename(category = open_data_confirmed_processed)
  
  return(OD_plot_data)
}

calculate_OD_percentages <- function(OD_data, categories)
{
  publ_all <- OD_data |>
    group_by(year_release) |>
    summarise(all = sum(count), .groups = "drop")
  
  publ_color_od <- OD_data |>
    filter(open_data_confirmed_processed %in% categories) |>
    group_by(open_data_confirmed_processed, year_release) |>
    summarise(OD = sum(count), .groups = "drop")
  
  OD_perc <- publ_color_od |>
    left_join(publ_all, by = join_by(year_release)) |>
    mutate(perc = round(OD / all * 100, 1)) |>
    #select(open_data_confirmed_processed, perc) |>
    ungroup()
  
  return(OD_perc)
}

OD_data <- geo_ressources_od |>
  drop_na(institut_short) |>
  drop_na(open_data_confirmed_processed) |>
  make_OD_plot_data() |>
  group_by(year_release) |>
  summarize(OD_perc = sum(perc), .groups = "drop")

OD_perc_overall <- OD_data |>
  summarise(OD_perc = sum(OD_perc)) |>
  pull(OD_perc)