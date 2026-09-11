#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Modules for dashboard ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Open access status of journals ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# UI modules ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

metric_box <- function(
  style_resp = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
  title, value, value_text, plot, info_id, info_title, info_text, info_alignment = "right"
) {
  wellPanel(
    style = style_resp,
    fluidRow(
      column(8, align = "left", h4(style = "color: #aa1c7d;text-align:left;font-size:20px;", title)),
      column(4,
        align = "right",
        h4(actionButton(
          inputId = info_id, label = "", icon = icon("circle-info"),
          class = "btn-primary", style = "padding:1px"
        )),
        bsPopover(info_id,
          info_title,
          info_text,
          info_alignment,
          options = list(container = "body")
        ),
        tags$style(".popover{width: 300px;}")
      )
    ),
    h1(style = "color: #aa1c7d;text-align:left;font-size:40px;", value),
    h4(style = "color: #aa1c7d;text-align:left;font-size:18px;", value_text),
    plot
  )
}

OA_text <- strwrap("Use the buttons to change time interval, institute(s) displayed,
                   or to toggle the Y-axis between “%” or “number of articles”.

                   The graphic is based on enriched data from the university
                   bibliography of the FU Berlin.

                   Click on any Open Access name(s) to unselect/select them.

                   Hover over the plots to see the numbers for individual data categories.") |>
  paste(collapse = " ")


OL_text <- strwrap("Use the buttons to adjust the time interval, select institutes, or toggle the Y-axis between percentages (%) and the number of articles.

                   The graphic is based on enriched data from the University Bibliography of FU Berlin.

                   Click on any CC license status name(s) to select or deselect them.

                   Hover over the plots to see the numbers for individual data categories.") |>
  paste(collapse = " ")

OA_text_non_j <- strwrap("Use the buttons to change time interval, institute(s) displayed,
                   or to toggle the Y-axis between “%” or “number of articles”.

                   The graphic is based on enriched data from the university
                   bibliography of the FU Berlin.

                   Click on any Open Access status name(s) to unselect/select them.

                   Hover over the plots to see the numbers for individual data categories.") |>
  paste(collapse = " ")

PID_text_non_j <- strwrap("Use the buttons to change time interval, institute(s) displayed,
                   or to toggle the Y-axis between “%” or “number of articles”.

                   The graphic is based on enriched data from the university
                   bibliography of the FU Berlin.

                   Click on Has PID or No PID to unselect/select them.

                   Hover over the plots to see the numbers for individual data categories.") |>
  paste(collapse = " ")

OD_total_text <- strwrap("Use the buttons to change time interval, institute(s) displayed,
                   or to toggle the Y-axis between “%” or “number of articles”.

                   The graphic is based on semi-autormatically processed data on Open Data
                   validation for journal articles listed in the university
                   bibliography of the FU Berlin.

                   Click on any legend group item to unselect/select Open Data/No Open Data.

                   Hover over the plots to see the numbers for individual data categories.") |>
  paste(collapse = " ")

OD_repo_text <- strwrap("Use the treemap to explore the distirubtion of Open Data across
                  different repositories. The size of each rectangle represents the number
                  of articles in that category.

                  Click on a category to zoom into General-purpose or disciplinary repositories,
                  or further into individual repository.

                  Click on the center of the treemap to move back to the previous level.

                  Hover over categories to see the number of datasets (n) and its share from the total
                  number of datasets.") |>
  paste(collapse = " ")


moduleUI_journal_OA <- function(id) {
  ns <- NS(id)

  wellPanel(
    style = "padding-top: 10px; padding-bottom: 0px;",
    fluidRow(
      column(
        8,
        h2(strong("Open Access status of journal articles"), align = "left"),
      )
    ),
    fluidRow(
      column(
        3,
        sliderInput(
          ns("year"),
          label = NULL,
          min = 2016,
          max = 2024,
          value = c(2016, 2024),
          sep = "",
          ticks = FALSE,
          round = TRUE
        )
      ),
      column(
        3,
        checkboxGroupInput(
          ns("select_inst"),
          label = NULL,
          choices = list(
            "Geological Sciences (WE 1)" = "WE 1",
            "Geographical Sciences (WE 2)" = "WE 2",
            "Meteorology (WE 3)" = "WE 3"
          ),
          selected = c("WE 1", "WE 2", "WE 3")
        )
      ),
      (column(
        3,
        checkboxInput(ns("checkbox_perc"), label = "Display in %", value = FALSE)
      ))
    ),
    fluidRow(
      column(
        8,
        fluidRow(
          style = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
          metric_box(
            title = "Open Access status of journal articles by year and institute",
            value = paste(round((OA_data |> filter(year_release == show_year))[["OA_perc"]], 0), "%"),
            value_text = paste0("of publications were open access in ", show_year),
            plot = plotlyOutput(ns("plot_journal_oa"), height = "400px"),
            info_id = "infoOA",
            info_title = "Open Access",
            info_text = OA_text
          )
        )
      ),
      column(
        4, includeMarkdown("texts/text_geo_OA_journal.md")
      )
    )
  )
}


moduleUI_journal_license <- function(id) {
  ns <- NS(id)

  wellPanel(
    style = "padding-top: 10px; padding-bottom: 0px;",
    fluidRow(column(
      8,
      h2(strong("Creative Commons Licenses of journal articles"), align = "left"),
    )),
    fluidRow(
      column(
        3,
        sliderInput(
          ns("year"),
          label = NULL,
          min = 2016,
          max = 2024,
          value = c(2016, 2024),
          sep = "",
          ticks = FALSE,
          round = TRUE
        )
      ),
      column(
        3,
        checkboxGroupInput(
          ns("select_inst"),
          inline = F,
          label = NULL,
          choices = list(
            "Geological Sciences (WE 1)" = "WE 1",
            "Geographical Sciences (WE 2)" = "WE 2",
            "Meteorology (WE 3)" = "WE 3"
          ),
          selected = c("WE 1", "WE 2", "WE 3")
        )
      ),
      column(
        3,
        checkboxInput(
          ns("checkbox_perc"),
          label = "Display in %",
          value = FALSE
        )
      )
    ),
    fluidRow(
      column(
        8,
        wellPanel(
          style = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
          metric_box(
            title = "Creative Commons Licenses of journal articles by year and institute",
            value = paste(round((OL_data |> filter(year_release == show_year))[["OL_perc"]], 0), "%"),
            value_text = paste0("of publications had a Creative Commons license in ", show_year),
            plot = plotlyOutput(ns("plot_journal_license"), height = "400px"),
            info_id = "infoOL",
            info_title = "Open Licenses",
            info_text = OL_text
          )
        )
      ),
      column(4, includeMarkdown("texts/text_geo_licenses_journal.md"))
    )
  )
}


moduleUI_non_journal_OA <- function(id) {
  ns <- NS(id)
  genre_vals <- sort(unique(data_other$genre_manual))

  genre_labels <- c(
    "Book" = "book",
    "Book chapter" = "book_chapter",
    "Conference paper" = "conference_paper"
  )

  genre_choices <- genre_labels[genre_labels %in% genre_vals]

  wellPanel(
    style = "padding-top: 10px; padding-bottom: 0px;",
    fluidRow(column(
      8,
      h2(strong("Open Access status of non-journal-article outputs"), align = "left"),
    )),
    fluidRow(
      column(
        3,
        selectInput(
          ns("genre_dropdown"),
          label = "Select non-journal-article output",
          choices = genre_choices,
          selected = genre_choices[1]
        )
      ),
      column(
        3,
        sliderInput(
          ns("year"),
          label = NULL,
          min = 2016,
          max = 2024,
          value = c(2016, 2024),
          sep = "",
          ticks = FALSE,
          round = TRUE
        ),
        checkboxInput(ns("checkbox_perc"),
          label = "Display in %",
          value = FALSE
        )
      ),
      column(
        3,
        checkboxGroupInput(
          ns("select_institut"),
          label = NULL,
          choices = list(
            "Geological Sciences (WE 1)" = "WE 1",
            "Geographical Sciences (WE 2)" = "WE 2",
            "Meteorology (WE 3)" = "WE 3"
          ),
          selected = c("WE 1", "WE 2", "WE 3")
        )
      )
    ),
    fluidRow(
      column(
        8,
        wellPanel(
          style = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
          metric_box(
            title = "Open Access status of non-journal-article outputs by year and institute",
            value = paste0(round(OA_other[["OA_perc_other"]], 0), "%"),
            value_text = paste0("of non-journal-article outputs were OA between 2016 and 2024"),
            plot = plotlyOutput(ns("plot_other_oa"), height = "400px"),
            info_id = "infoOA_alt",
            info_title = "Open Access",
            info_text = OA_text_non_j
          )
        )
      ),
      column(4, includeMarkdown("texts/text_geo_OA_other.md"))
    )
  )
}

moduleUI_non_journal_pid <- function(id) {
  ns <- NS(id)

  wellPanel(
    style = "padding-top: 10px; padding-bottom: 0px;",
    fluidRow(column(
      8,
      h2(strong("Persistent identifiers of non-journal-article outputs"), align = "left"),
    )),
    fluidRow(
      column(
        3,
        sliderInput(
          ns("year"),
          label = NULL,
          min = 2016,
          max = 2024,
          value = c(2016, 2024),
          sep = "",
          ticks = FALSE,
          round = TRUE
        )
      ),
      column(
        3,
        checkboxGroupInput(
          ns("select_inst"),
          label = NULL,
          choices = list(
            "Geological Sciences (WE 1)" = "WE 1",
            "Geographical Sciences (WE 2)" = "WE 2",
            "Meteorology (WE 3)" = "WE 3"
          ),
          selected = c("WE 2", "WE 1", "WE 3")
        )
      ),
      (column(
        3,
        checkboxInput(ns("checkbox_perc"), label = "Display in %", value = FALSE)
      ))
    ),
    fluidRow(
      column(
        8,
        wellPanel(
          style = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
          metric_box(
            title = "PIDs of non-journal-article outputs by year and institute",
            value = paste(round(PID_other[["PID_perc_other"]], 0), "%"),
            value_text = paste0("of non-journal-article outputs had a PID between 2016 and 2024"),
            plot = plotlyOutput(ns("plot_other_pid"), height = "400px"),
            info_id = "infoOL_alt",
            info_title = "PID",
            info_text = PID_text_non_j
          )
        )
      ),
      column(
        4, includeMarkdown("texts/text_geo_pid_other.md")
      )
    )
  )
}

moduleUI_open_data_articles <- function(id) {
  ns <- NS(id)

  wellPanel(
    style = "padding-top: 10px; padding-bottom: 0px;",
    fluidRow(column(
      8,
      h2(strong("Journal articles with Open Data mentions"), align = "left"),
    )),
    fluidRow(
      column(
        3,
        sliderInput(
          ns("year"),
          label = NULL,
          min = 2016,
          max = 2024,
          value = c(2016, 2024),
          sep = "",
          ticks = FALSE,
          round = TRUE
        )
      ),
      column(
        3,
        checkboxGroupInput(
          ns("select_inst"),
          label = NULL,
          choices = list(
            "Geological Sciences (WE 1)" = "WE 1",
            "Geographical Sciences (WE 2)" = "WE 2",
            "Meteorology (WE 3)" = "WE 3"
          ),
          selected = c("WE 1", "WE 2", "WE 3")
        )
      ),
      (column(
        3,
        checkboxInput(ns("checkbox_perc"),
          label = "Display in %", value = FALSE
        )
      ))
    ),
    fluidRow(
      column(
        8,
        wellPanel(
          style = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
          metric_box(
            title = "Journal articles with Open Data",
            value = paste0(round(length((OD_data)[["OD_perc"]]), 0), "%"),
            value_text = paste0("of 1722 screened articles shared Open Data over the years 2016-2024"),
            plot = plotlyOutput(ns("plot_open_data_total_articles"), height = "400px"),
            info_id = "infoOD_total",
            info_title = "Open Data by Article",
            info_text = OD_total_text
          )
        )
      ),
      column(
        4, includeMarkdown("texts/text_geo_OD_journal_articles.md")
      )
    )
  )
}
moduleUI_open_data <- function(id) {
  ns <- NS(id)

  wellPanel(
    style = "padding-top: 10px; padding-bottom: 0px;",
    fluidRow(column(
      8,
      h2(strong("Distribution of Open Data by repository type"), align = "left"),
    )),
    # fluidRow(
    #   column(
    #     3,
    #     sliderInput(
    #       ns("year"),
    #       label = NULL,
    #       min = 2016,
    #       max = 2024,
    #       value = c(2016, 2024),
    #       sep = "",
    #       ticks = FALSE,
    #       round = TRUE
    #     )
    #   ),
    #   column(3,
    #          checkboxGroupInput(
    #            ns("select_inst"),
    #            label = NULL,
    #            choices = list(
    #              "Geological Sciences (WE 1)" = "WE 1",
    #              "Geographical Sciences (WE 2)" = "WE 2",
    #              "Meteorology (WE 3)" = "WE 3"),
    #            selected = c("WE 2", "WE 1", "WE 3")
    #          )
    #
    #   )
    # ),
    fluidRow(
      column(
        8,
        wellPanel(
          style = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
          metric_box(
            title = "Open Data mentioned in articles' full texts by repository type",
            value = paste0(round(OD_repo_perc$perc[1]), "%"),
            value_text = paste0("of open datasets are shared through general-purpose repositories"),
            plot = plotlyOutput(ns("plot_od_treemap"), height = "400px"),
            info_id = "infoOD_repos",
            info_title = "Open Data Repositories",
            info_text = OD_repo_text
          )
        )
      ),
      column(
        4, includeMarkdown("texts/text_geo_OD_repositories.md")
      )
    )
  )
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Server module ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

moduleServer_journal <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$plot_journal_oa <- renderPlotly({
        data <- data_oa_journals(total_perc = input$checkbox_perc)
        plot_oa_journals(data,
          min = input$year[1],
          max = input$year[2],
          total_perc = input$checkbox_perc,
          institut = input$select_inst,
          color_seq = c("darkred", "darkblue", "darkgreen")
        )
      })


      output$plot_journal_license <- renderPlotly({
        data_license <- data_journals_licenses(
          total_perc = input$checkbox_perc,
          institut = input$select_inst
        )
        plot_journals_licenses(data_license,
          min = input$year[1],
          max = input$year[2],
          total_perc = input$checkbox_perc,
          institut = input$select_inst
        )
      })

      output$plot_other_oa <- renderPlotly({
        req(input$genre_dropdown)
        data_non_oa <- data_oa_other(
          data = data_other,
          min = input$year[1],
          max = input$year[2],
          institut = input$select_institut,
          total_perc = input$checkbox_perc
        )
        filtered_data <- data_non_oa |>
          filter(genre_manual == input$genre_dropdown)

        plot_oa_other(filtered_data,
          min = input$year[1],
          max = input$year[2],
          total_perc = input$checkbox_perc,
          institut = input$select_institut
        )
      })

      output$plot_other_pid <- renderPlotly({
        data_pid <- data_other_pid(
          institut = input$select_inst,
          total_perc = input$checkbox_perc
        )
        plot_other_pid(data_pid,
          min = input$year[1],
          max = input$year[2],
          total_perc = input$checkbox_perc,
          institut = input$select_inst
        )
      })

      output$plot_open_data_total_articles <- renderPlotly({
        data_open_data_articles <- data_od_total_articles(
          institut = input$select_inst,
          total_perc = input$checkbox_perc
        )
        plot_od_total_articles(data_open_data_articles,
          total_perc = input$checkbox_perc,
          institut = input$select_inst,
          min = input$year[1],
          max = input$year[2]
        )
      })
      output$plot_od_treemap <- renderPlotly({
        data_od_treemap <- data_od_treemap_repos( # institut = input$select_inst,
          # total_perc = input$checkbox_perc
        )
        plot_od_treemap_repos(
          data_od_treemap # ,
          # select_repository = input$select_repository
          # total_perc = input$checkbox_perc,
          # institut = input$select_inst,
          # min = input$year[1],
          # max = input$year[2]
        )
      })
    }
  )
}
