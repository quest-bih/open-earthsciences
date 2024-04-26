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
    title, value, value_text, plot, info_id, info_title, info_text, info_alignment = "right")
{
  
  wellPanel(style = style_resp, 
            #"height: 500px; overflow: scroll; padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5", 
            #padding-bottom: 0px;
            fluidRow(
              column(8, align="left", h4(style = "color: #aa1c7d;text-align:left;font-size:20px;", title)), 
              #, h4(strong(title))
              column(4, align="right",
                     h4(actionButton(inputId = info_id, label = "", icon = icon("circle-info"),
                                     class = "btn-primary", style='padding:1px')),
                     bsPopover(info_id, 
                               info_title, 
                               info_text,
                               info_alignment, 
                               options = list(container = "body")),
                     tags$style(".popover{width: 300px;}"))
            ),
            h1(style = "color: #aa1c7d;text-align:left;font-size:40px;", value),
            h4(style = "color: #aa1c7d;text-align:left;font-size:18px;", value_text),
            plot)
  
}

OA_text <- strwrap("Use the buttons to change time interval, institute(s) displayed, 
                   or to toggle the Y-axis between “%” or “number of articles”.
                   
                   The graphic is based on enriched data from the university 
                   bibliography of the FU Berlin.
                   
                   Click on any  Open Access name(s) to unselect/select them.") |>
  paste(collapse = " ")


OL_text <- strwrap("Use the buttons to change time interval, institute(s) displayed, 
                   or to toggle the Y-axis between “%” or “number of articles”. 
                   
                   The graphic is based on enriched data from the university 
                   bibliography of the FU Berlin.
                   
                   Click on any CC License status name(s) to unselect/select them.") |>
  paste(collapse = " ")

OA_text_non_j <- strwrap("Use the buttons to change time interval, institute(s) displayed, 
                   or to toggle the Y-axis between “%” or “number of articles”. 
                   
                   The graphic is based on enriched data from the university 
                   bibliography of the FU Berlin.
                   
                   Click on any Open Access status name(s) to unselect/select them.") |>
  paste(collapse = " ")

PID_text_non_j <- strwrap("Use the buttons to change time interval, institute(s) displayed, 
                   or to toggle the Y-axis between “%” or “number of articles”. 
                   
                   The graphic is based on enriched data from the university 
                   bibliography of the FU Berlin.
                   
                   Click on Has PID or No PID to unselect/select them.") |>
  paste(collapse = " ")



moduleUI_journal_OA <- function(id) {
  ns <- NS(id)
  
  wellPanel(
    style = "padding-top: 10px; padding-bottom: 0px;",
    fluidRow(
      column(8,
             h2(strong("Open Access status of journal articles"), align = "left"),
    )),
  
      
    fluidRow(
      column(
      3,
        sliderInput(
          ns("year"),
          label = NULL,
          min = 2016,
          max = 2022,
          value = c(2016, 2022),
          sep = "",
          ticks = FALSE,
          round = TRUE
        )
      ),
      column(3,
             checkboxGroupInput(
               ns("select_inst"),
               label = NULL,
               choices = list(
                 "Geological Sciences (WE 1)" = "WE 1", 
                 "Geographical Sciences (WE 2)" = "WE 2", 
                 "Meteorology (WE 3)" = "WE 3"),
               selected = c("WE 1", "WE 2", "WE 3")
             )
             
             ),
      (column(3,
              checkboxInput(ns("checkbox_perc"), label = "Display in %", value = FALSE)
      ))
    ),
    fluidRow(
      
      # fluidRow(
      #   column(col_width, metric_box(title = "Open Access",
      #                                value = paste(round((OA_data |> filter(year == show_year))[["OA_perc"]], 0), "%"),
      #                                value_text = paste0("of publications were open access in ", show_year),
      #                                plot = plotlyOutput('plot_OA', height = "300px"),
      #                                info_id = "infoOA",
      #                                info_title = "Open Access",
      #                                info_text = open_access_tooltip)),
      # 
      
      column(
      8,
      fluidRow(style = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
        # fluidRow(column(12, align = "left", h4(strong("How important do you think the following goals should be in the science system? – Regarding these goals, how much do you feel a pressure of expectations in your scientific work? – How do you prioritize these goals in your own work?"
        # )))),
      #  h1(style = "color: #aa1c7d;text-align:left;font-size:40px;", "25 %"), #textOutput(ns("module_number_prio"))
       # h4(style = "color: #aa1c7d;text-align:left;font-size:20px;", 
          # "Open Access status of journal articles by year and institute"
      # ), #textOutput(ns("module_text"))
        
        metric_box(title = "Open Access status of journal articles by year and institute",
                   value = paste(round((OA_data |> filter(year == show_year))[["OA_perc"]], 0), "%"),
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
    ))
  )
}


moduleUI_journal_license <- function(id) {
  ns <- NS(id)
  
  wellPanel(style = "padding-top: 10px; padding-bottom: 0px;",
            fluidRow(column(8,
                            h2(strong("Creative Commons licenses of journal articles"), align = "left"),
                            )),
            fluidRow(column(
              3,
              sliderInput(
                ns("year"),
                label = NULL,
                min = 2016,
                max = 2022, 
                value = c(2016, 2022),
                sep = "",
                ticks = FALSE,
                round = TRUE
              )
            ),
              column(3,
                     checkboxGroupInput(
                       ns("select_inst"),
                       inline = F,
                       label = NULL,
                       choices = list(
                         "Geological Sciences (WE 1)" = "WE 1", 
                         "Geographical Sciences (WE 2)" = "WE 2", 
                         "Meteorology (WE 3)" = "WE 3"),
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
            )),
            fluidRow(column(
              8,
              wellPanel(
                style = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
                metric_box(title = "Open licenses of journal articles (all institutes, 2016-2022)",
                           value = paste(round((OL_data |> filter(year == show_year))[["OL_perc"]], 0), "%"),
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
  
  wellPanel(
    style = "padding-top: 10px; padding-bottom: 0px;",
    fluidRow(column(8,
                    h2(strong("Open Access status of non-journal-article outputs"), align = "left"),
    )),
    fluidRow(
    column(
      3,
      sliderInput(
        ns("year"),
        label = NULL,
        min = 2016,
        max = 2022,
        value = c(2016, 2022),
        sep = "",
        ticks = FALSE,
        round = TRUE
      )
    ),
    # column(3,
    #        checkboxGroupInput(
    #          ns("select_institut"),
    #          label = NULL,
    #          choices = list(
    #            "Geological Sciences (WE 1)" = "WE 1", 
    #            "Geographical Sciences (WE 2)" = "WE 2", 
    #            "Meteorology (WE 3)" = "WE 3"),
    #          selected = c("WE 1", "WE 2", "WE 3")
    #        )
    # 
    # ),
    (column(3,
            checkboxInput(ns("checkbox_perc"), 
                          label = "Display in %", value = FALSE)
    ))
    ),
    fluidRow(column(
      8,
      wellPanel(
        style = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
        metric_box(title = "Open Access status of non-journal-article outputs by year and category", # and institute ((all institutes, 2016-2022)
                   #value = paste( "47 %"),
                   value = paste0(round(OA_other[["OA_perc_other"]], 0), "%"),  # filter(year == show_year))
                   value_text = paste0("of non-journal-article outputs were OA between 2016-2022"),
                   plot = plotlyOutput(ns("plot_other_oa"), height = "400px"),
                   info_id = "infoOA_alt",
                   info_title = "Open Access",
                   info_text = OA_text_non_j
        )
      )
    ),
    column(4, includeMarkdown("texts/text_geo_OA_other.md")
    ))
  )
}

moduleUI_non_journal_pid <- function(id) {
  ns <- NS(id)
  
  wellPanel(
    style = "padding-top: 10px; padding-bottom: 0px;",
    fluidRow(column(8,
                    h2(strong("Persistent identifiers of non-journal-article outputs"), align = "left"),
    )),
    fluidRow(
      column(
        3,
        sliderInput(
          ns("year"),
          label = NULL,
          min = 2016,
          max = 2022,
          value = c(2016, 2022),
          sep = "",
          ticks = FALSE,
          round = TRUE
        )
      ),
      column(3,
             checkboxGroupInput(
               ns("select_inst"),
               label = NULL,
               choices = list(
                 "Geological Sciences (WE 1)" = "WE 1", 
                 "Geographical Sciences (WE 2)" = "WE 2", 
                 "Meteorology (WE 3)" = "WE 3"),
               selected = c("WE 2", "WE 1", "WE 3")
             )),
      (column(3,
              checkboxInput(ns("checkbox_perc"), label = "Display in %", value = FALSE)
      ))
    ),
    fluidRow(column(
      8,
      wellPanel(
        style = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
        metric_box(title = "PIDs of non-journal-article outputs (all institutes, 2016-2022)",  # 273 of 822
                   #value = paste( "32 %"),
                   value = paste(round(PID_other[["PID_perc_other"]], 0), "%"), #  filter(year == show_year)
                   value_text = paste0("of non-journal-article outputs had PID between 2016 and 2022"), #, show_year),
                   plot = plotlyOutput(ns("plot_other_pid"), height = "400px"),
                   info_id = "infoOL_alt",
                   info_title = "PID",
                   info_text = PID_text_non_j
        )
      )
    ),
    column(
      4, includeMarkdown("texts/text_geo_pid_other.md")
    ))
  )
}

moduleUI_open_data <- function(id) {
  ns <- NS(id)
  
  wellPanel(
    style = "padding-top: 10px; padding-bottom: 0px;",
    fluidRow(column(8,
                    h2(strong("Open Data of journal articles"), align = "left"),
    )),
    fluidRow(
      column(
        3,
        sliderInput(
          ns("year"),
          label = NULL,
          min = 2016,
          max = 2022,
          value = c(2016, 2022),
          sep = "",
          ticks = FALSE,
          round = TRUE
        )
      ),
      column(3,
             checkboxGroupInput(
               ns("select_inst"),
               label = NULL,
               choices = list(
                 "Geological Sciences (WE 1)" = "WE 1", 
                 "Geographical Sciences (WE 2)" = "WE 2", 
                 "Meteorology (WE 3)" = "WE 3"),
               selected = c("WE 2", "WE 1", "WE 3")
               # multiple = TRUE,
               # selectize = TRUE,
               # width = NULL,
               # size = NULL
             )
             
      ),
      (column(3,
              checkboxInput(ns("checkbox_perc"),
                            label = "Display in %", value = FALSE)
      ))
    ),
    fluidRow(column(
      8,
      wellPanel(
        style = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
        # fluidRow(column(12, align = "left", h4(strong("How important do you think the following goals should be in the science system? – Regarding these goals, how much do you feel a pressure of expectations in your scientific work? – How do you prioritize these goals in your own work?"
        # )))),
        #  h1(style = "color: #aa1c7d;text-align:left;font-size:40px;", "25 %"), #textOutput(ns("module_number_prio"))
        h4(style = "color: #aa1c7d;text-align:left;font-size:20px;",
           "Open Data of journal articles by type of repository (all institutes, 2016-2022)"), #textOutput(ns("module_text"))
        plotlyOutput(ns("plot_open_data"), height = "400px")
      )
    ),
    column(
      4, includeMarkdown("texts/test.md")
    ))
  )
}

moduleUI_open_data_horiz <- function(id) {
  ns <- NS(id)
  
  wellPanel(
    style = "padding-top: 10px; padding-bottom: 0px;",
    fluidRow(column(8,
                    h2(strong("Open Data of journal articles"), align = "left"),
    )),
    fluidRow(
      # column(
      #   3,
      #   sliderInput(
      #     ns("year"),
      #     #label = "Jahr",
      #     label = NULL,
      #     #inputId = "year",
      #     min = 2016,
      #     max = 2022,
      #     value = c(2016, 2022),
      #     sep = "",
      #     ticks = FALSE,
      #     round = TRUE
      #   )
      # ),
      column(3,
             checkboxGroupInput(
               ns("select_inst"),
               label = NULL,
               choices = list(
                 "Geological Sciences (WE 1)" = "WE 1",
                 "Geographical Sciences (WE 2)" = "WE 2",
                 "Meteorology (WE 3)" = "WE 3"),
               selected = c("WE 1", "WE 2", "WE 3")
               # multiple = TRUE,
               # selectize = TRUE,
               # width = NULL,
               # size = NULL
             )
             
      )),
    # (column(3,
    #         checkboxInput(ns("checkbox_perc"), 
    #                       label = "Display in %", value = FALSE)
    # ))
    # ),
    fluidRow(column(
      8,
      wellPanel(
        style = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
        # fluidRow(column(12, align = "left", h4(strong("How important do you think the following goals should be in the science system? – Regarding these goals, how much do you feel a pressure of expectations in your scientific work? – How do you prioritize these goals in your own work?"
        # )))),
        #  h1(style = "color: #aa1c7d;text-align:left;font-size:40px;", "25 %"), #textOutput(ns("module_number_prio"))
        h4(style = "color: #aa1c7d;text-align:left;font-size:20px;",
           "Open Data of journal articles by type of data repository (all institutes, 2016-2022)"), #textOutput(ns("module_text"))
        plotlyOutput(ns("plot_open_data_horiz"), height = "400px")
      )
    ),
    column(
      4, includeMarkdown("texts/open_data_2.md")
    ))
  )
}

moduleUI_open_data_bar <- function(id) {
  ns <- NS(id)
  
  wellPanel(
    style = "padding-top: 10px; padding-bottom: 0px;",
    fluidRow(column(8,
                    h2(strong("Open Data of journal articles"), align = "left"),
    )),
    fluidRow(
      # column(
      #   3,
      #   sliderInput(
      #     ns("year"),
      #     #label = "Jahr",
      #     label = NULL,
      #     #inputId = "year",
      #     min = 2016,
      #     max = 2022,
      #     value = c(2016, 2022),
      #     sep = "",
      #     ticks = FALSE,
      #     round = TRUE
      #   )
      # ),
      column(3,
             checkboxGroupInput(
               ns("select_inst"),
               label = NULL,
               choices = list(
                 "Geological Sciences (WE 1)" = "WE 1",
                 "Geographical Sciences (WE 2)" = "WE 2",
                 "Meteorology (WE 3)" = "WE 3"),
               selected = c("WE 1", "WE 2", "WE 3")
               # multiple = TRUE,
               # selectize = TRUE,
               # width = NULL,
               # size = NULL
             )

      )),
      # (column(3,
      #         checkboxInput(ns("checkbox_perc"), 
      #                       label = "Display in %", value = FALSE)
      # ))
 # ),
    fluidRow(column(
      8,
      wellPanel(
        style = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
        # fluidRow(column(12, align = "left", h4(strong("How important do you think the following goals should be in the science system? – Regarding these goals, how much do you feel a pressure of expectations in your scientific work? – How do you prioritize these goals in your own work?"
        # )))),
        #  h1(style = "color: #aa1c7d;text-align:left;font-size:40px;", "25 %"), #textOutput(ns("module_number_prio"))
        h4(style = "color: #aa1c7d;text-align:left;font-size:20px;",
           "Open Data of journal articles by type of data repository (all institutes, 2016-2022)"), #textOutput(ns("module_text"))
        plotlyOutput(ns("plot_open_data_bar"), height = "400px")
      )
    ),
    column(
      4, includeMarkdown("texts/open_data_2.md")
    ))
  )
}

moduleUI_open_data_pub <- function(id) {
  ns <- NS(id)
  
  wellPanel(
    style = "padding-top: 10px; padding-bottom: 0px;",
    fluidRow(column(8,
                    h2(strong("Open Data of journal articles"), align = "left"),
    )),
    fluidRow(
    #   column(
    #     3,
    #     sliderInput(
    #       ns("year"),
    #       #label = "Jahr",
    #       label = NULL,
    #       #inputId = "year",
    #       min = 2016,
    #       max = 2022,
    #       value = c(2016, 2022),
    #       sep = "",
    #       ticks = FALSE,
    #       round = TRUE
    #     )
    #   ),
      column(3,
             checkboxGroupInput(
               ns("select_inst"),
               label = NULL,
               choices = list(
                 "Geological Sciences (WE 1)" = "WE 1",
                 "Geographical Sciences (WE 2)" = "WE 2",
                 "Meteorology (WE 3)" = "WE 3"),
               selected = c("WE 2", "WE 1", "WE 3")
               # multiple = TRUE,
               # selectize = TRUE,
               # width = NULL,
               # size = NULL
             )
             
      ),
      # (column(3,
      #         checkboxInput(ns("checkbox_perc"), 
      #                       label = "Display in %", value = FALSE)
      # ))
    ),
    fluidRow(column(
      8,
      wellPanel(
        style = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
        # fluidRow(column(12, align = "left", h4(strong("How important do you think the following goals should be in the science system? – Regarding these goals, how much do you feel a pressure of expectations in your scientific work? – How do you prioritize these goals in your own work?"
        # )))),
        #  h1(style = "color: #aa1c7d;text-align:left;font-size:40px;", "25 %"), #textOutput(ns("module_number_prio"))
        h4(style = "color: #aa1c7d;text-align:left;font-size:20px;",
           "Open Data of journal articles by publisher (all institutes, 2016-2022"), 
           #textOutput(ns("module_text"))
        plotlyOutput(ns("plot_open_data_pub"), height = "400px")
      )
    ),
    column(
      4, includeMarkdown("texts/open_data_3.md")
    ))
  )
}

moduleUI_open_code_bar <- function(id) {
  ns <- NS(id)
  
  wellPanel(
    style = "padding-top: 10px; padding-bottom: 0px;",
    fluidRow(column(8,
                    h2(strong("Open Code of journal articles"), align = "left"),
    )),
    fluidRow(
    #   column(
    #     3,
    #     sliderInput(
    #       ns("year"),
    #       #label = "Jahr",
    #       label = NULL,
    #       #inputId = "year",
    #       min = 2016,
    #       max = 2022,
    #       value = c(2016, 2022),
    #       sep = "",
    #       ticks = FALSE,
    #       round = TRUE
    #     )
    #   ),
      column(3,
             checkboxGroupInput(
               ns("select_inst"),
               label = NULL,
               choices = list(
                 "Geological Sciences (WE 1)" = "WE 1",
                 "Geographical Sciences (WE 2)" = "WE 2",
                 "Meteorology (WE 3)" = "WE 3"),
               selected = c("WE 2", "WE 1", "WE 3")
               # multiple = TRUE,
               # selectize = TRUE,
               # width = NULL,
               # size = NULL
             )

      )),
    #   # (column(3,
    #   #         checkboxInput(ns("checkbox_perc"), 
    #   #                       label = "Display in %", value = FALSE)
    #   # ))
    # ),
    fluidRow(column(
      8,
      wellPanel(
        style = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
        # fluidRow(column(12, align = "left", h4(strong("How important do you think the following goals should be in the science system? – Regarding these goals, how much do you feel a pressure of expectations in your scientific work? – How do you prioritize these goals in your own work?"
        # )))),
        #  h1(style = "color: #aa1c7d;text-align:left;font-size:40px;", "25 %"), #textOutput(ns("module_number_prio"))
        h4(style = "color: #aa1c7d;text-align:left;font-size:20px;",
           "Open Code of journal articles (all institutes, 2016-2022)"), 
        #textOutput(ns("module_text"))
        plotlyOutput(ns("plot_open_code_bar"), height = "400px")
      )
    ),
    column(
      4, includeMarkdown("texts/open_code.md")
    ))
  )
}

moduleUI_open_code <- function(id) {
  ns <- NS(id)
  
  wellPanel(
    style = "padding-top: 10px; padding-bottom: 0px;",
    fluidRow(column(8,
                    h2(strong("Open Code of journal articles"), align = "left"),
    )),
    fluidRow(
    #   column(
    #     3,
    #     sliderInput(
    #       ns("year"),
    #       #label = "Jahr",
    #       label = NULL,
    #       #inputId = "year",
    #       min = 2016,
    #       max = 2022,
    #       value = c(2016, 2022),
    #       sep = "",
    #       ticks = FALSE,
    #       round = TRUE
    #     )
    #   ),
      column(3,
             checkboxGroupInput(
               ns("select_inst"),
               label = NULL,
               choices = list(
                 "Geological Sciences (WE 1)" = "WE 1",
                 "Geographical Sciences (WE 2)" = "WE 2",
                 "Meteorology (WE 3)" = "WE 3"),
               selected = c("WE 2", "WE 1", "WE 3")
               # multiple = TRUE,
               # selectize = TRUE,
               # width = NULL,
               # size = NULL
             )

      ) #,
    #   # (column(3,
    #   #         checkboxInput(ns("checkbox_perc"), 
    #   #                       label = "Display in %", value = FALSE)
    #   # ))
     ),
    fluidRow(column(
      8,
      wellPanel(
        style = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
        # fluidRow(column(12, align = "left", h4(strong("How important do you think the following goals should be in the science system? – Regarding these goals, how much do you feel a pressure of expectations in your scientific work? – How do you prioritize these goals in your own work?"
        # )))),
        #  h1(style = "color: #aa1c7d;text-align:left;font-size:40px;", "25 %"), #textOutput(ns("module_number_prio"))
        h4(style = "color: #aa1c7d;text-align:left;font-size:20px;",
           "Open Code of journal articles (all institutes, 2016-2022)"), #textOutput(ns("module_text"))
        plotlyOutput(ns("plot_open_code_line"), height = "400px")
      )
    ),
    column(
      4, includeMarkdown("texts/open_code.md")
    ))
  )
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Server module ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

moduleServer_journal <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
      
      output$plot_journal_oa <- renderPlotly({
      
        data <- data_oa_journals(total_perc = input$checkbox_perc)
        plot_oa_journals(data, 
                         min = input$year[1], 
                         max= input$year[2],
                         total_perc = input$checkbox_perc,
                         institut = input$select_inst)

        })

      
      output$plot_journal_license <- renderPlotly({

        data_license <- data_journals_licenses(total_perc = input$checkbox_perc,
                                               institut = input$select_inst)
        plot_journals_licenses(data_license,
                               min = input$year[1],
                               max= input$year[2],
                               total_perc = input$checkbox_perc,
                               institut = input$select_inst)

      })


      output$plot_other_oa <- renderPlotly({

        data_other <- data_oa_other(institut = input$select_inst, 
                                    total_perc = input$checkbox_perc)
          plot_oa_other(data_other,
                        min = input$year[1],
                        max = input$year[2],
                        total_perc = input$checkbox_perc,
                        institut = input$select_inst)

      })
      
      output$plot_other_pid <- renderPlotly({
        
        data_pid <- data_other_pid(institut = input$select_inst,
                                   total_perc = input$checkbox_perc)
        plot_other_pid(data_pid,
                       min = input$year[1],
                       max= input$year[2],
                       total_perc = input$checkbox_perc,
                       institut = input$select_inst)
        
      })
      
      output$plot_open_data <- renderPlotly({
        
        data_open_data <- data_od(total_perc = input$checkbox_perc)
        plot_od(data_open_data,
                total_perc = input$checkbox_perc
                #min = input$year[1],
                #max= input$year[2],
                #total_perc = input$checkbox_perc,
                #institut = input$select_inst
                )
      }
      )
      
      output$plot_open_data_line <- renderPlotly({
        
        data_open_data <- data_open_data_journals_line() #total_perc = input$checkbox_perc
        plot_open_data_journal_line(data_open_data,
                            min = input$year[1],
                            max= input$year[2],
                            #total_perc = input$checkbox_perc,
                            institut = input$select_inst)
    }
    )
      output$plot_open_data_horiz <- renderPlotly({

        data_open_data <- data_open_data_journals_horiz() #institut = input$select_inst, total_perc = input$checkbox_perc
        plot_open_data_journal_horiz(data_open_data,
                                   institut = input$select_inst)#,color,
        #min = input$year[1],
        #max= input$year[2],
        #total_perc = input$checkbox_perc)
      })
      
      output$plot_open_data_bar <- renderPlotly({
        
        data_open_data <- data_open_data_journals_bar() #institut = input$select_inst, total_perc = input$checkbox_perc
        plot_open_data_journal_bar(data_open_data, 
                                   color,
                                   institut = input$select_inst)#,
                               #min = input$year[1],
                               #max= input$year[2],
                               #total_perc = input$checkbox_perc)
      })
        
        # output$plot_open_data_pub <- renderPlotly({
        #   
        #   data_open_data <- data_open_data_journals_pub() #total_perc = input$checkbox_perc
        #   plot_open_data_journal_pub(data_open_data, 
        #                              color,
        #                              institut = input$select_inst)
        # })
        
        # output$plot_oddpub_data <- renderPlotly({
        #   if(input$checkbox_total_OS) {
        #     return(plot_OD_total(oddpub_plot_data, color_palette))
        #   } else {
        #     return(plot_OD_perc(oddpub_plot_data, color_palette, input$checkbox_zoom_OS))
        #   }
        # })
        
        output$plot_open_code_bar <- renderPlotly({
          
          data_open_code <- data_open_code_journals_bar() #total_perc = input$checkbox_perc
          plot_open_code_journal_bar(data_open_code, color,
          #min = input$year[1],
          #max= input$year[2],
          #total_perc = input$checkbox_perc,
          institut = input$select_inst)
        })
 
      output$plot_open_code_line <- renderPlotly({
        
        data_open_code <- data_open_code_journals_line() #total_perc = input$checkbox_perc
        plot_open_code_journal_line(data_open_code,
                               #min = input$year[1],
                               #max= input$year[2],
                               #total_perc = input$checkbox_perc,
                               institut = input$select_inst)
      }
      )
    }
)}