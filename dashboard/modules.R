#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Modules for dashboard ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Open access status of journals ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# UI modules ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

moduleUI_journal <- function(id) {
  ns <- NS(id)
  
  wellPanel(
    style = "padding-top: 10px; padding-bottom: 0px;",
    fluidRow(column(8,
                    h2(strong("Open-Access-Status von Journal-Artikeln"), align = "left"),
    )),
    fluidRow(column(
      3,
        sliderInput(
          ns("year"),
          #label = "Jahr",
          label = NULL,
          #inputId = "year",
          min = 2016,
          max = 2021,
          value = c(2016, 2021),
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
                 "Geologische Wissenschaften (WE 1)" = "WE 1", 
                 "Geographische Wissenschaften (WE 2)" = "WE 2", 
                 "Meteorologie (WE 3)" = "WE 3"),
               selected = c("WE 1", "WE 2", "WE 3")
             )
             
             ),
      (column(3,
              checkboxInput(ns("checkbox_perc"), label = "Anzeige in %", value = FALSE)
      ))
    ),
    fluidRow(column(
      8,
      wellPanel(
        style = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
        # fluidRow(column(12, align = "left", h4(strong("How important do you think the following goals should be in the science system? – Regarding these goals, how much do you feel a pressure of expectations in your scientific work? – How do you prioritize these goals in your own work?"
        # )))),
      #  h1(style = "color: #aa1c7d;text-align:left;font-size:40px;", "25 %"), #textOutput(ns("module_number_prio"))
        h4(style = "color: #aa1c7d;text-align:left;font-size:20px;", "Open-Access-Status von Journal-Artikeln nach Jahr und Institut"), #textOutput(ns("module_text"))
        plotlyOutput(ns("plot_journal_oa"), height = "400px")
      )
    ),
    column(
      4, includeMarkdown("texts/test.md")
    ))
  )
}

moduleUI_journal_license <- function(id) {
  ns <- NS(id)
  
  wellPanel(style = "padding-top: 10px; padding-bottom: 0px;",
            fluidRow(column(8,
                            h2(
                              strong("Lizenzen von Journal-Artikeln"), align = "left"
                            ),)),
            fluidRow(column(
              3,
              checkboxInput(
                ns("checkbox_perc"),
                label = "Anzeige in %",
                value = FALSE
              )
            )),
            fluidRow(column(
              8,
              wellPanel(
                style = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
                # fluidRow(column(12, align = "left", h4(strong("How important do you think the following goals should be in the science system? – Regarding these goals, how much do you feel a pressure of expectations in your scientific work? – How do you prioritize these goals in your own work?"
                # )))),
                #  h1(style = "color: #aa1c7d;text-align:left;font-size:40px;", "25 %"), #textOutput(ns("module_number_prio"))
                h4(style = "color: #aa1c7d;text-align:left;font-size:20px;", "Lizenzen von Journal-Artikeln (alle Institute, 2016-2021)"),
                #textOutput(ns("module_text"))
                plotlyOutput(ns("plot_journal_license"), height = "400px")
              )
            ),
            column(4, includeMarkdown("texts/test.md"))))
}


moduleUI_other <- function(id) {
  ns <- NS(id)
  
  wellPanel(
    style = "padding-top: 10px; padding-bottom: 0px;",
    fluidRow(column(9,
                    h2(strong("Open-Access-Status von anderen Ressourcen"), align = "left"),
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
    #     max = 2021,
    #     value = c(2016, 2021),
    #     sep = "",
    #     ticks = FALSE,
    #     round = TRUE
    #   )
    # ),
    # column(3,
    #        selectInput(
    #          ns("select_inst"),
    #          label = NULL,
    #          choices = list(
    #            "Geologische Wissenschaften (WE 1)" = "WE 1", 
    #            "Geographische Wissenschaften (WE 2)" = "WE 2", 
    #            "Meteorologie (WE 3)" = "WE 3"),
    #          selected = c("WE 1", "WE 2", "WE 3"),
    #          multiple = TRUE,
    #          selectize = TRUE,
    #          width = NULL,
    #          size = NULL
    #        )
    #        
    # ),
    (column(3,
            checkboxInput(ns("checkbox_perc"), label = "Anzeige in %", value = FALSE)
    ))
    ),
    fluidRow(column(
      10,
      wellPanel(
        style = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
        # fluidRow(column(12, align = "left", h4(strong("How important do you think the following goals should be in the science system? – Regarding these goals, how much do you feel a pressure of expectations in your scientific work? – How do you prioritize these goals in your own work?"
        # )))),
        #  h1(style = "color: #aa1c7d;text-align:left;font-size:40px;", "25 %"), #textOutput(ns("module_number_prio"))
        h4(style = "color: #aa1c7d;text-align:left;font-size:20px;", "Open-Access-Status von anderen Ressourcen nach Jahr und Kategorie für Geographische Wissenschaften (WE 2)"), #textOutput(ns("module_text"))
        plotlyOutput(ns("plot_other_oa"), height = "400px")
      )
    ),
    column(
      10, includeMarkdown("texts/test.md")
    ))
  )
}

moduleUI_other_license <- function(id) {
  ns <- NS(id)
  
  wellPanel(
    style = "padding-top: 10px; padding-bottom: 0px;",
    fluidRow(column(8,
                    h2(strong("Lizenzen von anderen Ressourcen"), align = "left"),
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
      #     max = 2021,
      #     value = c(2016, 2021),
      #     sep = "",
      #     ticks = FALSE,
      #     round = TRUE
      #   )
      # ),
      # column(3,
      #        selectInput(
      #          ns("select_inst"),
      #          label = NULL,
      #          choices = list(
      #            "Geologische Wissenschaften (WE 1)" = "WE 1", 
      #            "Geographische Wissenschaften (WE 2)" = "WE 2", 
      #            "Meteorologie (WE 3)" = "WE 3"),
      #          selected = c("WE 1", "WE 2", "WE 3"),
      #          multiple = TRUE,
      #          selectize = TRUE,
      #          width = NULL,
      #          size = NULL
      #        )
      #        
      # ),
      (column(3,
              checkboxInput(ns("checkbox_perc"), label = "Anzeige in %", value = FALSE)
      ))
    ),
    fluidRow(column(
      8,
      wellPanel(
        style = "padding-top: 0px; padding-bottom: 0px; background-color:#DCE3E5",
        # fluidRow(column(12, align = "left", h4(strong("How important do you think the following goals should be in the science system? – Regarding these goals, how much do you feel a pressure of expectations in your scientific work? – How do you prioritize these goals in your own work?"
        # )))),
        #  h1(style = "color: #aa1c7d;text-align:left;font-size:40px;", "25 %"), #textOutput(ns("module_number_prio"))
        h4(style = "color: #aa1c7d;text-align:left;font-size:20px;", "Lizenzen von anderen Ressourcen (Geographische Wissenschaften (WE 2), 2016-2021)"), #textOutput(ns("module_text"))
        plotlyOutput(ns("plot_other_license"), height = "400px")
      )
    ),
    column(
      4, includeMarkdown("texts/test.md")
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
        plot_journals_licenses(total_perc = input$checkbox_perc)
      })
      
      
      output$plot_other_oa <- renderPlotly({
        
        data <- data_other()
        plot_oa_other(data,
                      total_perc = input$checkbox_perc)
        
      })
      
      output$plot_other_license <- renderPlotly({
        
        plot_other_licenses(data,
                            total_perc = input$checkbox_perc)
        
      })
      
      
      
      
    }
    )
}


# Test multicategorical axis and stacked bar chart

# Create dataframe
# x <- list(c(2019, 2019, 2020, 2020),
#           c("WE1", "WE2", "WE1", "WE2")
# )
# 
# x_1 <- c(2019, 2019, 2020, 2020)
# x_2 <- c("WE1", "WE2", "WE1", "WE2")
# y_1 <- c(1, 3, 5, 5)
# y_2 <- c(2, 2, 4, 5)
# 
# df <- data.frame(x_1, x_2, y_1, y_2)


# df |>
# # filter(x_1 == 2019) |>
# plot_ly(
#   x = ~ list(x_1, x_2)
# ) |>
# add_bars(y = ~ y_1,
#          marker = list(color = "green"),
#          name = "green"
#          ) |>
#   add_bars(y = ~ y_2,
#            marker = list(color = "gold"),
#            name = "gold") |>
#   layout(barmode = "relative")