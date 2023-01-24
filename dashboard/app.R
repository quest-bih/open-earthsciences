#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Dashboard for Geo Sciences ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load libraries ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(plotly)
library(shiny)
library(tidyverse)
library(readxl)
library(shinyBS)
#library(shinyjs)
library(shinythemes)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load functions and modules ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("functions.R", encoding = "UTF-8")
source("modules.R", encoding = "UTF-8")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# UI ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

ui <-
  tagList(
    # tags$head(tags$script(type="text/javascript", src = "code.js")),
    navbarPage(
      "",
      theme = shinytheme("flatly"),
      id = "navbarTabs",
      tabPanel(
        "Start page",
        value = "tabStart",
        wellPanel(br(),
                  fluidRow(
                    column(
                      8,
                      h1(
                        style = "margin-left:0cm",
                        strong("Open Access Dashboard Geowissenschaften"),
                        align = "left"
                      ),
                      h4(
                        style = "margin-left:0cm",
                        HTML(
                          'Das Konzept von Open Science umfasst viele Praktiken, die im wissenschaftlichen Alltag zunehmend relevant werden. Sie haben zum Ziel, den wissenschaftlichen Arbeitsprozess, seine Quellen und Ergebnisse langfristig offen zugänglich, nachvollziehbar und nachnutzbar zu machen.'
                        )
                      ),
                      h4(
                        style = "margin-left:0cm",
                        'Mit der Verbreitung offener Wissenschaft steigt auch der Bedarf nach einem Monitoring von Open-Science-Praktiken. Dies erlaubt einen breiten Blick auf die Aktivitäten, kann neue Anreize schaffen, Open Science umzusetzen und setzt Impulse für die Entwicklung von Policies oder Infrastrukturangeboten.'
                      ),
                      br()
                    ),
                    column(
                      4,
                      br(),
                      br(),
                      actionButton('buttonMethods',
                                   'Methodik'),
                      actionButton(#style = "color: white; background-color: #aa1c7d;",
                        'buttonDatasets',
                        'Datensätze'),
                      br()
                    )
                  ),),
        
        br(),
        moduleUI_journal("total"),
        moduleUI_journal_license("license"),
        moduleUI_other("other"),
        moduleUI_other_license("other_license"),
        br(),
        br(),
        bsCollapsePanel(
          strong("Impressum"),
          includeMarkdown("texts/test.md"),
          style = "default"
        ),
        bsCollapsePanel(
          strong("Datenschutz"),
          includeMarkdown("texts/test.md"),
          style = "default"
        )
      ),
      tabPanel(
        "Methodik",
        value = "tabMethods",
        h1("Methodik"),
        h4("Hier wird die Methodik beschrieben.")
      ),
      tabPanel(
        "Datensätze",
        value = "tabDatasets",
        h1("Datensätze"),
        h4("Hier werden die Datensätze angezeigt.")
      ),
      tabPanel("Über", value = "tabAbout",
               h1("Über"))
    )
  )


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Server ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

server <- function(input, output, session)
{
  
  moduleServer_journal("total")
  moduleServer_journal("license")
  moduleServer_journal("other")
  moduleServer_journal("other_license")
  
  #actionButton to switch tabs
  observeEvent(input$buttonMethods, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabMethods")
  })
  
  observeEvent(input$buttonDatasets, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabDatasets")
  })
  
  # URI routing
  # (see: https://stackoverflow.com/questions/71541259/uri-routing-with-shiny-router-and-navbarpage-in-a-r-shiny-app/71807248?noredirect=1#comment126924825_71807248)
  observeEvent(session$clientData$url_hash, {
    currentHash <- sub("#", "", session$clientData$url_hash)
    if(is.null(input$navbarTabs) || !is.null(currentHash) && currentHash != input$navbarTabs){
      freezeReactiveValue(input, "navbarTabs")
      updateTabsetPanel(session, "navbarTabs", selected = currentHash)
    }
  }, priority = 1)
  
  observeEvent(input$navbarTabs, {
    currentHash <- sub("#", "", session$clientData$url_hash)
    pushQueryString <- paste0("#", input$navbarTabs)
    if(is.null(currentHash) || currentHash != input$navbarTabs){
      freezeReactiveValue(input, "navbarTabs")
      updateQueryString(pushQueryString, mode = "push", session)
    }
  }, priority = 0)
  
}


# Run the application 
shinyApp(ui = ui, server = server)
