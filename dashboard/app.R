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

# ui_elemets
methods_panel <- function(title, what_text, how_text, limit_text, style = "default")
{
  bsCollapsePanel(title, # JT test remove strong strong(title), in order for updateCollapse() to work
                  strong("What it measures:"),
                  br(),
                  p(what_text),
                  strong("How it was calculated:"),
                  br(),
                  p(how_text),
                  strong("Limitations:"),
                  br(),
                  p(limit_text),
                  style = style)
}

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
                        strong("Open Science Dashboard Geowissenschaften"),
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
                        'Dieses Dashboard gibt einen Überblick über mehrere Metriken offener Forschung im Fachbereich Geowissenschaften an der Freien Universität Berlin. Dieses Dashboard ist ein Pilotprojekt, das sich noch in der Entwicklung befindet. In Zukunft können weitere Metriken hinzugefügt werden.'
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
        h2("Publication search"),
        bsCollapse(id = "methodsPanels_PublicationSearch",
                   bsCollapsePanel("Publication search",
                                   'The assessed metrics are publication-based metrics. The FU university bibliography provided this list of publications, which was created by merging, deduplicating and improving data.'
                   )),
        h2("Open Science"),
        bsCollapse(id = "methodsPanels_OpenScience",
                   methods_panel("Open Access",
                                
                                 'The open access metric measures the degree of openness of the publications by researchers from the Department of Earth Sciences, Free University Berlin. Open access publications are available to everyone worldwide for free, helping to distribute research results quickly and transparently.',
                                 'The Earth Sciences Library first created a list of journal article publications by FU Earth Science researchers, then queried the Unpaywall database via its API to obtain information on the Open Access (OA) status of those publications. Unpaywall is today the most comprehensive database of open access information on research articles. It has been queried using Digital Object Identifiers (DOIs) for each of the publications. There are different OA statuses a publication can have, which are color-coded. Gold OA denotes publication in a pure OA journal. Green OA denotes a freely available repository version. Hybrid OA denotes an OA publication in a paywalled journal where the author(s) have opted to pay for their article to be open access. Bronze OA denotes a publication which is freely available on the publisher website, but without a clear open license enabling re-use: this includes articles in a paywalled journal which have been made free to read but access might be withdrawn at any time. Thus we only consider the categories gold, green and hybrid to be true open access here. As one publication can have several OA versions (e.g. a gold version in an OA journal as well as a green version in a repository), a hierarchy is used so that each publication is assigned the OA category with the highest level of openness. The standard hierarchy used here is gold - hybrid - green (journal version before repository version, excepting bronze). We group the results from Unpaywall by OA status and publication year. One important point for OA status is that it may change over time: the OA percentage is not a fixed number. Repository versions (green OA) are often made available after a delay, such that the OA percentage for a given year typically rises retrospectively. Thus the point in time of data retrieval is important for understanding the OA percentage. The current OA status data were retrieved in November 2022.',
                                 'Unpaywall only stores information for publications that have a DOI assigned by Crossref. Articles without a Crossref DOI have to be excluded from the OA analysis. However, these publications will be evaluated manually.'
                   )
        )
      ),
      tabPanel(
        "Datensätze",
        value = "tabDatasets",
        h1("Datensätze"),
        h4("The following tables contain the datasets underlying the numbers and plots
              shown for the metrics included in this Shiny app."),
        br(),
        bsCollapse(id = "datasetPanels_PublicationDataset_geo",
                   bsCollapsePanel("Publication dataset",
                                   DT::dataTableOutput("data_table_geo"),
                                   style = "default")),
      ),
      tabPanel("Über", value = "tabAbout",
               h1("Über"))
    )
  )

observeEvent(input$buttonDatasetBSS, {
  updateTabsetPanel(session, "navbarTabs",
                    selected = "tabDatasets")
  updateCollapse(session, "datasetPanels_PublicationDataset_geo",
                 open = "Open Science Geowissenschaften Datensatz")
})

data_table_geo <- DT::renderDataTable({
  make_datatable(geo_ressources)
})

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
