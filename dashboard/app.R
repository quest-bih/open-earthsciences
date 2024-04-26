#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Dashboard for Geo Sciences ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load libraries ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(plotly)
library(ggplot2)
library(RColorBrewer)
library(shiny)
library(tidyverse)
library(readxl)
library(shinyBS)
library(shinyjs)
library(shinythemes)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load functions and modules ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("functions.R", encoding = "UTF-8")
source("modules.R", encoding = "UTF-8")
source("datasets_panel.R", encoding = "UTF-8")
source("texts/impressum.R",  encoding = "UTF-8")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# UI ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ui elemets

methods_panel <- function(title, what_text, how_text, limit_text, style = "default")
{
  bsCollapsePanel(title,
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
  fluidPage(
    #headerPanel("Header"),
  tagList(
    tags$head(tags$script(type="text/javascript", src = "code.js")),
    tags$style(
      HTML(".navbar-logo { max-height: 100px; width: auto; height: auto; }")
    ),
    navbarPage(
      "Open Science for Earth Sciences FU",
      theme = shinytheme("flatly"),
      id = "navbarTabs",
      tags$style(
        HTML(
          ".checkbox-inline { 
                    margin-left: 0px;
                    margin-right: 10px;
          }
         .checkbox-inline+.checkbox-inline {
                    margin-left: 0px;
                    margin-right: 10px;
          }
        "
        )
      ),
      tabPanel(
        "Open Access and Licenses",
        value = "tabOA",
        wellPanel(br(),
                  fluidRow(
                    column(
                      8,
                      h1(
                        style = "margin-left:0cm",
                        strong("Open Science Dashboard of Earth Sciences,"),
                        br(),
                        strong("Freie Universität Berlin"),
                        align = "left"
                      ),
                      h4(
                        style = "margin-left:0cm",
                        HTML(
                          'Open Science or Open Research encompasses many practices 
                          that are becoming increasingly relevant in the scientific 
                          community. They aim to make the scientific workflow, its 
                          sources, and results openly accessible, transparent, and 
                          reusable in the long term.'
                        )
                      ),
                      h4(
                        style = "margin-left:0cm",
                        HTML('This dashboard provides an overview of several Open Science 
                        metrics in the <a href="https://www.geo.fu-berlin.de/en/index.html/">Department of Earth Sciences at Freie Universität 
                        Berlin</a>. More metrics may be added in the future.'
                      )),
                      checkboxInput("checkbox_colorblind",
                                    "Click for colorblind-friendly charts",
                                    value = FALSE),
                      #plotlyOutput("plot_OA_journal"),
                      br()
                    ),
                    column(
                      4,
                      br(),
                      br(),
                      actionButton('buttonMethods',
                                   'Methods'),
                      actionButton('buttonDatasets',
                                   'Data sets'), #style = "color: white; background-color: #aa1c7d;",
                      br()
                    )
                  ),),
        
        br(),
        moduleUI_journal_OA("plot_OA_journal"),
        moduleUI_journal_license("license_journal"),
        moduleUI_non_journal_OA("plot_OA_non_journal"),
        moduleUI_non_journal_pid("non_journal_pid"),
        br(),
        br(),
        bsCollapsePanel(
          strong("Impressum"),
          impressum_text,
          style = "default"
        ),
        bsCollapsePanel(
          strong("Datenschutz"),
          datenschutz_text,
          style = "default"
        )
      ),
      # tabPanel(
      #   "Open Data and Open Code",
      #   value = "tabOpenData",
      #   wellPanel(br(),
      #             fluidRow(
      #               column(
      #                 12,
      #                 h2(
      #                   style = "margin-left:0cm",
      #                   strong("Open Data and Open Code Shares at Earth Sciences of the Freie Universität Berlin"),
      #                   align = "left"
      #                 ),
      #                 h4(
      #                   style = "margin-left:0cm",
      #                   HTML(
      #                     'Open Data and Open Code practices have become common 
      #                     practices, in addition to Open Access, for fostering good scientific
      #                     practice, transparency and data reuse.'
      #                   )
      #                 ),
      #                 h4(
      #                   style = "margin-left:0cm",
      #                   HTML(
      #                     'A broad definition of Open Data is presented, for example, by the 
      #                     <a href="https://www.openaire.eu/what-is-open-research-data">OpenAIRE
      #                     community</a>. For precise validation regarding openness of 
      #                     data (and code), as well as for the monitoring purposes, a 
      #                     list of strict criteria is neccessery. In this dashboard,
      #                     <a href="https://zenodo.org/record/6651941">Open Data criteria</a>
      #                     that were previously defined were used for monitoring and 
      #                     incentivisation at Charité – Universitätsmedizin Berlin.'
      #                   
      #                   )
      #                 ),
      #                 h4(
      #                   style = "margin-left:0cm",
      #                   'The following plots show the destribution of Open Data 
      #                     and Open Code outputs in the Department of Earth Sciences 
      #                     at Freie Universität Berlin. The Open Data detection workflow
      #                     used here is similar to the workflow used for the biomedical articles
      #                     at the Charité – Universitätsmedizin Berlin (see Iarkaeva et al., 2023).'
      #                 ),
      #             
      #                 h4(
      #                   style = "margin-left:0cm",
      #                   'From a total of more than 840 available cases, 505 PDFs
      #                   were downloaded and validated using ODDPub text-mining tool 
      #                   developed for detecting open data and open code statements
      #                   in the full texts. The automated validation resulted in 79 
      #                   "hits" for Open Data and 12 "hits" for Open Code for all 3 Geoscience institutes.'
      #                 ),
      #                 br()
      #               )
      #             ),),
      #   
      #   br(),
      #  moduleUI_open_data("open_data"),
      #  moduleUI_open_data_bar("open_data_bar"),
      #  #moduleUI_open_data_line("open_data"),
      #  #moduleUI_open_data_pub("open_data_pub"),
      #  moduleUI_open_code_bar("open_code_bar"),
      #  moduleUI_open_code("open_code")
      # ),
      tabPanel(
        "Methods",
        value = "tabMethods",
        h1("Methods"),
        #h2("Publication search"),
        bsCollapse(id = "methodsPanels_PublicationSearch",
                   methods_panel("Publication search",
                                   'The assessed metrics are publication-based metrics. 
                                   The FU university bibliography provided this list 
                                   of publications, which was created by merging, 
                                   deduplicating and improving data.',
                                 '...',
                                 '...'
                   )),
        #h2("Open Access"),
        bsCollapse(id = "methodsPanels_OpenAccess",
                   methods_panel("Open Access",
                                
                                 'The open access metric measures the degree of openness 
                                 of the publications by researchers from the Department 
                                 of Earth Sciences, Freie Universität Berlin. 
                                 Open access publications are available to everyone 
                                 worldwide for free, helping to distribute research 
                                 results quickly and transparently.',
                                 'The Earth Sciences Library first created a list of 
                                 journal article publications by FU Earth Science 
                                 researchers, then queried the Unpaywall database via 
                                 its API to obtain information on the Open Access (OA) 
                                 status of those publications. Unpaywall is today the 
                                 most comprehensive database of open access information 
                                 on research articles. It has been queried using Digital 
                                 Object Identifiers (DOIs) for each of the publications. 
                                 There are different OA statuses a publication can have, 
                                 which are color-coded. Gold OA denotes publication in a 
                                 pure OA journal. Green OA denotes a freely available 
                                 repository version. Hybrid OA denotes an OA publication 
                                 in a paywalled journal where the author(s) have opted to 
                                 pay for their article to be open access. Bronze OA denotes 
                                 a publication which is freely available on the publisher 
                                 website, but without a clear open license enabling re-use: 
                                 this includes articles in a paywalled journal which have 
                                 been made free to read but access might be withdrawn at any 
                                 time. Thus we only consider the categories gold, green 
                                 and hybrid to be true open access here. As one publication 
                                 can have several OA versions (e.g. a gold version in an OA 
                                 journal as well as a green version in a repository), a
                                 hierarchy is used so that each publication is assigned 
                                 the OA category with the highest level of openness. 
                                 The standard hierarchy used here is gold - hybrid - 
                                 green (journal version before repository version, 
                                 excepting bronze). We group the results from Unpaywall 
                                 by OA status and publication year. One important 
                                 point for OA status is that it may change over time: 
                                 the OA percentage is not a fixed number. Repository 
                                 versions (green OA) are often made available after 
                                 a delay, such that the OA percentage for a given 
                                 year typically rises retrospectively. Thus the point 
                                 in time of data retrieval is important for understanding 
                                 the OA percentage. The current OA status data were 
                                 retrieved in November 2022.',
                                 'Unpaywall only stores information for publications 
                                 that have a DOI assigned by Crossref. Articles without 
                                 a Crossref DOI have to be excluded from the OA analysis. 
                                 However, these publications will be evaluated manually.'
                   )),
        #h2("Licenses"),
        bsCollapse(id = "methodsPanels_Licenses",
                   methods_panel("Licenses",
                                 '...',
                                 '...',
                                 '...'
                   )),
        #h2("Persistent Identifiers"),
        bsCollapse(id = "methodsPanels_PIDs",
                   methods_panel("Persistent Identifiers",
                                 '...',
                                 '...',
                                 '...'
                   ))
      #   h2("Open Data"),
      #   bsCollapse(id = "methodsPanels_OpenData",
      #              methods_panel("Open Data",
      #                            'Open Data collection ...',
      #                            '...',
      #                            '...'
      #              )),
       ),
      tabPanel(
        "Datasets",
        value = "tabDatasets",
        h1("Datasets"),
        h4("The following table contains the dataset underlying the numbers and plots
              shown for the metrics included in this Shiny app."),
        br(),
        bsCollapse(id = "datasetPanels_PublicationDataset",
                   bsCollapsePanel("Journal publications",
        #wellPanel(title = "Earth Sciences publications dataset",
                                   DT::dataTableOutput("data_table_articles"),
                                   style = "default")),
        bsCollapse(id = "datasetPanels_OtherDataset",
                   bsCollapsePanel("Non-journal research outputs",
                                   #wellPanel(title = "Earth Sciences publications dataset",
                                   DT::dataTableOutput("data_table_other"),
                                   style = "default")),
      ),
      tabPanel(
        "About", 
        value = "tabAbout",
        h1("About"),
        h4())
    )
  )
 )

# observeEvent(input$infoOA, {
#   updateTabsetPanel(session, "navbarTabs",
#                     selected = "tabMethods")
#   updateCollapse(session, "methodsPanels_OpenScience", open = "Open Access")
# })
# 
# observeEvent(input$buttonDatasets, {
#   updateTabsetPanel(session, "navbarTabs",
#                     selected = "tabDatasets")
# #   updateCollapse(session, "datasetPanels_PublicationDataset_geo",
# #                  open = "Open Science Earth Sciences data set")
# })
# 
# data_table_geo <- DT::renderDataTable({
#   make_datatable_2(geo_ressources)
# })
# 
# open_data_geo <- DT::renderDataTable({
#   make_datatable_2(data_other)
# })

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Server ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

server <- function(input, output, session)
{
  # # observe({
  # output$plot_OA_journal <- renderPlotly({
  #   if (input$checkbox_colorblind) {
  #     color_seq <- c("#440154", "#25858E", "#FDE725", "#AA493A", "#F1BA50", "#007265")  # Colorblind-friendly color scheme
  #     return(color_seq)
  #   } else {
  #     color_seq <- c("#AA493A", "#F1BA50", "#007265", "#AA493A", "#F1BA50", "#007265")  # Default color scheme
  #     return(color_seq)
  #     #return(brewer.pal(7,"Greens"))
  #   }
  #   plot_oa_journals(data, min, max, total_perc, institut, color_seq) # input$checkbox_colorblind) #data, min, max, total_perc, institut, color_seq
  # })
  # # })
  
  moduleServer_journal("plot_OA_journal")
  moduleServer_journal("license_journal")
  moduleServer_journal("plot_OA_non_journal")
  moduleServer_journal("non_journal_pid")
  moduleServer_journal("open_data")
  moduleServer_journal("open_data_horiz")
  moduleServer_journal("open_data_bar")
  #moduleServer_journal("open_data_pub")
  moduleServer_journal("open_code_bar")
  moduleServer_journal("open_code")
  
  
  #actionButton to switch tabs
  observeEvent(input$buttonMethods, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabMethods")
  })
  
  observeEvent(input$buttonDatasets, {
    updateTabsetPanel(session, "navbarTabs",
                      selected = "tabDatasets")
    updateCollapse(session, "datasetPanels_PublicationDataset",
                   open = "Earth Sciences publications dataset")
    updateCollapse(session, "datasetPanels_OtherDataset",
                   open = "Earth Sciences non-journal research outputs dataset")
  })
  
  # color_scheme <- reactive({
  #   if (input$checkbox_colorblind) {
  #     color_seq <- c("#440154", "#25858E", "#FDE725")  # Colorblind-friendly color scheme
  #   } else {
  #     color_seq <- c("#AA493A", "#F1BA50", "#007265")  # Default color scheme
  #     #return(brewer.pal(7,"Greens"))
  #   }
  # })
  

  
  
  # data_OA_journal <- geo_ressources |>
  #   data_oa_journals(total_perc = TRUE)
  # 
  # output$plot_OA_journal <- renderPlotly({
  #   
  #   if(input$checkbox_colorblind){
  #     color <- c("#440154", "#25858E", "#FDE725") #color_seq
  #   } else {
  #     color <- c("#AA493A", "#F1BA50", "#007265")
  #   }
  #   
  #   plot_oa_journals(color = color)
  # })
  # 
  
  output$data_table_articles <- DT::renderDataTable({
    make_datatable(geo_ressources)
  })
  
  output$data_table_other <- DT::renderDataTable({
    make_datatable(data_other)
  })
  
  # URI routing
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
