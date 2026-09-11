#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Dashboard for Geo Sciences ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load libraries ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(tidyverse)
library(plotly)
library(ggplot2)
library(RColorBrewer)
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinythemes)
library(shinylogs)
library(broom)
library(here)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load functions and modules ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("functions.R", encoding = "UTF-8")
source("modules.R", encoding = "UTF-8")
# source("datasets_panel.R", encoding = "UTF-8")
source("texts/impressum.R", encoding = "UTF-8")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# UI ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ui elemets

methods_panel <- function(title,
                          what_text,
                          how_text,
                          limit_text,
                          style = "default")
{
  bsCollapsePanel(
    title,
    strong("What it measures:"),
    br(),
    p(HTML(what_text)),
    strong("How it was calculated:"),
    br(),
    p(HTML(how_text)),
    # strong("Limitations:"),
    # br(),
    # p(limit_text),
    style = style
  )
}

ui <-
  fluidPage(tagList(
    tags$head(tags$script(type = "text/javascript", src = "code.js")),
    tags$style(
      HTML(
        "
      .navbar-logo {
      max-height: 100px; width: auto; height: auto;
      }

      .checkbox-inline {
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
    navbarPage(
      "",
      theme = shinytheme("flatly"),
      id = "navbarTabs",
      tabPanel(
        "Open Access and Licenses",
        value = "tabOA",
        wellPanel(br(), fluidRow(
          column(
            9,
            h2(
              style = "margin-left:0cm",
              strong(
                "Open Science Dashboard for the Department of Earth Sciences, Freie Universität Berlin"
              ),
              align = "left"
            ),
            h4(
              style = "margin-left:0cm",
              HTML(
                'Open Science or Open Research aims to make the scientific workflow,
                          its sources, and results openly accessible, transparent, and reusable
                          in the long term.'
              )
            ),
            h4(
              style = "margin-left:0cm",
              HTML(
                'This dashboard provides an overview of several Open Science-related
                        metrics in the Department of Earth Sciences at Freie Universität Berlin.
                        It may be used by researchers as well as the department administration to
                        evaluate the status and uptake trends and developments of Open Science practices.
                        Such practices are addressed in several university policy documents (see for
                        example the <a href="http://dx.doi.org/10.17169/refubium-31442">Open Access
                        Policy of FU Berlin</a>, the <a href="http://dx.doi.org/10.17169/refubium-32141">Research Data Policy of FU Berlin</a>
                        and the <a href="https://www.berlin-university-alliance.de/en/commitments/research-quality/open-science/Leitbild-fuer-OS/index.html">Mission Statement for Open Science at the Berlin University Alliance</a>). The dashboard is under continuous development and more metrics may
                        be added in the future.'
              )
            ),
            # checkboxInput("checkbox_colorblind",
            #               "Click for colorblind-friendly charts",
            #               value = FALSE),
            #plotlyOutput("plot_OA_journal"),
            br()
          ), column(
            3,
            div(
              style = "text-align: right;padding-top: 80px;padding-right: 20px;",
              actionButton('buttonMethods', 'Methods'),
              actionButton('buttonDatasets', 'Data sets', style = "margin-left: 12px;"),
              br()
            )
          )
        )),
        
        br(),
        moduleUI_journal_OA("plot_OA_journal"),
        moduleUI_journal_license("license_journal"),
        moduleUI_non_journal_OA("plot_OA_non_journal"),
        moduleUI_non_journal_pid("plot_non_journal_pid"),
        br(),
        br(),
        bsCollapsePanel(strong("Impressum"), impressum_text, style = "default"),
        bsCollapsePanel(strong("Datenschutz"), datenschutz_text, style = "default")
      ),
      tabPanel(
        "Open Data",
        value = "tabOpenData",
        wellPanel(br(), fluidRow(
          column(
            9,
            h2(
              style = "margin-left:0cm",
              strong(
                "Open Data underlying journal articles at the Department of Earth Sciences, Freie Universität Berlin"
              ),
              align = "left"
            ),
            h4(style = "margin-left:0cm", HTML('')),
            h4(
              style = "margin-left:0cm",
              HTML(
                'This dashboard section visualizes the absolute number and percentage of journal
                          articles with related Open Data from authors affiliated to the Department of
                          Earth Sciences, Freie Universität Berlin. Also shown are the (research data)
                          repositories where the open datasets are published. The dashboard is used to
                          monitor and increase visibility of Open Data sharing practices. In the long run,
                          it aims to support the future inclusion of researcher´s FAIR and open data
                          practices in the assessment of scientific output.'
              )
            ),
            h4(
              style = "margin-left:0cm",
              HTML(
                'We share <a href="https://www.openaire.eu/what-is-open-research-data">OpenAIRE</a>\'s
                        community definition that "<i>Open data are data that anyone can freely access,
                        use, modify, and share for any purpose (Open Knowledge Foundation,
                        <a href="https://opendefinition.org/od/2.1/">Open Definition 2.1</a>).
                        The goal of open data is to make information available so that it can be reused, combined
                        with other data, and built upon to generate new knowledge, products, or services. [...]
                        From a legal perspective, the data should be released under an open licence
                        (e.g. <a href="https://creativecommons.org/licenses/by/4.0/>CC BY 4.0</a>) or
                        dedicated to the public domain (e.g. <a href="https://creativecommons.org/publicdomain/zero/1.0/>CC0 1.0</a>)</i>".
                        (OpenAIRE, 2026).'
              )
            ),
            h4(
              style = "margin-left:0cm",
              HTML(
                'For precise validation regarding openness of data, as well as for the monitoring purposes,
                        a list of specific criteria was applied here, that were previously defined and used for
                        monitoring and incentivisation at Charité – Universitätsmedizin Berlin
                        (see <a href="https://zenodo.org/record/6651941">Open Data criteria</a>).'
              )
            ),
            h4(
              style = "margin-left:0cm",
              HTML(
                '<span style="font-size: 1.5rem">OpenAIRE. (2026). <i>What is Open Data</i>.
                          <a href="https://www.openaire.eu/should-my-research-data-be-open">https://www.openaire.eu/should-my-research-data-be-open</a> (accessed 03 Sept. 2026)</span>'
              )
            )
          ), column(
            3,
            div(
              style = "text-align: right;padding-top: 80px;padding-right: 20px;",
              actionButton('buttonMethods', 'Methods'),
              actionButton('buttonDatasets', 'Data sets', style = "margin-left: 12px;"),
              br()
            )
          )
        )),
        
        br(),
        moduleUI_open_data_articles("open_data_articles"),
        moduleUI_open_data("open_data_repo")
        
      ),
      tabPanel(
        "Methods",
        value = "tabMethods",
        h1("Methods"),
        bsCollapse(
          id = "methodsPanels_PublicationSearch",
          methods_panel(
            "Publication search",
            'The list of publications is available in the Freie
                                 Universität Berlin University Library (FU UB) from the
                                 Department of Earth Sciences:<br>
                                 - Institute of Geological Sciences (WE 1) <br>
                                 - Institute of Geographical Sciences (WE 2) <br>
                                 - Institute of Meteorology (WE 3)',
            'The list of publications with authors from the Department
                                 of Earth Sciences underwent deduplication and data
                                 enrichment processes which are described <a href="https://doi.org/10.5281/zenodo.10998219">here</a>.',
          )
        ),
        bsCollapse(
          id = "methodsPanels_OpenAccess",
          methods_panel(
            "Open Access",
            
            'The Open Access metric measures the share of open
                                 access publications by researchers from the Department
                                 of Earth Sciences, Freie Universität Berlin. What
                                 does Open Access mean? <a href="https://open-access.network/en/information/open-access-primers/what-does-open-access-mean">Read more</a>.',
            'The Freie Universität Berlin Library bibliography
                                 data provides the publication data used for the
                                 dashboard. The dataset is manually checked and
                                 enriched with open access status and copyright/reuse
                                 information. The exact open access status definitions
                                 used for this dashboard are described <a href="https://doi.org/10.5281/zenodo.10998219">here</a>. The open
                                 access status information for the 2016-2022 dataset
                                 was retrieved in November 2022, for the 2023
                                 dataset in September 2024, and for the 2024 dataset in January 2026.'
          )
        ),
        bsCollapse(
          id = "methodsPanels_Licenses",
          methods_panel(
            "Licenses",
            'The License metric measures the use of the different
                                 Creative Commons licenses for publications by researchers
                                 from the Department of Earth Sciences, Freie Universität
                                 Berlin. Open licenses provide users with certainty that
                                 the publication can be used and shared for a wide range
                                 of purposes. An open license builds upon copyright and
                                 is an explicit permission to use the publication in more
                                 ways than copyright would allow. ',
            'The license information was partially provided by the
                                 university library but had to be manually enriched. The
                                 licenses mostly included Creative Commons (CC) open licenses,
                                 which were counted per institute and year for journal articles.'
          )
        ),
        bsCollapse(
          id = "methodsPanels_PIDs",
          methods_panel(
            "Persistent Identifiers",
            'This metric shows the number of Persistent Identifiers (PIDs),
                                 e.g., DOIs, URNs and other, allocated to non-journal-article
                                 outputs. PIDs are essential for an open infrastructure because
                                 they provide a stable, long-term reference for digital objects,
                                 ensuring that researchers can reliably locate and cite these
                                 resources. PIDs help improve the discoverability, accessibility,
                                 and interoperability of academic outputs across various platforms
                                 and systems, contributing to more efficient knowledge dissemination
                                 and collaboration.',
            'Persistent Identifiers (PIDs) were partially included in the
                                 University Bibliography but had to be manually enriched. Each
                                 entry without a PID underwent individual research. In cases where
                                 no PID was found, the URL was included.'
          )
        ),
        bsCollapse(id = "methodsPanels_OD", methods_panel(
          "Open Data",
          HTML(
            'This metric shows the distribution of Open Data publications
                          associated with published research articles and provides an overview of
                          Open Data practices across the department.
                                 The screening for Open Data cases uses <a href="https://zenodo.org/record/6651941">Open Data criteria</a>,
                                 originally developed for monitoring and incentivization of Open Data
                        practices at Charité – Universitätsmedizin Berlin.
                        The detection workflow used here was adapted to Earth Sciences and follows an approach similar to that used for biomedical research articles
                        (see <a href="https://doi.org/10.1371/journal.pone.0302787">Iarkaeva et al., 2023</a>).'
          ),
          HTML(
            '<p>
                                 From a total of 2186 journal articles, 1722 PDFs have been accessed, downloaded and
                                 validated using <a href="https://github.com/quest-bih/oddpub">ODDPub text-mining tool</a>
                                 developed to detect formal Open Data or other evidence of Open Data related to the article
                                 in the full texts. Originally developed for the biomedical field, the ODDPub
                                 algorithm was adapted to detect repositories specific to Earth Sciences.
                                 </p>
                                 <ol style="margin: 15px 0 20px 0; padding-left: 25px; line-height: 1.6; ">
                                 <li style="margin-bottom: 8px;">
                                 Preparation step: Adjustment of the ODDPub algorithm to the Earth Sciences disciplinary field </li>
                                 <li style="margin-bottom: 8px;">
                                 Downloading full texts of the journal articles as PDFs and conversion into .txt files </li>
                                 <li style="margin-bottom: 8px;"> Application of the adjusted ODDPub algorithm </li>
                                 <li style="margin-bottom: 8px;"> Screening of the ODDPub output (only articles flagged
                                 as <i>"open data positive"</i>) in the Numbat screening tool by one or more raters </li>
                                 <li style="margin-bottom: 8px;"> Data cleaning </li> <li> Merging the validated results with
                                 the complete list of journal articles (<i>‘open data negative’</i> and not screened articles) </li>
                                 </ol><p>
                                 The automated validation (Step 3) resulted in 300 \'hits\' for Open Data for all Earth Science
                                 institutes.
                                 Manual validation (Step 4) of the ODDPub positive Open Data cases resulted in 154 articles (9%)
                                 having Open Data associated.
                                 The detailed methodology publication about data collection and validation for the Open Data metric
                                 included in the tab <b>Open data</b> will be published in <b>September 2026</b>.
                                 </p>'
          )
        )),
      ),
      tabPanel(
        "Data sets",
        value = "tabDatasets",
        h1("Data sets"),
        h3("Open Access, Licenses and Persistent Identifiers"),
        h4(
          HTML(
            'The <a href="https://doi.org/10.5281/zenodo.14627194">dataset publication</a>
        contains the datasets underlying the numbers and plots shown for the metrics included in this dashboard.'
          )
        ),
        br(),
        h3("Open Data and Data Repositories"),
        h4(
          HTML(
            'The dataset publication underlying the numbers and plots shown for the metrics included in the
                tab <b>Open data</b> will be published in <b>September 2026</b>.'
          )
        ),
        
        # bsCollapse(id = "datasetPanels_PublicationDataset",
        # bsCollapsePanel("Journal publications",
        #                DT::dataTableOutput("data_table_articles"),
        #                style = "default")),
        # bsCollapse(id = "datasetPanels_OtherDataset",
        #            bsCollapsePanel("Non-journal research outputs",
        #                            DT::dataTableOutput("data_table_other"),
        #                            style = "default")),
      ),
      tabPanel(
        "About",
        value = "tabAbout",
        h1("About"),
        h4(
          HTML(
            'This dashboard has been developed as part of the BUA OS Dashboards project and the BUA OS
        Magnifiers project.'
          )
        ),
        h4(
          HTML(
            'The BUA OS Dashboards project and the BUA OS Magnifiers project are funded by the Federal
        Ministry of Education and Research (BMBF) and the state of Berlin under the Excellence
        Strategy of the Federal Government and the Länder through the Berlin University Alliance.'
          )
        ),
        h4(
          HTML(
            'Our <a href="https://doi.org/10.5281/zenodo.14170751">conference paper</a>
        describes the results of analysis of Open Access statuses, Creative Commons licenses
        and Persistent Identifiers for journal and non-journal outputs between 2016 and 2022.
        Furthermore, for this paper, we also validated additional non-journal-article outputs
        such as \"conference abstracts\" and \"other\" (e.g. blog posts, book reviews and
        project reports). These are, however, not included in the dashboard visualisation.'
          )
        ),
        h4(
          HTML(
            'For a detailed description and analysis of publication data and open data at the Department
        of Earth Sciences at the Freie Universität Berlin, please contact us or visit
        the <a href="https://blogs.fu-berlin.de/open-research-berlin/">Open Research blog Berlin</a> for updates.'
          )
        )
      ),
    )
  ))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Server ----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++

server <- function(input, output, session)
{
  track_usage(
    storage_mode = store_json(path = "logs/"),
    what = c("session", "input", "output")
  )
  
  moduleServer_journal("plot_OA_journal")
  moduleServer_journal("license_journal")
  moduleServer_journal("plot_OA_non_journal")
  moduleServer_journal("plot_non_journal_pid")
  moduleServer_journal("open_data_articles")
  moduleServer_journal("open_data_repo")
  
  #actionButton to switch tabs
  observeEvent(input$buttonMethods, {
    updateTabsetPanel(session, "navbarTabs", selected = "tabMethods")
  })
  
  observeEvent(input$buttonDatasets, {
    updateTabsetPanel(session, "navbarTabs", selected = "tabDatasets")
    updateCollapse(session, "datasetPanels_PublicationDataset", open = "Earth Sciences publications dataset")
    updateCollapse(session, "datasetPanels_OtherDataset", open = "Earth Sciences non-journal research outputs dataset")
  })
  
  # color_scheme <- reactive({
  #   if (input$checkbox_colorblind) {
  #     color_seq <- c("#440154", "#25858E", "#FDE725")  # Colorblind-friendly color scheme
  #   } else {
  #     color_seq <- c("#AA493A", "#F1BA50", "#007265")  # Default color scheme
  #     #return(brewer.pal(7,"Greens"))
  #   }
  # })
  
  # URI routing
  observeEvent(session$clientData$url_hash, {
    currentHash <- sub("#", "", session$clientData$url_hash)
    if (is.null(input$navbarTabs) ||
        !is.null(currentHash) && currentHash != input$navbarTabs) {
      freezeReactiveValue(input, "navbarTabs")
      updateTabsetPanel(session, "navbarTabs", selected = currentHash)
    }
  }, priority = 1)
  
  observeEvent(input$navbarTabs, {
    currentHash <- sub("#", "", session$clientData$url_hash)
    pushQueryString <- paste0("#", input$navbarTabs)
    if (is.null(currentHash) || currentHash != input$navbarTabs) {
      freezeReactiveValue(input, "navbarTabs")
      updateQueryString(pushQueryString, mode = "push", session)
    }
  }, priority = 0)
  
}


# Run the application
shinyApp(ui = ui, server = server)
