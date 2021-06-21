
# load libraries
library(shiny)


source("Functions_CRAFTY-Scotland_WEB.R")  # shiny runs at the folder in which server and ui scripts exist.


# 
# absolutePanel(
#   top = 0, left = 0, right = 0,
#   fixed = TRUE,
#   div(
#     style="padding: 8px; border-bottom: 1px solid #CCC; background: #FFFFEE;",
#     HTML(markdownToHTML(fragment.only=TRUE, text=c(
#       "This absolutePanel is docked to the top of the screen
#                  using `top`, `left`, and `right` attributes.
#                  Because `fixed=TRUE`, it won't scroll with the page."
#     )))
#   )
# )

# https://www.cultureofinsight.com/blog/2018/03/15/2018-03-15-responsive-iframes-for-shiny-apps/


navbarPage("CRAFTY interactive web-interface", windowTitle =  "CRAFTY interactive web-interface (Scotland project 2021)", fluid = T, 
           
           
           tabPanel("Map and parameters", 
                    
                    # Sidebar layout with input and output definitions ----
                    sidebarLayout(
                      sidebarPanel(width=SIDEBAR_WIDTH,
                                   
                                   
                                   # selectInput("version", "Version",
                                   #             version_names, selected = version_names[1]
                                   # ), 
                                   fluidPage(br(), h4("Scenario customisation"))
                                   , sliderInput("year",
                                                 "Year:",
                                                 min = 2015,
                                                 max = 2100, sep = "",
                                                 value = 2015, step=10, animate =animationOptions(interval = 5000, loop = FALSE, playButton = NULL, pauseButton = NULL)),
                                   
                                   selectInput("paramset_full", label = "Behavioural parameter set-up",
                                               choices = paramsets_fullnames, selected = paramsets_fullnames[1]
                                   ),
                                   selectInput("scenario", "Vision",
                                               scenario_names[], selected = scenario_names[1]
                                   ),
                                   
                                   radioButtons("outputGroup", "Print layer",
                                                c("Output"="print_out", "Input"="print_in")
                                   ),
                                   # actionButton(inputId = "REFRESH", label = "Refresh map")
                                   # ,
                                   fluidPage(br(), h4("Map customisation"))
                                   , selectInput("outputlayer", "Model Output", 
                                                 indicator_names[c(29, 1:14)], selected=indicator_names[28]
                                   )                                  
                                   , selectInput("inputlayer", "Model Input",
                                                 indicator_names[15:28], selected=indicator_names[28]
                                   )
                                   , htmlOutput("ReferenceToScenarios")
                                   
                                   , actionButton(inputId = "deleteCache", label = "Delete cached files")
                                   
                      ),
                      # 
                      # # Main panel for displaying outputs ----
                      mainPanel(width=MAINPANEL_WIDTH, 
                                tabsetPanel(
                                  tabPanel("Map", 
                                           leafletOutput("Tab1_MapPane", height = PLOT_HEIGHT)
                                           # Run info
                                           , verbatimTextOutput("PaneRuninfo")
                                           
                                           , absolutePanel(
                                             top = 380, left = 20, width = 180,
                                             draggable = TRUE,
                                             
                                             
                                             wellPanel(
                                               # HTML(markdownToHTML(fragment.only=TRUE, text=c("contents"
                                               #   # "This is an absolutePanel that uses `bottom` and `right` attributes.
                                               #   # It also has `draggable = TRUE`, so you can drag it to move it around the page.
                                               #   # The slight transparency is due to `style = 'opacity: 0.92'`.
                                               #   # You can put anything in absolutePanel, including inputs and outputs:"
                                               # ))),
                                               
                                               sliderInput("alpha", "Transparency",0, 1,
                                                           value = TRANSPARENCY_DEFAULT, step = 0.1
                                               ),
                                               selectInput("colors", "Color palette (cont.)",
                                                           rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                                               )
                                               
                                               # , radioButtons("plotType", "Plot type",
                                               #              c("Scatter"="p", "Line"="l")
                                               # )
                                               , checkboxInput("InvertColour", "Invert colour", FALSE)
                                               , checkboxInput("legend", "Show legend", TRUE)
                                               ,
                                               
                                               selectInput("background", "Basemap", choices =
                                                             provider_names, selected=providers$OpenStreetMap.Mapnik
                                               )
                                               # sliderInput("n", "", min=3, max=20, value=5),
                                               # plotOutput("plot2", height="50px")
                                               , downloadLink("downloadData", "Download output map (GeoTIFF)")
                                             ),
                                             style = "opacity: 0.8" # 0.5 previously
                                             
                                             
                                           )
                                  )
                                  , tabPanel("Time-series",
                                             plotOutput("Tab2_TimeseriesPlotPane", height = PLOT_HEIGHT)
                                             
                                             # Map view options
                                  )
                                  
                                  , tabPanel("Behavioural parameters",
                                             # Time series info
                                             dataTableOutput('Tab1_BehaviouralTablePane')
                                             , htmlOutput("ReferenceToParameters")
                                             
                                             # Map view options
                                  )
                                  
                         
                                )))
           )
           
           
           , tabPanel("Land Use Transition",
                      # Show a transition plot of the selected
                      
                      # Sidebar layout with input and output definitions ----
                      sidebarLayout(
                        sidebarPanel(width=SIDEBAR_WIDTH, height=PLOT_HEIGHT,
                                     
                                     fluidPage(br(), h4("Land use transition from"))
                                     
                                     , selectInput("version_from", "Version",
                                                   version_names, selected = version_names[version_default_idx]
                                     )
                                     , sliderInput("year_from",
                                                   "Year",
                                                   min = 2015,
                                                   max = 2100, sep = "",
                                                   value = 2015, step=10),
                                     selectInput("paramset_full_from", label = "Behavioural parameter set-up",
                                                 choices = paramsets_fullnames, selected = paramsets_fullnames[1]
                                     ),
                                     selectInput("scenario_from", "Climate and socio-economic scenario",
                                                 scenario_names[], selected = scenario_names[1]
                                                 
                                     )
                                     , fluidPage(br(), h4("To"))
                                     
                                     , selectInput("version_to", "Version",
                                                   version_names, selected = version_names[1]
                                     )
                                     ,  sliderInput("year_to",
                                                    "Year",
                                                    min = 2015,
                                                    max = 2100, sep = "",
                                                    value = 2080, step=10),
                                     selectInput("paramset_full_to", label = "Behavioural parameter set-up",
                                                 choices = paramsets_fullnames, selected = paramsets_fullnames[1]
                                     ),
                                     selectInput("scenario_to", "Vision",
                                                 scenario_names[], selected = scenario_names[2]
                                     )
                                     
                                     
                        ),
                        #
                        # # Main panel for displaying outputs ----
                        mainPanel(
                          
                          tabPanel("Tab3_TransitionPlotPane",
                                   plotOutput("Tab3_TransitionPlotPane")
                          )
                        )
                      )
           )
           , tabPanel("AFT description",
                      # AFT info
                      dataTableOutput("Tab1_AFTTablePane")
           )
           # , tabPanel("About",
           #            fluidRow(
           #              column(12,
           #                     includeMarkdown("crafty_about.md")
           #              )
           #              
           #            )
           # )
           
)
