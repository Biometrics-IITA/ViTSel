library(knitr)
library(kableExtra)
library(shiny)
library(shinydashboard)
library(GGally)
library(readxl)
library(openxlsx)
library(factoextra)
library(magrittr)
library(huxtable)
library(tidyverse)

ui <- div(
  tags$head(tags$style(
    HTML(".shiny-output-error-validation {color: green;}")
  )),
  class = "navbar1",
  navbarPage(
    title = div(tags$b("ViTSel"), style = "color:#F26522"),
    windowTitle = "ViTSel",
    theme = "mycustom.css",
    
    tabPanel("Input File",
             sidebarLayout(
               tags$div(
                 style = "color:white;",
                 sidebarPanel(
                   tags$style(".well {background-color:black;}"),
                   div(
                     tags$p(
                       "The input data is the table of performance of genotypes (BLUPS or BLUES) stacked by environment.
                       If the combined analysis is included, its environment must be coded",
                       style = "display: inline;",
                       tags$b("OVERALL.", style = "color:#F26522")
                     )
                   ),
                   br(),
                   tags$div(
                     "See example of an input data:",
                     br(),
                     downloadButton("dwlExample", "Example"),
                     style = "display:inline-block"
                   ),
                   hr(),
                   fileInput(
                     "fileUp",
                     "Upload file",
                     placeholder = "Select file...",
                     accept = ".xlsx"
                   ),
                   uiOutput("outSheetBLUPS"),
                   uiOutput("outGenotype"),
                   conditionalPanel(
                     condition = "output.fileUploaded",
                     selectInput(
                       "InputEnvironment",
                       "Select Environment",
                       choices = "",
                       selected = ""
                     )
                   ),
                   uiOutput("outTraitAnalysis"),
                   actionButton("btnPreview", "View Data", icon = icon("table"))
                 )
               ),
               
               mainPanel(tabsetPanel(
                 type = "tabs",
                 tabPanel("All values", htmlOutput("AllValues")),
                 tabPanel("Summary", htmlOutput("Summary"))
               ))
             )),
    
    navbarMenu(
      "Visualization",
      tabPanel("BiPlot",
               sidebarLayout(
                 tags$div(
                   style = "color:white;",
                   sidebarPanel(
                     tags$style(".well {background-color:black;}"),
                     tags$p("Genotype-by-environment biplots"),
                     hr(),
                     uiOutput("outseltraitBiplot"),
                     uiOutput("outDimX"),
                     uiOutput("outDimY")
                   )
                 ),
                 mainPanel(tabsetPanel(
                   type =
                     "tabs",
                   tabPanel(
                     "Biplot",
                     box(
                       div(
                         "Results of this visualization may be misleading in the presence of missing data.",
                         style = "color:gray"
                       ),
                       conditionalPanel(
                         condition = "(input.InputDimY.length==0)",
                         div(
                           "You need more than one environment (with or without the combined analysis).",
                           style = "color:gray"
                         )
                       ),
                       status = "primary",
                       solidHeader = TRUE,
                       width = 12
                     ),
                     br(),
                     tags$div(uiOutput("dwlBiPlotui"),
                              style = "display:inline-block"),
                     plotOutput("BiPlot")
                     
                   ),
                   tabPanel("Codes for Environment",
                            htmlOutput("CodesEnvBiplot")),
                   tabPanel("Codes for Genotype",
                            htmlOutput("CodesGenoBiPlot"))
                 ))
               )),
      
      tabPanel("BoxPlot",
               sidebarLayout(
                 tags$div(style = "color:white;",
                          sidebarPanel(
                            tags$style(".well {background-color:black;}"),
                            tags$p("Boxplots of traits by environment"),
                            hr(),
                            uiOutput("outseltraitBox")
                          )),
                 mainPanel(tabsetPanel(
                   type = "tabs",
                   tabPanel(
                     "Boxplot",
                     tags$div(uiOutput("dwlBoxPlotui"),
                              style =
                                "display:inline-block"),
                     plotOutput("BoxPlot")
                   ),
                   tabPanel("Codes for Environment",
                            htmlOutput("CodesEnvBoxPlot"))
                 ))
               )),
      tabPanel("HeatMap",
               sidebarLayout(
                 tags$div(style = "color:white;",
                          sidebarPanel(
                            tags$style(".well {background-color:black;}"),
                            tags$p("Heatmap of traits by environment"),
                            hr(),
                            uiOutput("outseltraitHeat")
                          )),
                 mainPanel(tabsetPanel(
                   type = "tabs",
                   tabPanel(
                     "Heatmap",
                     fluidRow(
                       column(width = 2,
                              h2(""),
                              align = "left"),
                       column(
                         width = 8,
                         align = "center",
                         img(src = "heatmap.png", width = "100%")
                       ),
                       column(width = 2,
                              h2(""),
                              align = "right")
                     ),
                     br(),
                     htmlOutput("TableHeatmap")
                     
                     
                   ),
                   tabPanel("Codes for Environment",
                            htmlOutput("CodesEnvHeat")),
                   tabPanel("Codes for Genotype",
                            htmlOutput("CodesGenoHeat"))
                 ))
               )),
      
      tabPanel("ScatterPlot",
               sidebarLayout(
                 tags$div(style = "color:white;",
                          sidebarPanel(
                            tags$style(".well {background-color:black;}"),
                            helpText(
                              "Scatterplot matrix by environment (including the combined analysis)."
                            )
                          )),
                 mainPanel(tabsetPanel(
                   type = "tabs",
                   tabPanel(
                     "Scatterplot",
                     uiOutput("OneTraitInfo"),
                     br(),
                     tags$div(uiOutput("dwlScatterPlotui"),
                              style =
                                "display:inline-block"),
                     br(),
                     plotOutput("ScatterPlot")
                   ),
                   
                   tabPanel("Codes for Environment",
                            htmlOutput("CodesEnvScater")),
                   tabPanel("Codes for Genotype",
                            htmlOutput("CodesGenoScater"))
                 ))
               ))
    ),
    
    navbarMenu(
      "Compare Genotypes",
      
      tabPanel("Checks",
               sidebarLayout(
                 tags$div(
                   style = "color:white;",
                   sidebarPanel(
                     tags$style(".well {background-color:black;}"),
                     uiOutput("outEnvChecks"),
                     uiOutput("outInputChecks"),
                     uiOutput("outtraitSelHighChecks"),
                     uiOutput("outtraitSelLowChecks"),
                     uiOutput("outSliderRangeCheks"),
                     uiOutput("outtraitSliderChecksR"),
                     hr(),
                     actionButton("btnCompareChecks", "Run", icon = icon("play-circle"))
                   )
                 ),
                 mainPanel(fluidRow(
                   box(
                     conditionalPanel(condition = "input.InputHighTraitsChecks!=='undefined' && input.InputHighTraitsChecks.length>0",
                                      htmlOutput("TableChecksHighInfo"),),
                     conditionalPanel(condition = "input.InputHighTraitsChecks!=='undefined' && input.InputHighTraitsChecks.length>0",
                                      htmlOutput("TableChecksHigh"),),
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12
                   ),
                   br(),
                   box(
                     conditionalPanel(condition = "input.InputLowTraitsChecks!=='undefined' && input.InputLowTraitsChecks.length>0",
                                      
                                      htmlOutput("TableChecksLowInfo"),),
                     conditionalPanel(condition = "input.InputLowTraitsChecks!=='undefined' && input.InputLowTraitsChecks.length>0",
                                      htmlOutput("TableChecksLow"),),
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12
                   ),
                   br(),
                   box(
                     conditionalPanel(condition = "input.InputRangeTraitsChecks!=='undefined' && input.InputRangeTraitsChecks.length>0",
                                      htmlOutput("TableChecksRInfo"),),
                     conditionalPanel(condition = "input.InputRangeTraitsChecks!=='undefined' && input.InputRangeTraitsChecks.length>0",
                                      htmlOutput("TableChecksR"),),
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12
                   ),
                   br(),
                   box(
                     conditionalPanel(condition = "input.InputEnvChecks === 'OVERALL'",
                                      htmlOutput("TableSatisfyChecksEnvSupInfo")),
                     br(),
                     tags$div(uiOutput("dwlChecksAllui"),
                              style =
                                "display:inline-block"),
                     br(),
                     br(),
                     conditionalPanel(condition = "(input.InputHighTraitsChecks!=='undefined' && input.InputHighTraitsChecks.length>0) || (input.InputLowTraitsChecks!=='undefined' && input.InputLowTraitsChecks.length>0) || (input.InputRangeTraitsChecks!=='undefined' && input.InputRangeTraitsChecks.length>0)",
                                      htmlOutput("TableSatisfyChecks")),
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12
                   )
                 ))
               )),
      tabPanel("Specificied values",
               sidebarLayout(
                 tags$div(
                   style = "color:white;",
                   sidebarPanel(
                     tags$style(".well {background-color:black;}"),
                     uiOutput("outEnvSpecific"),
                     uiOutput("outSliderSpecific"),
                     uiOutput("outtraitSelLow"),
                     uiOutput("outSliderRange"),
                     uiOutput("outtraitSlider"),
                     uiOutput("outtraitSliderR"),
                     hr(),
                     actionButton("btnCompareSpecific", "Run", icon = icon("play-circle"))
                   )
                 ),
                 mainPanel(fluidRow(
                   box(
                     conditionalPanel(condition = "input.InputHighTraits!=='undefined' && input.InputHighTraits.length>0",
                                      htmlOutput("TableSpecificHighInfo")),
                     conditionalPanel(condition = "input.InputHighTraits!=='undefined' && input.InputHighTraits.length>0",
                                      htmlOutput("TableSpecifiedValuesHigh")),
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12
                   ),
                   br(),
                   
                   box(
                     conditionalPanel(condition = "input.InputLowTraits!=='undefined' && input.InputLowTraits.length>0",
                                      htmlOutput("TableSpecificLowInfo")),
                     conditionalPanel(condition = "input.InputLowTraits!=='undefined' && input.InputLowTraits.length>0",
                                      htmlOutput("TableSpecifiedValuesLow")),
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12
                   ),
                   br(),
                   
                   
                   
                   box(
                     conditionalPanel(condition = "input.InputRangeTraits!=='undefined' && input.InputRangeTraits.length>0",
                                      htmlOutput("TableSpecifiedValuesRInfo")),
                     conditionalPanel(condition = "input.InputRangeTraits!=='undefined' && input.InputRangeTraits.length>0",
                                      htmlOutput("TableSpecifiedValuesR")),
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12
                   ),
                   br(),
                   box(
                     conditionalPanel(condition = "input.InputEnvSpecific==='OVERALL'",
                                      htmlOutput("TableSatisfySpecificEnvSupInfo")),
                     br(),
                     tags$div(uiOutput("dwlSpecificAllui"),
                              style = "display:inline-block"),
                     br(),
                     br(),
                     conditionalPanel(condition = "(input.InputHighTraits!=='undefined' && input.InputHighTraits.length>0) || (input.InputLowTraits!=='undefined' && input.InputLowTraits.length>0) || (input.InputRangeTraits!=='undefined' && input.InputRangeTraits.length>0)",
                                      htmlOutput("TableSatisfySpecific")),
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12
                   )
                 ))
               )),
      
      tabPanel("Base Index",
               sidebarLayout(
                 tags$div(
                   style = "color:white;",
                   sidebarPanel(
                     tags$style(".well {background-color:black;}"),
                     uiOutput("outEnvBaseIndex"),
                     uiOutput("outTraitsBaseIndex"),
                     uiOutput("outTraitsBaseIndexSlider"),
                     hr(),
                     actionButton("btnBaseIndex", "Run", icon = icon("play-circle"))
                   )
                 ),
                 mainPanel(fluidRow(
                   box(
                     conditionalPanel(condition = "(input.InputBaseTraits!=='undefined' && input.InputBaseTraits.length>0)",
                                      htmlOutput("TableBaseInfo")),
                     br(),
                     tags$div(uiOutput("dwlBaseIndexAllui"),
                              style = "display:inline-block"),
                     br(),
                     br(),
                     conditionalPanel(condition = "(input.InputRankTraits!=='undefined' && input.InputRankTraits.length>0)",
                                      htmlOutput("TableBaseIndex")),
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12
                   )
                 ))
               ))
    ),
    
    tabPanel(
      "About",
      
      fluidRow(
        column(width = 4, h2(""), align = "left"),
        column(width = 4,
               img(src = "cgiar.png", align = "left")),
        column(width = 4, h2(""), align = "left")
      ),
      br(),
      fluidRow(
        column(width = 4,
               img(
                 src = "AfricaRice.png", align = "left", width = "58%"
               )),
        column(
          width = 4,
          img(src = "CIMMYT_new.png", align = "left", width = "62.5%")
        ),
        column(width = 4,
               img(
                 src = "iita.png", align = "left", width = "41%"
               ))
        
      ),
      
      
      
      br(),
      br(),
      
      
      fluidRow(
        column(width = 2,
               h2("")),
        column(
          width = 8,
          tags$p(
            "ViTSel which stands for Visualization Tool for Selection is jointly developed by AfricaRice Data Integration and Biometrics Unit, CIMMYT Biometrics and Statistics Unit, and IITA Biometrics Unit.
                                                 ViTSel meets specialized needs for Breeders to investigate and visualize Genotype by Environment interaction (GxE).
                                                 This program is intended as a decision support tool to ease selection."
          ),
          br(),
          
          tags$b("Authors:"),
          tags$p(
            "Ibnou Dieng",
            br(),
            "Francisco Rodriguez",
            br(),
            "Angela Pacheco",
            br(),
            "Gregorio Alvarado",
            br(),
            "Juan Burgueno"
          )
          
        )
      ),
      
      br(),
      
      fluidRow(column(width = 2,
                      h2("")),
               column(
                 width = 8,
                 tags$p(
                   "ViTSel was developped with the support of the Excellence in Breeding Platform (EiB)"
                 )
               )),
      
      br(),
      
      fluidRow(
        column(width = 4,
               h2(""),
               align = "left"),
        column(width = 4,
               align = "left",
               img(src = "EiB.png")),
        column(width = 4,
               h2(""),
               align = "left")
      ),
      
      br(),
      br(),
      
      fluidRow(
        column(width = 2,
               h2("")),
        column(
          width = 8,
          tags$p(
            "ViTSel is a graphical user interface based on",
            tags$a("R", href = "https://www.r-project.org/"),
            "a free software for statistical computing and graphics.

                                               Any R component of this program as well as the program as a whole developed by AfricaRice, CIMMYT and IITA are
                                                      hereby licensed as per the terms of the",
            tags$a("GNU General Public License version 3.", href =
                     "http://www.gnu.org/licenses/gpl-3.0.html"),
            
            "ViTSel is free, you can re-distribute it and/or modify it under the terms of the GNU General Public
                                              License as published by the Free Software Foundation; either version 3 of the License, or any later version.
                                              For details about the GNU General Public License; please write to the Free Software Foundation, Inc.,
                                              51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA. ViTSel is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
                                              implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE."
          ),
          
          tags$p(
            "ViTSel includes the R",
            tags$a("shiny", href = "https://shiny.rstudio.com/"),
            "tools and many other free third-part libraries.
                                              The following is a list of these components:"
          ),
          tags$ul(
            tags$li("factoextra"),
            tags$li("GGally"),
            tags$li("huxtable"),
            tags$li("kableExtra"),
            tags$li("knitr"),
            tags$li("magrittr"),
            tags$li("openxlsx"),
            tags$li("readxl"),
            tags$li("shiny"),
            tags$li("shinydashboard"),
            tags$li("tidyverse")
          ),
          
          tags$p(
            "Citation:",
            br(),
            tags$b(
              "ViTSel. Version 1.1 (2023). Africa Rice Centre (AfricaRice), Centro Internacional de Mejoramiento de Maiz y Trigo (CIMMYT), and International Institute of Tropical Agriculture (IITA)."
            )
          ),
          br(),
          
          tags$p("For further information, please contact:"),
          tags$p(
            tags$b("AfricaRice"),
            br(),
            "Mbe Research Station",
            br(),
            "01 B.P. 2551, Bouake 01, Cote d'Ivoire",
            br(),
            "T: +225 22 48 09 20",
            br(),
            "F: +225 31 63 25 78",
            br(),
            "E-mail: AfricaRice@cgiar.org"
          ),
          br(),
          tags$p(
            tags$b("CIMMYT"),
            br(),
            "Km. 45, Carretera",
            br(),
            "Mexico-Veracruz, El Batán,",
            br(),
            "Texcoco CP 56237",
            br(),
            "Edo. de Mexico, Mexico",
            br(),
            "Tel: +52 55 5804 2004 or +52 595 952 1900 or +1 612 605-5205",
            br(),
            "Email: CIMMYT-Knowledge-Center@cgiar.org"
          ),
          br(),
          tags$p(
            tags$b("IITA"),
            br(),
            "PMB 5320, Oyo Road",
            br(),
            "Ibadan 200001, Nigeria",
            br(),
            "Tel: +234 700800IITA or +1 201 633-6094",
            br(),
            "Email:  iita@cgiar.org"
          ),
          br(),
          br()
        )
      )
    )
  )
  
)
