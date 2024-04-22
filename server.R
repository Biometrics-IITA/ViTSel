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

server <- function (input, output, session) {
  ########################################################################
  # Import data
  ########################################################################
  
  # Upload example file
  
  BLUPS_Example <- read_excel("data/Example.xlsx", "BLUPS")
  
  output$dwlExample <- downloadHandler(
    filename = function() {
      paste0("Example", ".xlsx")
    },
    content = function(file) {
      write.xlsx(BLUPS_Example, file, rowNames = FALSE)
    }
  )
  
  
  dataBLUPS <- reactive({
    req(input$fileUp)
    dfile <- input$fileUp
    if (!is.null(dfile) && input$selSheetBLUPS %in% sheets()) {
      dBLUPS <- read_excel(dfile$datapath, sheet = input$selSheetBLUPS)
      dBLUPS %<>%
        filter_all(any_vars(!is.na(.)))
    } else {
      return()
    }
  })
  
  output$fileUploaded <- reactive({
    return(!is.null(input$fileUp))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
  
  sheets <- reactive({
    if (!is.null(input$fileUp)) {
      return(excel_sheets(path = input$fileUp$datapath))
    } else {
      return(NULL)
    }
  })
  
  output$outSheetBLUPS <- renderUI({
    req(input$fileUp)
    selectInput("selSheetBLUPS",
                "Select Sheet",
                choices = sheets())
  })
  
  output$outGenotype <- renderUI({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    dataBLUPSAll <- dataBLUPS()
    
    dNames <- names(dataBLUPSAll)
    selectInput('InputGenotype',
                'Select Genotype',
                choices = dNames)
  })
  
  observeEvent(input$InputGenotype, {
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputGenotype)
    dataBLUPSAll <- dataBLUPS()
    dNames <- names(dataBLUPSAll)
    updateSelectInput(session,
                      "InputEnvironment",
                      "Select Environment",
                      choices = dNames[dNames != input$InputGenotype])
  })
  
  output$outTraitAnalysis <- renderUI({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    dataBLUPSAll <- dataBLUPS()
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    TraitsAll <-
      sort(setdiff(names(dataBLUPSAll), c(Environment, Genotype)))
    dataBLUPSAll <- dataBLUPSAll %>%
      select(all_of(TraitsAll))
    selectInput(
      'InputAnalysisTraits',
      'Select Trait(s)',
      choices = TraitsAll,
      selected = TraitsAll[1],
      multiple = TRUE
    )
  })
  
  hideAll <- reactiveValues(clearAll = TRUE)
  
  observeEvent(
    c(
      input$fileUp,
      input$selSheetBLUPS,
      input$InputGenotype,
      input$InputEnvironment,
      input$InputAnalysisTraits,
      input$InputEnvChecks,
      input$InputChecks,
      input$InputHighTraitsChecks,
      input$InputLowTraitsChecks,
      input$InputRangeTraitsChecks,
      #input$shuklaChecks,
      input$InputEnvSpecific,
      input$InputHighTraits,
      input$InputLowTraits,
      input$InputRangeTraits,
      input$TraitSlider,
      input$TraitSliderR,
      #input$shuklaSpecific,
      input$InputEnvRankIndex,
      input$InputRankTraits,
      input$InputEnvBaseIndex,
      input$InputBaseTraits
    ),
    {
      hideAll$clearAll <- TRUE
    }
  )
  
  output$outEnvSpecific <- renderUI({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputAnalysisTraits)
    req(input$InputEnvironment)
    Environment <- input$InputEnvironment
    dataBLUPSAll <- dataBLUPS()
    dataBLUPSAll %<>%
      rename(Environment = all_of(Environment))
    Env <- unique(dataBLUPSAll$Environment)
    
    
    if ("OVERALL" %in% Env) {
      if (length(Env) > 2) {
        Env <- sort(Env[Env != "OVERALL"])
        selectInput(
          'InputEnvSpecific',
          'Environment',
          choices = list("OVERALL", 'Environments' = Env)
        )
      } else {
        selectInput('InputEnvSpecific', 'Environment', choices = Env)
      }
      
    } else {
      selectInput('InputEnvSpecific', 'Environment', choices = Env)
    }
    
  })
  
  output$outSliderSpecific <- renderUI({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputAnalysisTraits)
    req(input$InputEnvSpecific)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currenttraits <- input$InputAnalysisTraits
    currenttraits <- input$InputAnalysisTraits
    currentenv <- input$InputEnvSpecific
    myEnvironment <- reactive({
      currentenv
    })
    myEnvironment <- myEnvironment()
    dataBLUPSAll <- dataBLUPS()
    dataBLUPS <- dataBLUPSAll %>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype)) %>%
      filter(Environment == myEnvironment) %>%
      select(Environment, Genotype, all_of(currenttraits)) %>%
      arrange(Genotype)
    currenttraits <-
      currenttraits[colSums(is.na(dataBLUPS[, currenttraits])) != nrow(dataBLUPS[, currenttraits])]
    selectInput(
      'InputHighTraits',
      paste0("Trait(s)", ": ", "higher values"),
      choices = currenttraits,
      multiple = TRUE
    )
  })
  
  output$outtraitSelLow <- renderUI({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputAnalysisTraits)
    req(input$InputEnvSpecific)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currenttraits <- input$InputAnalysisTraits
    currenttraithigh <- input$InputHighTraits
    currentenv <- input$InputEnvSpecific
    myEnvironment <- reactive({
      currentenv
    })
    myEnvironment <- myEnvironment()
    dataBLUPSAll <- dataBLUPS()
    dataBLUPS <- dataBLUPSAll %>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype)) %>%
      filter(Environment == myEnvironment) %>%
      select(Environment, Genotype, all_of(currenttraits)) %>%
      arrange(Genotype)
    currenttraits <-
      currenttraits[colSums(is.na(dataBLUPS[, currenttraits])) != nrow(dataBLUPS[, currenttraits])]
    if (!is.null(currenttraithigh)) {
      currenttraits <- sort(setdiff(currenttraits, currenttraithigh))
    }
    selectInput(
      'InputLowTraits',
      paste0("Trait(s)", ": ", "lower values"),
      choices = currenttraits,
      multiple = TRUE
    )
  })
  
  output$outtraitSlider <- renderUI({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputEnvSpecific)
    req(input$InputAnalysisTraits)
    req(!is.null(input$InputHighTraits) |
          !is.null(input$InputLowTraits))
    req(input$InputGenotype)
    req(input$InputEnvironment)
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currentenv <- input$InputEnvSpecific
    currenttraits <- input$InputAnalysisTraits
    currenttraithigh <- input$InputHighTraits
    currenttraitlow <- input$InputLowTraits
    currenttraithighlow <-
      sort(c(currenttraithigh, currenttraitlow))
    dataBLUPSAll <- dataBLUPS()
    myEnvironment <- reactive({
      currentenv
    })
    myEnvironment <- myEnvironment()
    dataBLUPS <- dataBLUPSAll %>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype)) %>%
      filter(Environment == myEnvironment) %>%
      select(Environment, Genotype, all_of(currenttraits)) %>%
      arrange(Genotype)
    summaryDat <- NULL
    for (i in 1:length(currenttraithighlow)) {
      trait <- dataBLUPS %>%
        pull(currenttraithighlow[i])
      summaryDat <-
        rbind(
          summaryDat,
          data.frame(
            Trait = currenttraithighlow[i],
            Min = min(trait, na.rm =
                        TRUE),
            Mean = mean(trait, na.rm =
                          TRUE),
            Max = max(trait, na.rm =
                        TRUE),
            stringsAsFactors = FALSE
          )
        )
    }
    lapply(1:nrow(summaryDat), function(i) {
      sliderInput(
        paste0('TraitSlider', i),
        currenttraithighlow[i],
        min = round(summaryDat[i, 2], 2),
        max = round(summaryDat[i, 4], 2),
        value = round(summaryDat[i, 3], 2),
        step = 0.01
      )
    })
  })
  
  output$outSliderRange <- renderUI({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputAnalysisTraits)
    req(input$InputEnvSpecific)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currenttraits <- input$InputAnalysisTraits
    currenttraithigh <- input$InputHighTraits
    currenttraitlow <- input$InputLowTraits
    currenttraithighlow <- c(currenttraithigh, currenttraitlow)
    currentenv <- input$InputEnvSpecific
    dataBLUPSAll <- dataBLUPS()
    myEnvironment <- reactive({
      currentenv
    })
    myEnvironment <- myEnvironment()
    dataBLUPSAll <- dataBLUPS()
    dataBLUPS <- dataBLUPSAll %>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype)) %>%
      filter(Environment == myEnvironment) %>%
      select(Environment, Genotype, all_of(currenttraits)) %>%
      arrange(Genotype)
    currenttraits <-
      currenttraits[colSums(is.na(dataBLUPS[, currenttraits])) != nrow(dataBLUPS[, currenttraits])]
    currenttraits <-
      sort(setdiff(currenttraits, currenttraithighlow))
    selectInput(
      'InputRangeTraits',
      paste0("Trait(s)", ": ", "values within range"),
      choices = currenttraits,
      multiple = TRUE
    )
  })
  
  output$outtraitSliderR <- renderUI({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputAnalysisTraits)
    req(input$InputEnvSpecific)
    req(input$InputRangeTraits)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currentenv <- input$InputEnvSpecific
    currenttraits <- input$InputAnalysisTraits
    currenttraitrange <- input$InputRangeTraits
    dataBLUPSAll <- dataBLUPS()
    myEnvironment <- reactive({
      currentenv
    })
    myEnvironment <- myEnvironment()
    dataBLUPS <- dataBLUPSAll %>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype)) %>%
      filter(Environment == myEnvironment) %>%
      select(Environment, Genotype, all_of(currenttraitrange)) %>%
      arrange(Genotype)
    summaryDat <- NULL
    for (i in 1:length(currenttraitrange)) {
      trait <- dataBLUPS %>%
        pull(currenttraitrange[i])
      summaryDat <-
        rbind(
          summaryDat,
          data.frame(
            Trait = currenttraitrange[i],
            Min = min(trait, na.rm =
                        TRUE),
            Mean = mean(trait, na.rm =
                          TRUE),
            Max = max(trait, na.rm =
                        TRUE),
            stringsAsFactors = FALSE
          )
        )
    }
    lapply(1:nrow(summaryDat), function(i) {
      sliderInput(
        paste0('TraitSliderR', i),
        currenttraitrange[i],
        min = round(summaryDat[i, 2], 2),
        max = round(summaryDat[i, 4], 2),
        value = round(c(summaryDat[i, 2], summaryDat[i, 4]), 2),
        step = 0.01
      )
    })
  })
  
  output$outEnvChecks <- renderUI({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputAnalysisTraits)
    req(input$InputEnvironment)
    Environment <- input$InputEnvironment
    dataBLUPSAll <- dataBLUPS()
    dataBLUPSAll %<>%
      rename(Environment = all_of(Environment))
    Env <- unique(dataBLUPSAll$Environment)
    
    if ("OVERALL" %in% Env) {
      if (length(Env) > 2) {
        Env <- sort(Env[Env != "OVERALL"])
        selectInput('InputEnvChecks',
                    'Environment',
                    choices = list("OVERALL", 'Environments' = Env))
      } else {
        selectInput('InputEnvChecks', 'Environment', choices = Env)
      }
      
    } else {
      selectInput('InputEnvChecks', 'Environment', choices = Env)
    }
  })
  
  output$outInputChecks <- renderUI({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputEnvChecks)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currentenv <- input$InputEnvChecks
    dataBLUPSAll <- dataBLUPS()
    myEnvironment <- reactive({
      currentenv
    })
    myEnvironment <- myEnvironment()
    dataBLUPS <- dataBLUPSAll %>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype)) %>%
      filter(Environment == myEnvironment) %>%
      arrange(Genotype)
    Genotypes <- sort(unique(dataBLUPS$Genotype))
    selectInput('InputChecks',
                'Check(s)',
                choices = Genotypes,
                multiple = TRUE)
  })
  
  output$outtraitSelHighChecks <- renderUI({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputAnalysisTraits)
    req(input$InputEnvChecks)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currenttraits <- input$InputAnalysisTraits
    currentenv <- input$InputEnvChecks
    myEnvironment <- reactive({
      currentenv
    })
    myEnvironment <- myEnvironment()
    dataBLUPSAll <- dataBLUPS()
    dataBLUPS <- dataBLUPSAll %>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype)) %>%
      filter(Environment == myEnvironment) %>%
      select(Environment, Genotype, all_of(currenttraits)) %>%
      arrange(Genotype)
    currenttraits <-
      currenttraits[colSums(is.na(dataBLUPS[, currenttraits])) != nrow(dataBLUPS[, currenttraits])]
    selectInput(
      'InputHighTraitsChecks',
      paste0("Trait(s)", ": ", "higher values"),
      choices = currenttraits,
      multiple = TRUE
    )
  })
  
  output$outtraitSelLowChecks <- renderUI({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputAnalysisTraits)
    req(input$InputEnvChecks)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currenttraits <- input$InputAnalysisTraits
    currenttraithigh <- input$InputHighTraitsChecks
    currentenv <- input$InputEnvChecks
    myEnvironment <- reactive({
      currentenv
    })
    myEnvironment <- myEnvironment()
    dataBLUPSAll <- dataBLUPS()
    dataBLUPS <- dataBLUPSAll %>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype)) %>%
      filter(Environment == myEnvironment) %>%
      select(Environment, Genotype, all_of(currenttraits)) %>%
      arrange(Genotype)
    currenttraits <-
      currenttraits[colSums(is.na(dataBLUPS[, currenttraits])) != nrow(dataBLUPS[, currenttraits])]
    currenttraits <- sort(setdiff(currenttraits, currenttraithigh))
    selectInput(
      'InputLowTraitsChecks',
      paste0("Trait(s)", ": ", "lower values"),
      choices = currenttraits,
      multiple = TRUE
    )
  })
  
  output$outSliderRangeCheks <- renderUI({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputAnalysisTraits)
    req(input$InputEnvChecks)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currenttraits <- input$InputAnalysisTraits
    currenttraithigh <- input$InputHighTraitsChecks
    currenttraitlow <- input$InputLowTraitsChecks
    currenttraithighlow <- c(currenttraithigh, currenttraitlow)
    currentenv <- input$InputEnvChecks
    myEnvironment <- reactive({
      currentenv
    })
    myEnvironment <- myEnvironment()
    dataBLUPSAll <- dataBLUPS()
    dataBLUPS <- dataBLUPSAll %>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype)) %>%
      filter(Environment == myEnvironment) %>%
      select(Environment, Genotype, all_of(currenttraits)) %>%
      arrange(Genotype)
    currenttraits <-
      currenttraits[colSums(is.na(dataBLUPS[, currenttraits])) != nrow(dataBLUPS[, currenttraits])]
    currenttraits <-
      sort(setdiff(currenttraits, currenttraithighlow))
    selectInput(
      'InputRangeTraitsChecks',
      paste0("Trait(s)", ": ", "values within range"),
      choices = currenttraits,
      multiple = TRUE
    )
  })
  
  output$outtraitSliderChecksR <- renderUI({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputEnvChecks)
    req(input$InputRangeTraitsChecks)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currentenv <- input$InputEnvChecks
    currenttraitrange <- input$InputRangeTraitsChecks
    dataBLUPSAll <- dataBLUPS()
    myEnvironment <- reactive({
      currentenv
    })
    myEnvironment <- myEnvironment()
    dataBLUPS <- dataBLUPSAll %>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype)) %>%
      filter(Environment == myEnvironment) %>%
      select(Environment, Genotype, all_of(currenttraitrange)) %>%
      arrange(Genotype)
    
    summaryDat <- NULL
    for (i in 1:length(currenttraitrange)) {
      trait <- dataBLUPS %>%
        pull(currenttraitrange[i])
      summaryDat <-
        rbind(
          summaryDat,
          data.frame(
            Trait = currenttraitrange[i],
            Min = min(trait, na.rm =
                        TRUE),
            Mean = mean(trait, na.rm =
                          TRUE),
            Max = max(trait, na.rm =
                        TRUE),
            stringsAsFactors = FALSE
          )
        )
    }
    lapply(1:nrow(summaryDat), function(i) {
      sliderInput(
        paste0('TraitSliderChecksR', i),
        currenttraitrange[i],
        min = round(summaryDat[i, 2], 2),
        max = round(summaryDat[i, 4], 2),
        value = round(c(summaryDat[i, 2], summaryDat[i, 4]), 2),
        step = 0.01
      )
    })
  })
  
  output$outEnvRankIndex <- renderUI({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputAnalysisTraits)
    req(input$InputEnvironment)
    Environment <- input$InputEnvironment
    dataBLUPSAll <- dataBLUPS()
    dataBLUPSAll %<>%
      rename(Environment = all_of(Environment))
    Env <- unique(dataBLUPSAll$Environment)
    
    if ("OVERALL" %in% Env) {
      if (length(Env) > 2) {
        Env <- sort(Env[Env != "OVERALL"])
        selectInput(
          'InputEnvRankIndex',
          'Environment',
          choices = list("OVERALL", 'Environments' = Env)
        )
      } else {
        selectInput('InputEnvRankIndex', 'Environment', choices = Env)
      }
      
    } else {
      selectInput('InputEnvRankIndex', 'Environment', choices = Env)
    }
    
  })
  
  output$outSliderRankIndexHigh <- renderUI({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    req(input$InputEnvRankIndex)
    req(input$InputAnalysisTraits)
    currenttraits <- input$InputAnalysisTraits
    dataBLUPSAll <- dataBLUPS()
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    
    currentenv <- input$InputEnvRankIndex
    myEnvironment <- reactive({
      currentenv
    })
    myEnvironment <- myEnvironment()
    dataBLUPSAll <- dataBLUPS()
    dataBLUPS <- dataBLUPSAll %>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype)) %>%
      filter(Environment == myEnvironment) %>%
      select(Environment, Genotype, all_of(currenttraits)) %>%
      arrange(Genotype)
    currenttraits <-
      currenttraits[colSums(is.na(dataBLUPS[, currenttraits])) != nrow(dataBLUPS[, currenttraits])]
    selectInput(
      'InputRankTraitsHigh',
      paste0("Trait(s)", ": ", "descending order is desired"),
      choices = currenttraits,
      multiple = TRUE
    )
  })
  
  output$outSliderRankIndexLow <- renderUI({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    req(input$InputEnvRankIndex)
    req(input$InputAnalysisTraits)
    currenttraits <- input$InputAnalysisTraits
    dataBLUPSAll <- dataBLUPS()
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    
    currenttraithigh <- input$InputRankTraitsHigh
    
    currentenv <- input$InputEnvRankIndex
    myEnvironment <- reactive({
      currentenv
    })
    myEnvironment <- myEnvironment()
    dataBLUPSAll <- dataBLUPS()
    dataBLUPS <- dataBLUPSAll %>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype)) %>%
      filter(Environment == myEnvironment) %>%
      select(Environment, Genotype, all_of(currenttraits)) %>%
      arrange(Genotype)
    currenttraits <-
      currenttraits[colSums(is.na(dataBLUPS[, currenttraits])) != nrow(dataBLUPS[, currenttraits])]
    if (!is.null(currenttraithigh)) {
      currenttraits <- sort(setdiff(currenttraits, currenttraithigh))
    }
    selectInput(
      'InputRankTraitsLow',
      paste0("Trait(s)", ": ", "ascending order is desired"),
      choices = currenttraits,
      multiple = TRUE
    )
  })
  
  output$outEnvBaseIndex <- renderUI({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputAnalysisTraits)
    req(input$InputEnvironment)
    
    Environment <- input$InputEnvironment
    dataBLUPSAll <- dataBLUPS()
    dataBLUPSAll %<>%
      rename(Environment = all_of(Environment))
    Env <- unique(dataBLUPSAll$Environment)
    
    if ("OVERALL" %in% Env) {
      if (length(Env) > 2) {
        Env <- sort(Env[Env != "OVERALL"])
        selectInput(
          'InputEnvBaseIndex',
          'Environment',
          choices = list("OVERALL", 'Environments' = Env)
        )
      } else {
        selectInput('InputEnvBaseIndex', 'Environment', choices = Env)
      }
      
    } else {
      selectInput('InputEnvBaseIndex', 'Environment', choices = Env)
    }
    
  })
  
  output$outTraitsBaseIndex <- renderUI({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    req(input$InputEnvBaseIndex)
    req(input$InputAnalysisTraits)
    currenttraits <- input$InputAnalysisTraits
    dataBLUPSAll <- dataBLUPS()
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    EnvironmentBase <- input$InputEnvBaseIndex
    dataBLUPSAll %<>%
      rename(Environment = all_of(Environment)) %>%
      filter(Environment == EnvironmentBase)
    currenttraits <-
      currenttraits[colSums(is.na(dataBLUPSAll[, currenttraits])) != nrow(dataBLUPSAll[, currenttraits])]
    selectInput('InputBaseTraits',
                'Trait(s)',
                choices = currenttraits,
                multiple = TRUE)
  })
  
  output$outTraitsBaseIndexSlider <- renderUI({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    req(input$InputBaseTraits)
    dataBLUPSAll <- dataBLUPS()
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currenttraits <- input$InputBaseTraits
    lapply(1:length(currenttraits), function(i) {
      sliderInput(
        paste0('TraitSliderBaseIndex', i),
        paste0('Weight', ": ", currenttraits[i]),
        min = -5,
        max = 5,
        value = 1,
        step = 0.5
      )
    })
  })
  
  
  output$outseltraitHeat <- renderUI({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputAnalysisTraits)
    currenttraits <- input$InputAnalysisTraits
    selectInput("seltraitHeat", "Trait", choices = currenttraits)
  })
  
  
  output$outseltraitBox <- renderUI({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputAnalysisTraits)
    currenttraits <- input$InputAnalysisTraits
    selectInput("seltraitBox", "Select Trait", choices = currenttraits)
  })
  
  output$outseltraitBiplot <- renderUI({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputAnalysisTraits)
    currenttraits <- input$InputAnalysisTraits
    selectInput("seltraitBiplot", "Select Trait", choices = currenttraits)
  })
  
  output$outDimX <- renderUI({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputAnalysisTraits)
    req(input$seltraitBiplot)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currenttraits <- input$InputAnalysisTraits
    currenttraitBiplot <- input$seltraitBiplot
    dataBLUPSAll <- dataBLUPS()
    datBLUPSBiplot <- dataBLUPSAll %>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype)) %>%
      select(Environment, Genotype, all_of(currenttraitBiplot)) %>%
      arrange(Genotype)
    missingBLUPS <- datBLUPSBiplot %>%
      arrange(Environment) %>%
      rename(temp = all_of(currenttraitBiplot)) %>%
      group_by(Genotype) %>%
      summarize(new = mean(temp, na.rm = TRUE))
    datBLUPSBiplot %<>%
      left_join(missingBLUPS, by = "Genotype") %>%
      rename(temp = all_of(currenttraitBiplot)) %>%
      mutate(temp = ifelse(is.na(temp), new, temp)) %>%
      select(-new) %>%
      filter(!is.na(temp))
    
    Env <- unique(datBLUPSBiplot$Environment)
    
    if ("OVERALL" %in% Env) {
      if (length(Env) > 2) {
        dTrait <- datBLUPSBiplot %>%
          filter(Environment != "OVERALL") %>%
          arrange(Environment, Genotype) %>%
          spread(key = Environment, value = temp) %>%
          column_to_rownames("Genotype")
        
        if (ncol(dTrait) > 1) {
          dTrait <- dTrait[, colSums(is.na(dTrait)) != nrow(dTrait)]
        }
        if (ncol(dTrait) == 2) {
          choix <- 1
        } else if ((ncol(dTrait) < 5)) {
          choix <- 1:(ncol(dTrait) - 1)
        } else {
          choix <- 1:4
        }
        selectInput('InputDimX', 'Dimension for the X-axis', choices = choix)
      }
    } else {
      if (length(Env) >= 2) {
        dTrait <- datBLUPSBiplot %>%
          arrange(Environment, Genotype) %>%
          spread(key = Environment, value = temp) %>%
          column_to_rownames("Genotype")
        
        if (ncol(dTrait) > 1) {
          dTrait <- dTrait[, colSums(is.na(dTrait)) != nrow(dTrait)]
        }
        if (ncol(dTrait) == 2) {
          choix <- 1
        } else if ((ncol(dTrait) < 5)) {
          choix <- 1:(ncol(dTrait) - 1)
        } else {
          choix <- 1:4
        }
        
        selectInput('InputDimX', 'Dimension for the X-axis', choices = choix)
      }
    }
  })
  
  output$outDimY <- renderUI({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputAnalysisTraits)
    req(input$seltraitBiplot)
    
    req(input$InputDimX)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currentraits <- input$InputAnalysisTraits
    currenttraitBiplot <- input$seltraitBiplot
    dataBLUPSAll <- dataBLUPS()
    datBLUPSBiplot <- dataBLUPSAll %>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype)) %>%
      select(Environment, Genotype, all_of(currenttraitBiplot)) %>%
      arrange(Genotype)
    missingBLUPS <- datBLUPSBiplot %>%
      arrange(Environment) %>%
      rename(temp = all_of(currenttraitBiplot)) %>%
      group_by(Genotype) %>%
      summarize(new = mean(temp, na.rm = TRUE))
    datBLUPSBiplot %<>%
      left_join(missingBLUPS, by = "Genotype") %>%
      rename(temp = all_of(currenttraitBiplot)) %>%
      mutate(temp = ifelse(is.na(temp), new, temp)) %>%
      select(-new) %>%
      filter(!is.na(temp))
    
    Env <- unique(datBLUPSBiplot$Environment)
    
    if ("OVERALL" %in% Env) {
      if (length(Env) > 2) {
        dTrait <- datBLUPSBiplot %>%
          filter(Environment != "OVERALL") %>%
          arrange(Environment, Genotype) %>%
          spread(key = Environment, value = temp) %>%
          column_to_rownames("Genotype")
        
        if (ncol(dTrait) > 1) {
          dTrait <- dTrait[, colSums(is.na(dTrait)) != nrow(dTrait)]
        }
        
        if ((ncol(dTrait) < 5)) {
          choix <- 1:ncol(dTrait)
          choix <- choix[choix > input$InputDimX]
        } else {
          choix <- 1:5
          choix <- choix[choix > input$InputDimX]
        }
        
        selectInput('InputDimY', 'Dimension for the Y-axis', choices = choix)
      }
    } else {
      if (length(Env) >= 2) {
        dTrait <- datBLUPSBiplot %>%
          arrange(Environment, Genotype) %>%
          spread(key = Environment, value = temp) %>%
          column_to_rownames("Genotype")
        
        if (ncol(dTrait) > 1) {
          dTrait <- dTrait[, colSums(is.na(dTrait)) != nrow(dTrait)]
        }
        
        if ((ncol(dTrait) < 5)) {
          choix <- 1:ncol(dTrait)
          choix <- choix[choix > input$InputDimX]
        } else {
          choix <- 1:5
          choix <- choix[choix > input$InputDimX]
        }
        
        selectInput('InputDimY', 'Dimension for the Y-axis', choices = choix)
      }
    }
    
  })
  
  
  observeEvent(input$btnPreview, {
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputAnalysisTraits)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currenttraits <- input$InputAnalysisTraits
    dataBLUPSAll <- dataBLUPS()
    dataBLUPSAll %<>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype)) %>%
      select(Environment, Genotype, all_of(currenttraits)) %>%
      arrange(Environment, Genotype)
    
    
    dataBLUPSAllDisplay <- dataBLUPSAll
    for (i in 1:length(currenttraits)) {
      dataBLUPSAllDisplay %<>%
        rename(temp = currenttraits[i]) %>%
        mutate(temp = replace_na(as.character(temp), "")) %>%
        rename_at(vars("temp"),
                  list( ~ str_replace(., ., currenttraits[i])))
    }
    
    summaryDat <- NULL
    for (i in 1:length(currenttraits)) {
      temp <- dataBLUPSAll %>%
        select(Environment, currenttraits[i]) %>%
        rename(temp = currenttraits[i]) %>%
        group_by(Environment) %>%
        summarize(
          Min = min(temp, na.rm = TRUE),
          Mean = mean(temp, na.rm = TRUE),
          SD = sd(temp, na.rm = TRUE),
          Median = median(temp, na.rm = TRUE),
          Max = max(temp, na.rm = TRUE)
        ) %>%
        select(-Environment)
      temp <-
        data.frame(Environment = unique(dataBLUPSAll$Environment),
                   Trait = currenttraits[i],
                   temp)
      summaryDat <- rbind(summaryDat, temp)
    }
    summaryDat$SD[is.na(summaryDat$SD)] <- ""
    summaryDat$Min[is.na(summaryDat$Min)] <- ""
    summaryDat$Mean[is.na(summaryDat$Mean)] <- ""
    summaryDat$Median[is.na(summaryDat$Median)] <- ""
    summaryDat$Max[is.na(summaryDat$Max)] <- ""
    summaryDat$SD[summaryDat$SD == "-Inf"] <- ""
    summaryDat$Min[summaryDat$Min == "-Inf"] <- ""
    summaryDat$Mean[summaryDat$Mean == "-Inf"] <- ""
    summaryDat$Median[summaryDat$median == "-Inf"] <- ""
    summaryDat$Max[summaryDat$Max == "-Inf"] <- ""
    summaryDat$SD[summaryDat$SD == "Inf"] <- ""
    summaryDat$Min[summaryDat$Min == "Inf"] <- ""
    summaryDat$Mean[summaryDat$Mean == "Inf"] <- ""
    summaryDat$Median[summaryDat$median == "Inf"] <- ""
    summaryDat$Max[summaryDat$Max == "Inf"] <- ""
    summaryDat$Max[summaryDat$Max == "Ibn"] <- ""
    
    output$AllValues <- renderText({
      if (hideAll$clearAll)
        return()
      else
        dataBLUPSAllDisplay %>%
        rename_at(vars(one_of(sort(
          c("Environment", "Genotype")
        ))),
        list( ~ str_replace(., ., c(
          Environment, Genotype
        )))) %>%
        kable(format = "html", escape = F) %>%
        kable_styling(
          bootstrap_options = c("hover", "condensed", "responsive"),
          fixed_thead = TRUE
        )
    })
    
    output$Summary <- renderText({
      if (hideAll$clearAll)
        return()
      else
        summaryDat %>%
        rename_at(vars("Environment"),
                  list( ~ str_replace(., ., Environment))) %>%
        kable(format = "html", escape = F) %>%
        kable_styling(
          bootstrap_options = c("hover", "condensed", "responsive"),
          fixed_thead = TRUE
        )
    })
    hideAll$clearAll <- FALSE
  })
  
  output$CodesEnvBoxPlot <- renderText({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$seltraitBox)
    req(input$InputAnalysisTraits)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currenttraits <- input$InputAnalysisTraits
    currenttraitBoxplot <- input$seltraitBox
    dataBLUPSAll <- dataBLUPS()
    
    dTrait <- dataBLUPSAll %>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype)) %>%
      select(Environment, Genotype, all_of(currenttraitBoxplot)) %>%
      as.data.frame()
    nE <- sort(unique(dTrait$Environment))
    
    Environment_c <- setNames(paste0("E", seq(nE)), nE)
    
    E_c <-
      tibble(Code = Environment_c, Environment = names(Environment_c)) %>%
      rename_at(vars("Environment"),
                list( ~ str_replace(., ., Environment))) %>%
      kable(format = "html", escape = F) %>%
      kable_styling(
        bootstrap_options = c("hover", "condensed", "responsive"),
        fixed_thead = TRUE
      )
  })
  
  output$BoxPlot <- renderPlot({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$seltraitBox)
    req(input$InputAnalysisTraits)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currenttraits <- input$InputAnalysisTraits
    currenttraitBoxplot <- input$seltraitBox
    dataBLUPSAll <- dataBLUPS()
    
    dTrait <- dataBLUPSAll %>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype)) %>%
      select(Environment, Genotype, all_of(currenttraitBoxplot)) %>%
      as.data.frame()
    
    nE <- sort(unique(dTrait$Environment))
    nG <- sort(unique(dTrait$Genotype))
    Environment_c <- setNames(paste0("E", seq(nE)), nE)
    Genotype_c <- setNames(paste0("G", seq(nG)), nG)
    dTrait %<>%
      mutate(
        Environment_n = recode(Environment,!!!Environment_c),
        Genotype_n = recode(Genotype,!!!Genotype_c)
      )
    p <-
      ggplot(dTrait,
             aes(x = Environment_n, y = dTrait[, 3], fill = Environment_n)) +
      geom_boxplot() +
      labs(x = "Environment", y = currenttraitBoxplot) +
      theme(panel.background = element_rect(fill = "white", colour = "grey50"))
    p <- p + theme(legend.position = "none")
    p
  })
  
  output$dwlBoxPlotui <- renderUI({
    if (hideAll$clearAll)
      return()
    else
      downloadButton("dwlBoxPlot", "Download")
  })
  
  output$dwlBoxPlot <- downloadHandler(
    content = function(file) {
      if (hideAll$clearAll)
        return()
      else
        req(input$fileUp)
      req(input$selSheetBLUPS)
      req(input$seltraitBox)
      req(input$InputAnalysisTraits)
      req(input$InputGenotype)
      req(input$InputEnvironment)
      Genotype <- input$InputGenotype
      Environment <- input$InputEnvironment
      currenttraits <- input$InputAnalysisTraits
      currenttraitBoxplot <- input$seltraitBox
      dataBLUPSAll <- dataBLUPS()
      
      dTrait <- dataBLUPSAll %>%
        rename(Environment = all_of(Environment),
               Genotype = all_of(Genotype)) %>%
        select(Environment, Genotype, all_of(currenttraitBoxplot)) %>%
        as.data.frame()
      
      nE <- sort(unique(dTrait$Environment))
      nG <- sort(unique(dTrait$Genotype))
      Environment_c <- setNames(paste0("E", seq(nE)), nE)
      Genotype_c <- setNames(paste0("G", seq(nG)), nG)
      dTrait %<>%
        mutate(
          Environment_n = recode(Environment,!!!Environment_c),
          Genotype_n = recode(Genotype,!!!Genotype_c)
        )
      p <-
        ggplot(dTrait,
               aes(x = Environment_n, y = dTrait[, 3], fill = Environment_n)) +
        geom_boxplot() +
        labs(x = "Environment", y = currenttraitBoxplot) +
        theme(panel.background = element_rect(fill = "white", colour = "grey50"))
      p <- p + theme(legend.position = "none")
      # print(names(dTrait)[3],)
      png(file)
      print(p)
      dev.off()
    },
    filename = function() {
      req(input$fileUp)
      req(input$selSheetBLUPS)
      req(input$seltraitBox)
      req(input$InputAnalysisTraits)
      req(input$InputGenotype)
      req(input$InputEnvironment)
      Genotype <- input$InputGenotype
      Environment <- input$InputEnvironment
      currenttraits <- input$InputAnalysisTraits
      currenttraitBoxplot <- input$seltraitBox
      dataBLUPSAll <- dataBLUPS()
      
      dTrait <- dataBLUPSAll %>%
        rename(Environment = all_of(Environment),
               Genotype = all_of(Genotype)) %>%
        select(Environment, Genotype, all_of(currenttraitBoxplot)) %>%
        as.data.frame()
      paste0("BoxPlot-",
             names(dTrait)[3],
             "-",
             gsub(" ", "_", gsub(":", ".", Sys.time())),
             ".png")
    }
  )
  
  output$CodesEnvHeat <- renderText({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$seltraitHeat)
    req(input$InputAnalysisTraits)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currenttraits <- input$InputAnalysisTraits
    currenttraitHeat <- input$seltraitHeat
    dataBLUPSAll <- dataBLUPS()
    
    dTrait <- dataBLUPSAll %>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype)) %>%
      filter(Environment != "OVERALL") %>%
      select(Environment, Genotype, all_of(currenttraitHeat)) %>%
      as.data.frame()
    nE <- sort(unique(dTrait$Environment))
    Environment_c <- setNames(paste0("E", seq(nE)), nE)
    
    E_c <-
      tibble(Code = Environment_c, Environment = names(Environment_c)) %>%
      rename_at(vars("Environment"),
                list( ~ str_replace(., ., Environment))) %>%
      kable(format = "html", escape = F) %>%
      kable_styling(
        bootstrap_options = c("hover", "condensed", "responsive"),
        fixed_thead = TRUE
      )
  })
  
  output$CodesGenoHeat <- renderText({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$seltraitHeat)
    req(input$InputAnalysisTraits)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currenttraits <- input$InputAnalysisTraits
    currenttraitHeat <- input$seltraitHeat
    dataBLUPSAll <- dataBLUPS()
    
    dTrait <- dataBLUPSAll %>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype)) %>%
      filter(Environment != "OVERALL") %>%
      select(Environment, Genotype, all_of(currenttraitHeat)) %>%
      as.data.frame()
    nG <- sort(unique(dTrait$Genotype))
    Genotype_c <- setNames(paste0("G", seq(nG)), nG)
    
    G_c <-
      tibble(Code = Genotype_c, Genotype = names(Genotype_c)) %>%
      rename_at(vars("Genotype"),
                list( ~ str_replace(., ., Genotype))) %>%
      kable(format = "html", escape = F) %>%
      kable_styling(
        bootstrap_options = c("hover", "condensed", "responsive"),
        fixed_thead = TRUE
      )
  })
  
  output$TableHeatmap <- renderText({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$seltraitHeat)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currenttraitHeat <- input$seltraitHeat
    dataBLUPSAll <- dataBLUPS()
    dataBLUPSheat <- dataBLUPSAll %>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype)) %>%
      select(Environment, Genotype, all_of(currenttraitHeat)) %>%
      arrange(Genotype)
    missingBLUPS <- dataBLUPSheat %>%
      arrange(Environment) %>%
      rename(temp = all_of(currenttraitHeat)) %>%
      group_by(Genotype) %>%
      summarize(new = mean(temp, na.rm = TRUE))
    missinGeno <- missingBLUPS %>%
      filter(is.na(new)) %>%
      select(Genotype) %>%
      as_vector()
    dataBLUPSheat %<>%
      filter(Environment != "OVERALL") %>%
      filter (!(Genotype %in% missinGeno)) %>%
      left_join(missingBLUPS, by = "Genotype") %>%
      rename(temp = all_of(currenttraitHeat)) %>%
      mutate(temp = ifelse(is.na(temp), new, temp)) %>%
      select(-new)
    
    nE <- sort(unique(dataBLUPSheat$Environment))
    nG <- sort(unique(dataBLUPSheat$Genotype))
    Environment_c <- setNames(paste0("E", seq(nE)), nE)
    Genotype_c <- setNames(paste0("G", seq(nG)), nG)
    dataBLUPSheat %<>%
      mutate(
        Environment_n = recode(Environment,!!!Environment_c),
        Genotype_n = recode(Genotype,!!!Genotype_c)
      )
    
    dTrait <- dataBLUPSheat %>%
      select(Environment_n, Genotype_n, temp) %>%
      mutate_at(vars(temp), round, 1) %>%
      arrange(Environment_n, Genotype_n) %>%
      spread(key = Environment_n, value = temp)
    
    dTrait1 <-  dTrait %>%
      mutate_if(is.numeric, function(x) {
        x = cell_spec(
          round(x, 1),
          color = "white",
          bold = T,
          background = spec_color(
            x,
            end = 0.9,
            option = "E",
            direction = -1
          )
        )
      }) %>%
      mutate(Genotype = Genotype_n) %>%
      select(-Genotype_n) %>%
      select(Genotype, everything())
    dTrait1 %>%
      kable(escape = F, align = "c") %>%
      kable_styling(c("striped", "condensed"), full_width = F)
    
  })
  
  
  output$CodesEnvBiplot <- renderText({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputAnalysisTraits)
    req(input$seltraitBiplot)
    
    req(input$InputGenotype)
    req(input$InputEnvironment)
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currenttraits <- input$InputAnalysisTraits
    currenttraitBiplot <- input$seltraitBiplot
    dataBLUPSAll <- dataBLUPS()
    
    datBLUPSBiplot <- dataBLUPSAll %>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype)) %>%
      filter(Environment != "OVERALL") %>%
      select(Environment, Genotype, all_of(currenttraitBiplot)) %>%
      as.data.frame()
    
    nE <- sort(unique(datBLUPSBiplot$Environment))
    Environment_c <- setNames(paste0("E", seq(nE)), nE)
    
    E_c <-
      tibble(Code = Environment_c, Environment = names(Environment_c)) %>%
      rename_at(vars("Environment"),
                list( ~ str_replace(., ., Environment))) %>%
      kable(format = "html", escape = F) %>%
      kable_styling(
        bootstrap_options = c("hover", "condensed", "responsive"),
        fixed_thead = TRUE
      )
    
  })
  
  output$CodesGenoBiPlot <- renderText({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$seltraitBiplot)
    req(input$InputAnalysisTraits)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currenttraits <- input$InputAnalysisTraits
    currenttraitBiplot <- input$seltraitBiplot
    dataBLUPSAll <- dataBLUPS()
    
    datBLUPSBiplot <- dataBLUPSAll %>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype)) %>%
      filter(Environment != "OVERALL") %>%
      select(Environment, Genotype, all_of(currenttraitBiplot)) %>%
      as.data.frame()
    
    nG <- sort(unique(datBLUPSBiplot$Genotype))
    Genotype_c <- setNames(paste0("G", seq(nG)), nG)
    
    G_c <-
      tibble(Code = Genotype_c, Genotype = names(Genotype_c)) %>%
      rename_at(vars("Genotype"),
                list( ~ str_replace(., ., Genotype))) %>%
      kable(format = "html", escape = F) %>%
      kable_styling(
        bootstrap_options = c("hover", "condensed", "responsive"),
        fixed_thead = TRUE
      )
  })
  
  output$BiPlot <- renderPlot({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputAnalysisTraits)
    req(input$seltraitBiplot)
    req(input$InputDimX)
    req(input$InputDimY)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currenttraitBiplot <- input$seltraitBiplot
    currentDimX <- input$InputDimX
    currentDimY <- input$InputDimY
    currenttraits <- input$InputAnalysisTraits
    dataBLUPSAll <- dataBLUPS()
    
    datBLUPSBiplot <- dataBLUPSAll %>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype)) %>%
      select(Environment, Genotype, all_of(currenttraitBiplot)) %>%
      arrange(Genotype)
    
    Env <- unique(datBLUPSBiplot$Environment)
    
    if ("OVERALL" %in% Env) {
      datBLUPSBiplot %<>%
        filter(Environment != "OVERALL")
    }
    
    nE <- sort(unique(datBLUPSBiplot$Environment))
    nG <- sort(unique(datBLUPSBiplot$Genotype))
    Environment_c <- setNames(paste0("E", seq(nE)), nE)
    Genotype_c <- setNames(paste0("G", seq(nG)), nG)
    EnvNULL <- datBLUPSBiplot %>%
      select(Environment, all_of(currenttraitBiplot)) %>%
      rename(temp = all_of(currenttraitBiplot)) %>%
      group_by(Environment) %>%
      summarize(Count = sum(!is.na(temp))) %>%
      filter(Count == 0) %>%
      select(Environment) %>%
      as_vector()
    datBLUPSBiplot <- datBLUPSBiplot %>%
      filter(!(Environment %in% EnvNULL))
    Environment_c <-
      Environment_c[names(Environment_c) %in% unique(datBLUPSBiplot$Environment)]
    missingBLUPS <- datBLUPSBiplot %>%
      arrange(Environment) %>%
      rename(temp = all_of(currenttraitBiplot)) %>%
      group_by(Genotype) %>%
      summarize(new = mean(temp, na.rm = TRUE))
    datBLUPSBiplot %<>%
      left_join(missingBLUPS, by = "Genotype") %>%
      rename(temp = all_of(currenttraitBiplot)) %>%
      mutate(temp = ifelse(is.na(temp), new, temp)) %>%
      select(-new) %>%
      filter(!is.na(temp))
    
    datBLUPSBiplot %<>%
      mutate(
        Environment_n = recode(Environment,!!!Environment_c),
        Genotype_n = recode(Genotype,!!!Genotype_c)
      )
    dTrait <- datBLUPSBiplot %>%
      select(Environment_n, Genotype_n, temp) %>%
      arrange(Environment_n, Genotype_n) %>%
      spread(key = Environment_n, value = temp) %>%
      column_to_rownames("Genotype_n") %>%
      na.omit()
    
    dTrait <- Filter(var, dTrait)
    if (ncol(dTrait) > 1) {
      dTrait <- dTrait[, colSums(is.na(dTrait)) != nrow(dTrait)]
      pca <- prcomp(dTrait, scale = TRUE, center = TRUE)
      yX <- as.numeric(currentDimX)
      yY <- as.numeric(currentDimY)
      
      fviz_pca_biplot(
        pca,
        axes = c(yX, yY),
        col.ind = "blue",
        col.var = "red",
        title = ""
      )
    }
    
  })
  
  output$dwlBiPlotui <- renderUI({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputAnalysisTraits)
    req(input$seltraitBiplot)
    req(input$InputDimX)
    req(input$InputDimY)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currenttraitBiplot <- input$seltraitBiplot
    currentDimX <- input$InputDimX
    currentDimY <- input$InputDimY
    currenttraits <- input$InputAnalysisTraits
    dataBLUPSAll <- dataBLUPS()
    
    datBLUPSBiplot <- dataBLUPSAll %>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype)) %>%
      select(Environment, Genotype, all_of(currenttraitBiplot)) %>%
      arrange(Genotype)
    
    Env <- unique(datBLUPSBiplot$Environment)
    
    if (hideAll$clearAll)
      return()
    else
      if ("OVERALL" %in% Env) {
        if (length(Env) > 2) {
          downloadButton("dwlBiPlot", "Download")
        }
      } else {
        if (length(Env) > 1) {
          downloadButton("dwlBiPlot", "Download")
        }
      }
    
  })
  
  output$dwlBiPlot <- downloadHandler(
    content = function(file) {
      if (hideAll$clearAll)
        return()
      else
        req(input$fileUp)
      req(input$selSheetBLUPS)
      req(input$InputAnalysisTraits)
      req(input$seltraitBiplot)
      req(input$InputDimX)
      req(input$InputDimY)
      req(input$InputGenotype)
      req(input$InputEnvironment)
      Genotype <- input$InputGenotype
      Environment <- input$InputEnvironment
      currenttraitBiplot <- input$seltraitBiplot
      currentDimX <- input$InputDimX
      currentDimY <- input$InputDimY
      currenttraits <- input$InputAnalysisTraits
      dataBLUPSAll <- dataBLUPS()
      
      datBLUPSBiplot <- dataBLUPSAll %>%
        rename(Environment = all_of(Environment),
               Genotype = all_of(Genotype)) %>%
        select(Environment, Genotype, all_of(currenttraitBiplot)) %>%
        arrange(Genotype)
      
      Env <- unique(datBLUPSBiplot$Environment)
      
      if ("OVERALL" %in% Env) {
        datBLUPSBiplot %<>%
          filter(Environment != "OVERALL")
      }
      
      nE <- sort(unique(datBLUPSBiplot$Environment))
      nG <- sort(unique(datBLUPSBiplot$Genotype))
      Environment_c <- setNames(paste0("E", seq(nE)), nE)
      Genotype_c <- setNames(paste0("G", seq(nG)), nG)
      EnvNULL <- datBLUPSBiplot %>%
        select(Environment, all_of(currenttraitBiplot)) %>%
        rename(temp = all_of(currenttraitBiplot)) %>%
        group_by(Environment) %>%
        summarize(Count = sum(!is.na(temp))) %>%
        filter(Count == 0) %>%
        select(Environment) %>%
        as_vector()
      datBLUPSBiplot <- datBLUPSBiplot %>%
        filter(!(Environment %in% EnvNULL))
      Environment_c <-
        Environment_c[names(Environment_c) %in% unique(datBLUPSBiplot$Environment)]
      missingBLUPS <- datBLUPSBiplot %>%
        arrange(Environment) %>%
        rename(temp = all_of(currenttraitBiplot)) %>%
        group_by(Genotype) %>%
        summarize(new = mean(temp, na.rm = TRUE))
      datBLUPSBiplot %<>%
        left_join(missingBLUPS, by = "Genotype") %>%
        rename(temp = all_of(currenttraitBiplot)) %>%
        mutate(temp = ifelse(is.na(temp), new, temp)) %>%
        select(-new) %>%
        filter(!is.na(temp))
      
      datBLUPSBiplot %<>%
        mutate(
          Environment_n = recode(Environment,!!!Environment_c),
          Genotype_n = recode(Genotype,!!!Genotype_c)
        )
      dTrait <- datBLUPSBiplot %>%
        select(Environment_n, Genotype_n, temp) %>%
        arrange(Environment_n, Genotype_n) %>%
        spread(key = Environment_n, value = temp) %>%
        column_to_rownames("Genotype_n") %>%
        na.omit()
      
      png(file)
      
      if (ncol(dTrait) > 1) {
        dTrait <- dTrait[, colSums(is.na(dTrait)) != nrow(dTrait)]
        pca <- prcomp(dTrait, scale = TRUE, center = TRUE)
        yX <- as.numeric(currentDimX)
        yY <- as.numeric(currentDimY)
        
        print(fviz_pca_biplot(
          pca,
          axes = c(yX, yY),
          col.ind = "blue",
          col.var = "red",
          title = ""
        ))
      }
      
      dev.off()
    },
    
    filename = function() {
      req(input$fileUp)
      req(input$selSheetBLUPS)
      req(input$seltraitBiplot)
      req(input$InputAnalysisTraits)
      req(input$InputGenotype)
      req(input$InputEnvironment)
      req(input$InputDimX)
      req(input$InputDimY)
      Genotype <- input$InputGenotype
      Environment <- input$InputEnvironment
      currenttraits <- input$InputAnalysisTraits
      currenttraitBiplot <- input$seltraitBiplot
      
      paste0("BiPlot-",
             currenttraitBiplot,
             "-",
             gsub(" ", "_", gsub(":", ".", Sys.time())),
             ".png")
    }
  )
  
  output$CodesEnvScater <- renderText({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputAnalysisTraits)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currenttraits <- input$InputAnalysisTraits
    dataBLUPSAll <- dataBLUPS()
    
    dataBLUPSAll %<>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype)) %>%
      select(Environment, Genotype, all_of(currenttraits)) %>%
      na.omit() %>%
      as.data.frame()
    nE <- sort(unique(dataBLUPSAll$Environment))
    Environment_c <- setNames(paste0("E", seq(nE)), nE)
    
    E_c <-
      tibble(Code = Environment_c, Environment = names(Environment_c)) %>%
      rename_at(vars("Environment"),
                list( ~ str_replace(., ., Environment))) %>%
      kable(format = "html", escape = F) %>%
      kable_styling(
        bootstrap_options = c("hover", "condensed", "responsive"),
        fixed_thead = TRUE
      )
  })
  
  output$CodesGenoScater <- renderText({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputAnalysisTraits)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currenttraits <- input$InputAnalysisTraits
    dataBLUPSAll <- dataBLUPS()
    
    dataBLUPSAll %<>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype)) %>%
      select(Environment, Genotype, all_of(currenttraits)) %>%
      na.omit() %>%
      as.data.frame()
    
    nG <- sort(unique(dataBLUPSAll$Genotype))
    Genotype_c <- setNames(paste0("G", seq(nG)), nG)
    
    G_c <-
      tibble(Code = Genotype_c, Genotype = names(Genotype_c)) %>%
      rename_at(vars("Genotype"),
                list( ~ str_replace(., ., Genotype))) %>%
      kable(format = "html", escape = F) %>%
      kable_styling(
        bootstrap_options = c("hover", "condensed", "responsive"),
        fixed_thead = TRUE
      )
  })
  
  
  output$OneTraitInfo <- renderText({
    req(input$InputAnalysisTraits)
    if (hideAll$clearAll)
      return()
    else
      if (length(input$InputAnalysisTraits) < 2) {
        print("You need more than one trait.")
      }
  })
  
  output$ScatterPlot <- renderPlot({
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputAnalysisTraits)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currenttraits <- input$InputAnalysisTraits
    dataBLUPSAll <- dataBLUPS()
    dataBLUPSAll %<>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype)) %>%
      select(Environment, Genotype, all_of(currenttraits)) %>%
      na.omit()
    
    nE <- sort(unique(dataBLUPSAll$Environment))
    nG <- sort(unique(dataBLUPSAll$Genotype))
    Environment_c <- setNames(paste0("E", seq(nE)), nE)
    Genotype_c <- setNames(paste0("G", seq(nG)), nG)
    
    dataBLUPSAll %<>%
      mutate(
        Environment = recode(Environment,!!!Environment_c),
        Genotype = recode(Genotype,!!!Genotype_c)
      )
    
    if (length(currenttraits) > 1) {
      p <-
        ggscatmat(
          dataBLUPSAll,
          columns = 3:length(dataBLUPSAll),
          color = "Environment",
          alpha = 0.8
        )
      p <- p + labs(x = "", y = "")
      p <- p + theme_bw()
      p
    }
    
  })
  
  output$dwlScatterPlotui <- renderUI({
    if (hideAll$clearAll)
      return()
    else
      if (length(input$InputAnalysisTraits) > 1)
        downloadButton("dwlScatterPlot", "Download")
  })
  
  output$dwlScatterPlot <- downloadHandler(
    content = function(file) {
      if (hideAll$clearAll)
        return()
      else
        req(input$fileUp)
      req(input$selSheetBLUPS)
      req(input$InputAnalysisTraits)
      req(input$InputGenotype)
      req(input$InputEnvironment)
      Genotype <- input$InputGenotype
      Environment <- input$InputEnvironment
      currenttraits <- input$InputAnalysisTraits
      dataBLUPSAll <- dataBLUPS()
      dataBLUPSAll %<>%
        rename(Environment = all_of(Environment),
               Genotype = all_of(Genotype)) %>%
        select(Environment, Genotype, all_of(currenttraits)) %>%
        na.omit()
      
      nE <- sort(unique(dataBLUPSAll$Environment))
      nG <- sort(unique(dataBLUPSAll$Genotype))
      Environment_c <- setNames(paste0("E", seq(nE)), nE)
      Genotype_c <- setNames(paste0("G", seq(nG)), nG)
      
      dataBLUPSAll %<>%
        mutate(
          Environment = recode(Environment,!!!Environment_c),
          Genotype = recode(Genotype,!!!Genotype_c)
        )
      
      png(file)
      
      if (length(currenttraits) > 1) {
        p <-
          ggscatmat(
            dataBLUPSAll,
            columns = 3:length(dataBLUPSAll),
            color = "Environment",
            alpha = 0.8
          )
        p <- p + labs(x = "", y = "")
        p <- p + theme_bw()
        p
      }
      
      print(p)
      dev.off()
    },
    
    filename = function() {
      paste0("ScatterPlot-", gsub(" ", "_", gsub(":", ".", Sys.time())), ".png")
    }
  )
  
  
  observeEvent(input$btnCompareChecks, {
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputAnalysisTraits)
    req(input$InputEnvChecks)
    req(input$InputChecks)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    req(
      !is.null(input$InputHighTraitsChecks) |
        !is.null(input$InputRangeTraitsChecks) |
        !is.null(input$InputLowTraitsChecks)
    )
    currentenv <- input$InputEnvChecks
    currentchecks <- input$InputChecks %>%
      sort()
    
    currenttraits <- input$InputAnalysisTraits
    currenttraithigh <- input$InputHighTraitsChecks
    currenttraitlow <- input$InputLowTraitsChecks
    currenttraitrange <- input$InputRangeTraitsChecks
    myEnvironment <- reactive({
      currentenv
    })
    myEnvironment <- myEnvironment()
    dataBLUPSAll <- dataBLUPS()
    dataBLUPSAll %<>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype))
    
    dataBLUPS <- dataBLUPSAll %>%
      filter(Environment == myEnvironment) %>%
      select(Environment, Genotype, all_of(currenttraits)) %>%
      arrange(Genotype)
    
    currenttraits <-
      currenttraits[colSums(is.na(dataBLUPS[, currenttraits])) != nrow(dataBLUPS[, currenttraits])]
    
    currenttraithigh <-
      sort(intersect(currenttraithigh, currenttraits))
    currenttraitlow <-
      sort(intersect(currenttraitlow, currenttraits))
    currenttraitrange <-
      sort(intersect(currenttraitrange, currenttraits))
    TraitnoRange <- sort(c(currenttraithigh, currenttraitlow))
    TraitRange <- sort(currenttraitrange)
    Traits <- sort(c(TraitnoRange, TraitRange))
    
    values <- dataBLUPS %>%
      filter(Genotype %in% currentchecks) %>%
      select(all_of(currenttraits)) %>%
      summarise_all(mean, na.rm = T)
    
    if (length(TraitRange) != 0) {
      range <- matrix(0, length(TraitRange), 2)
      for (i in 1:length(TraitRange)) {
        rangeS <- paste0('input$', 'TraitSliderChecksR', i)
        range[i,] <- sapply(rangeS, function(x)
          eval(parse(text = x)))
      }
    }
    
    if (!is.null(input$InputHighTraitsChecks)) {
      output$TableChecksHighInfo <- renderText({
        req(input$InputHighTraitsChecks)
        if (hideAll$clearAll)
          return()
        else
          "Genotypes above the performance of the check(s) are highlighted in green or red, otherwise."
      })
      
      output$TableChecksHigh <- renderText({
        req(input$InputHighTraitsChecks)
        if (hideAll$clearAll)
          return()
        else
          Checks <- dataBLUPS %>%
            filter(Genotype %in% currentchecks) %>%
            select(c(Genotype, all_of(currenttraits)))
        ValuesMean <- c("Mean", values)
        names(ValuesMean) <- names(Checks)
        Checks <- rbind(Checks, ValuesMean)
        colnames(Checks)[1] <- "Check(s)"
        if (length(currentchecks) == 1) {
          Checks <- Checks[Checks[, 1] != "Mean",]
        }
        Checks %<>%
          select(c("Check(s)", all_of(currenttraithigh)))
        ifelse(
          length(currentchecks) == 1,
          colnames(Checks)[1] <- "Check",
          colnames(Checks)[1] <- "Checks"
        )
        Checks[, currenttraithigh] %<>%
          round(2)
        Checks %>%
          kable(format = "html", escape = F) %>%
          kable_styling(
            bootstrap_options = c("striped", "hover", "condensed", "responsive"),
            fixed_thead = TRUE
          )
      })
    }
    
    if (!is.null(input$InputLowTraitsChecks)) {
      output$TableChecksLowInfo <- renderText({
        req(input$InputLowTraitsChecks)
        if (hideAll$clearAll)
          return()
        else
          "Genotypes below the performance of the check(s) are highlighted in green or red, otherwise."
      })
      
      output$TableChecksLow <- renderText({
        req(input$InputLowTraitsChecks)
        if (hideAll$clearAll)
          return()
        else
          Checks <- dataBLUPS %>%
            filter(Genotype %in% currentchecks) %>%
            select(c(Genotype, all_of(currenttraits)))
        ValuesMean <- c("Mean", values)
        names(ValuesMean) <- names(Checks)
        Checks <- rbind(Checks, ValuesMean)
        colnames(Checks)[1] <- "Check(s)"
        if (length(currentchecks) == 1) {
          Checks <- Checks[Checks[, 1] != "Mean",]
        }
        Checks %<>%
          select(c("Check(s)", all_of(currenttraitlow)))
        ifelse(
          length(currentchecks) == 1,
          colnames(Checks)[1] <- "Check",
          colnames(Checks)[1] <- "Checks"
        )
        Checks[, currenttraitlow] %<>%
          round(2)
        Checks %>%
          kable(format = "html", escape = F) %>%
          kable_styling(
            bootstrap_options = c("striped", "hover", "condensed", "responsive"),
            fixed_thead = TRUE
          )
      })
    }
    
    
    if (!is.null(input$InputRangeTraitsChecks)) {
      output$TableChecksRInfo <- renderText({
        req(input$InputRangeTraitsChecks)
        if (hideAll$clearAll)
          return()
        else
          "Genotypes within range values are highlighted in green or red, otherwise."
      })
      
      output$TableChecksR <- renderText({
        req(input$InputRangeTraitsChecks)
        if (hideAll$clearAll)
          return()
        else
          rangeMin <- range[, 1]
        rangeMax <- range[, 2]
        ChecksR <-
          data.frame(TraitRange, round(rangeMin, 2), round(rangeMax, 2))
        colnames(ChecksR) <- c("Trait", "Lowest", "Highest")
        ChecksR %>%
          kable(format = "html", escape = F) %>%
          kable_styling(
            bootstrap_options = c("striped", "hover", "condensed", "responsive"),
            fixed_thead = TRUE
          )
      })
    }
    
    
    if (length(currenttraithigh) != 0) {
      IndexSatisfyHigh <- dataBLUPSHigh <- dataBLUPSAll %>%
        select(Environment, Genotype, all_of(currenttraithigh))
      valuesHigh <- values[, currenttraithigh]
      
      for (i in 1:length(currenttraithigh)) {
        j <- i + 2
        IndexSatisfyHigh[, j] <-
          as.vector(ifelse(dataBLUPSHigh[, j] > as.numeric(valuesHigh[1, i]), 1, 0))
      }
    }
    
    if (length(currenttraitlow) != 0) {
      IndexSatisfyInf <- dataBLUPSLow <- dataBLUPSAll %>%
        select(Environment, Genotype, all_of(currenttraitlow))
      valuesLow <- values[, currenttraitlow]
      for (i in 1:length(currenttraitlow)) {
        j <- i + 2
        IndexSatisfyInf[, j] <-
          as.vector(ifelse(dataBLUPSLow[, j] <= as.numeric(valuesLow[1, i]), 1, 0))
      }
    }
    
    if (length(currenttraitrange) != 0) {
      IndexSatisfyRange <- dataBLUPSRange <- dataBLUPSAll %>%
        select(Environment, Genotype, all_of(currenttraitrange))
      for (i in 1:length(currenttraitrange)) {
        j <- i + 2
        IndexSatisfyRange[, j] <-
          as.vector(ifelse((dataBLUPSRange[, j] <= range[i, 2] &
                              dataBLUPSRange[, j] >= range[i, 1]),
                           1,
                           0
          ))
      }
    }
    
    if (length(currenttraithigh) != 0 &
        length(currenttraitlow) == 0 & length(currenttraitrange) == 0) {
      traits <- currenttraithigh
      IndexSatisfy <- dataBLUPSAll %>%
        select(Environment, Genotype) %>%
        left_join(IndexSatisfyHigh) %>%
        select (Environment, Genotype, all_of(traits))
      traitsTest <- paste0(traits, ".Test")
    } else if (length(currenttraithigh) != 0 &
               length(currenttraitlow) != 0 & length(currenttraitrange) == 0) {
      traits <- sort(c(currenttraithigh, currenttraitlow))
      IndexSatisfy <- dataBLUPSAll %>%
        select(Environment, Genotype) %>%
        left_join(IndexSatisfyHigh) %>%
        left_join(IndexSatisfyInf) %>%
        select (Environment, Genotype, all_of(traits))
      traitsTest <- paste0(traits, ".Test")
    } else if (length(currenttraithigh) != 0 &
               length(currenttraitlow) == 0 & length(currenttraitrange) != 0) {
      traits <- sort(c(currenttraitrange, currenttraithigh))
      IndexSatisfy <- dataBLUPSAll %>%
        select(Environment, Genotype) %>%
        left_join(IndexSatisfyHigh) %>%
        left_join(IndexSatisfyRange) %>%
        select (Environment, Genotype, all_of(traits))
      traitsTest <- paste0(traits, ".Test")
    } else if (length(currenttraithigh) == 0 &
               length(currenttraitlow) == 0 & length(currenttraitrange) != 0) {
      traits <- currenttraitrange
      IndexSatisfy <- dataBLUPSAll %>%
        select(Environment, Genotype) %>%
        left_join(IndexSatisfyRange) %>%
        select (Environment, Genotype, all_of(traits))
      traitsTest <- paste0(traits, ".Test")
    } else if (length(currenttraithigh) == 0 &
               length(currenttraitlow) != 0 & length(currenttraitrange) == 0) {
      traits <- currenttraitlow
      IndexSatisfy <- dataBLUPSAll %>%
        select(Environment, Genotype) %>%
        left_join(IndexSatisfyInf) %>%
        select (Environment, Genotype, all_of(traits))
      traitsTest <- paste0(currenttraitlow, ".Test")
    } else if (length(currenttraithigh) == 0 &
               length(currenttraitlow) != 0 & length(currenttraitrange) != 0) {
      traits <- sort(c(currenttraitrange, currenttraitlow))
      IndexSatisfy <- dataBLUPSAll %>%
        select(Environment, Genotype) %>%
        left_join(IndexSatisfyRange) %>%
        left_join(IndexSatisfyInf) %>%
        select (Environment, Genotype, all_of(traits))
      traitsTest <- paste0(traits, ".Test")
    } else {
      traits <-
        sort(c(currenttraitrange, currenttraithigh, currenttraitlow))
      IndexSatisfy <- dataBLUPSAll  %>%
        select(Environment, Genotype) %>%
        left_join(IndexSatisfyHigh) %>%
        left_join(IndexSatisfyRange) %>%
        left_join(IndexSatisfyInf) %>%
        select (Environment, Genotype, all_of(traits))
      traitsTest <- paste0(traits, ".Test")
    }
    
    IndexSatisfycurrent <- IndexSatisfy %>%
      as_tibble() %>%
      filter(Environment == myEnvironment) %>%
      arrange(Genotype) %>%
      select(-Environment)
    colnames(IndexSatisfycurrent)[-1] <- traitsTest
    nt <- length(traits)
    
    dataBLUPSIndex <- dataBLUPS %>%
      select(Environment, Genotype, all_of(traits)) %>%
      left_join(IndexSatisfycurrent)
    
    if (myEnvironment == "OVERALL" &
        length(unique(dataBLUPSAll$Environment)) > 1)  {
      TableFinalChecks <- dataBLUPSIndex %>%
        arrange(Genotype) %>%
        mutate_at(c(all_of(traitsTest), all_of(traits)), round, digits =
                    2)
      
      hide1 <- nt + 3
      hide2 <- 2 * nt + 2
      
      traitsRed <- colnames(TableFinalChecks) %in% traits
      
      TableFinalChecksK <- as_huxtable(TableFinalChecks) %>%
        set_text_color(everywhere, traitsRed, 'red') %>%
        slice(2:n())
      
      for (i in 1:length(traitsTest)) {
        j <- 2 + i
        k <- nt + j
        testColor <- as.logical(t(TableFinalChecksK[, k]) == 1)
        
        TableFinalChecksK <-
          TableFinalChecksK %>% insert_column(testColor, after = colnames(TableFinalChecksK)[ncol(TableFinalChecksK)])
        colnames(TableFinalChecksK)[ncol(TableFinalChecksK)] <-
          "testColor"
        
        TableFinalChecksK %<>%
          set_text_color(row = testColor, col = j, "green4")
        TableFinalChecksK <-
          TableFinalChecksK[, -ncol(TableFinalChecksK)]
      }
      
      TableFinalChecksK <- TableFinalChecksK %>%
        select(-all_of(traitsTest))
      TableFinalChecksK <- add_colnames(TableFinalChecksK) %>%
        set_number_format(everywhere, colnames(TableFinalChecksK), fmt_pretty())
      bold(TableFinalChecksK)[1, ] <- TRUE
      bottom_border(TableFinalChecksK)[1, ] <- 1
      bottom_border(TableFinalChecksK)[nrow(TableFinalChecksK), ] <-
        1
    } else {
      TableFinalChecks <- dataBLUPSIndex
      hide1 <- nt + 3
      hide2 <- ncol(TableFinalChecks)
      
      traitsRed <- colnames(TableFinalChecks) %in% traits
      
      TableFinalChecksK <- as_huxtable(TableFinalChecks) %>%
        set_text_color(everywhere, traitsRed, 'red') %>%
        slice(2:n())
      
      for (i in 1:length(traitsTest)) {
        j <- 2 + i
        k <- nt + j
        testColor <- as.logical(t(TableFinalChecksK[, k]) == 1)
        
        TableFinalChecksK <-
          TableFinalChecksK %>% insert_column(testColor, after = colnames(TableFinalChecksK)[ncol(TableFinalChecksK)])
        colnames(TableFinalChecksK)[ncol(TableFinalChecksK)] <-
          "testColor"
        
        TableFinalChecksK %<>%
          set_text_color(row = testColor, col = j, "green4")
        TableFinalChecksK <-
          TableFinalChecksK[, -ncol(TableFinalChecksK)]
      }
      
      TableFinalChecksK <- TableFinalChecksK %>%
        select(-all_of(traitsTest))
      TableFinalChecksK <- add_colnames(TableFinalChecksK) %>%
        set_number_format(everywhere, colnames(TableFinalChecksK), fmt_pretty())
      bold(TableFinalChecksK)[1, ] <- TRUE
      bottom_border(TableFinalChecksK)[1, ] <- 1
      bottom_border(TableFinalChecksK)[nrow(TableFinalChecksK), ] <-
        1
    }
    
    if (myEnvironment == "OVERALL")  {
      output$TableSatisfyChecksEnvSupInfo <- renderText({
        if (hideAll$clearAll)
          return()
      })
    }
    
    if (length(Traits) != 0) {
      output$TableSatisfyChecks <- renderText({
        if (hideAll$clearAll)
          return()
        else
          for (i in 1:length(traits)) {
            j <- i + 2
            TableFinalChecks[[j]] <- case_when(
              TableFinalChecks[[j + length(traits)]] == 1 ~ cell_spec(round(TableFinalChecks[[j]], 2), color = "green"),
              TableFinalChecks[[j + length(traits)]] == 0 ~ cell_spec(round(TableFinalChecks[[j]], 2), color = "red")
            )
          }
        
        TableFinalChecks[, -(hide1:hide2)] %>%
          kable("html", escape = F, align = "c") %>%
          kable_styling(
            bootstrap_options = c("striped", "hover", "condensed", "responsive"),
            full_width = F,
            position = "left",
            fixed_thead = T
          )
      })
      
      output$dwlChecksAllui <- renderUI({
        if (hideAll$clearAll)
          return()
        else
          downloadButton("dwlChecksAll", "Download")
      })
      
      output$dwlChecksAll <- downloadHandler(
        filename = function() {
          paste0("Checks-", gsub(" ", "_", gsub(":", ".", Sys.time())), ".xlsx")
        },
        content = function(file) {
          if (hideAll$clearAll)
            return()
          else
            quick_xlsx(TableFinalChecksK, file = file, open = FALSE)
        }
      )
      
    }
    hideAll$clearAll <- FALSE
  })
  
  observeEvent(input$btnCompareSpecific, {
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputAnalysisTraits)
    req(input$InputEnvSpecific)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    req(
      !is.null(input$InputHighTraits) |
        !is.null(input$InputLowTraits) | !is.null(input$InputRangeTraits)
    )
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currentenv <- input$InputEnvSpecific
    currenttraits <- input$InputAnalysisTraits
    currenttraithigh <- input$InputHighTraits
    currenttraitrange <- input$InputRangeTraits
    currenttraitlow <- input$InputLowTraits
    myEnvironment <- reactive({
      currentenv
    })
    myEnvironment <- myEnvironment()
    dataBLUPSAll <- dataBLUPS()
    dataBLUPSAll %<>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype))
    dataBLUPS <- dataBLUPSAll %>%
      filter(Environment == myEnvironment) %>%
      select(Environment, Genotype, all_of(currenttraits)) %>%
      arrange(Genotype)
    
    currenttraits <-
      currenttraits[colSums(is.na(dataBLUPS[, currenttraits])) != nrow(dataBLUPS[, currenttraits])]
    currenttraithigh <-
      sort(intersect(currenttraithigh, currenttraits))
    currenttraitlow <-
      sort(intersect(currenttraitlow, currenttraits))
    currenttraitrange <-
      sort(intersect(currenttraitrange, currenttraits))
    TraitnoRange <- sort(c(currenttraithigh, currenttraitlow))
    TraitRange <- sort(currenttraitrange)
    Traits <- sort(c(TraitnoRange, TraitRange))
    
    summaryDat <- NULL
    for (i in 1:length(currenttraits)) {
      trait <- dataBLUPS %>%
        pull(currenttraits[i])
      summaryDat <-
        rbind(
          summaryDat,
          data.frame(
            Trait = currenttraits[i],
            Min = min(trait, na.rm =
                        TRUE),
            Mean = mean(trait, na.rm =
                          TRUE),
            Max = max(trait, na.rm =
                        TRUE),
            stringsAsFactors = FALSE
          )
        )
    }
    
    if (length(currenttraithigh) != 0) {
      values <- NULL
      for (i in 1:length(TraitnoRange)) {
        values[i] <- paste0('input$', 'TraitSlider', i)
        values[i] <- eval(parse(text = values[i]))
      }
      values <- t(as.numeric(values)) %>%
        as.data.frame() %>%
        as_tibble(.name_repair = "unique") %>%
        rename_all(list( ~ str_replace(., ., TraitnoRange)))
    }
    
    if (length(currenttraitlow) != 0) {
      values <- NULL
      for (i in 1:length(TraitnoRange)) {
        values[i] <- paste0('input$', 'TraitSlider', i)
        values[i] <- eval(parse(text = values[i]))
      }
      values <- t(as.numeric(values)) %>%
        as.data.frame() %>%
        as_tibble(.name_repair = "unique") %>%
        rename_all(list( ~ str_replace(., ., TraitnoRange)))
    }
    
    if (length(currenttraitrange) != 0) {
      range <- matrix(0, length(currenttraitrange), 2)
      for (i in 1:length(currenttraitrange)) {
        rangeS <- paste0('input$', 'TraitSliderR', i)
        range[i,] <- sapply(rangeS, function(x)
          eval(parse(text = x)))
      }
    }
    
    if (!is.null(input$InputHighTraits)) {
      output$TableSpecificHighInfo <- renderText({
        req(input$InputHighTraits)
        if (hideAll$clearAll)
          return()
        else
          "Genotypes above the specified value are highlighted in green or red, otherwise.
                  Default value is the genotypes mean and the choosen value is the current value choosen between the minimum and
                                                                         the maximum in each environment."
      })
      
      output$TableSpecifiedValuesHigh <- renderText({
        req(input$InputHighTraits)
        if (hideAll$clearAll)
          return()
        else
          summaryDatHigh <- summaryDat %>%
            filter(Trait %in% currenttraithigh)
        default <- as.numeric(summaryDatHigh$Mean)
        values <- as.numeric(values)
        values_index <- match(currenttraithigh, TraitnoRange)
        values <- values[values_index]
        SpecifiedValues <-
          data.frame(currenttraithigh, round(default, 2), round(values, 2))
        colnames(SpecifiedValues) <-
          c("Trait", "Default value", "Choosen value")
        SpecifiedValues %>%
          kable(format = "html", escape = F) %>%
          kable_styling(
            bootstrap_options = c("striped", "hover", "condensed", "responsive"),
            fixed_thead = TRUE
          )
      })
    }
    
    if (!is.null(input$InputLowTraits)) {
      output$TableSpecificLowInfo <- renderText({
        req(input$InputLowTraits)
        if (hideAll$clearAll)
          return()
        else
          "Genotypes below the specified value are highlighted in green or red, otherwise.
                  Default value is the genotypes mean and the choosen value is the current value choosen between the minimum and
                                                                         the maximum in each environment."
      })
      
      output$TableSpecifiedValuesLow <- renderText({
        req(input$InputLowTraits)
        if (hideAll$clearAll)
          return()
        else
          summaryDatLow <- summaryDat %>%
            filter(Trait %in% currenttraitlow)
        default <- as.numeric(summaryDatLow$Mean)
        
        values <- as.numeric(values)
        values_index <- match(currenttraitlow, TraitnoRange)
        values <- values[values_index]
        
        SpecifiedValues <-
          data.frame(currenttraitlow, round(default, 2), round(values, 2))
        colnames(SpecifiedValues) <-
          c("Trait", "Default value", "Choosen value")
        SpecifiedValues %>%
          kable(format = "html", escape = F) %>%
          kable_styling(
            bootstrap_options = c("striped", "hover", "condensed", "responsive"),
            fixed_thead = TRUE
          )
      })
    }
    
    
    if (!is.null(input$InputRangeTraits)) {
      output$TableSpecifiedValuesRInfo <- renderText({
        req(input$InputRangeTraits)
        if (hideAll$clearAll)
          return()
        else
          "Genotypes within range values are highlighted in green or red, otherwise."
      })
      
      output$TableSpecifiedValuesR <- renderText({
        req(input$InputRangeTraits)
        if (hideAll$clearAll)
          return()
        else
          rangeMin <- range[, 1]
        rangeMax <- range[, 2]
        SpecifiedValuesR <-
          data.frame(TraitRange, round(rangeMin, 2), round(rangeMax, 2))
        colnames(SpecifiedValuesR) <-
          c("Trait", "Lowest", "Highest")
        SpecifiedValuesR %>%
          kable(format = "html", escape = F) %>%
          kable_styling(
            bootstrap_options = c("striped", "hover", "condensed", "responsive"),
            fixed_thead = TRUE
          )
      })
    }
    
    
    if (length(currenttraithigh) != 0) {
      IndexSatisfyHigh <- dataBLUPSHigh <- dataBLUPSAll %>%
        select(Environment, Genotype, all_of(currenttraithigh))
      valuesHigh <- values[, currenttraithigh]
      for (i in 1:length(currenttraithigh)) {
        j <- i + 2
        IndexSatisfyHigh[, j] <-
          as.vector(ifelse(dataBLUPSHigh[, j] > as.numeric(valuesHigh[1, i]), 1, 0))
      }
    }
    
    if (length(currenttraitlow) != 0) {
      IndexSatisfyInf <- dataBLUPSLow <- dataBLUPSAll %>%
        select(Environment, Genotype, all_of(currenttraitlow))
      valuesLow <- values[, currenttraitlow]
      for (i in 1:length(currenttraitlow)) {
        j <- i + 2
        IndexSatisfyInf[, j] <-
          as.vector(ifelse(dataBLUPSLow[, j] <= as.numeric(valuesLow[1, i]), 1, 0))
      }
    }
    
    if (length(currenttraitrange) != 0) {
      IndexSatisfyRange <- dataBLUPSRange <- dataBLUPSAll %>%
        select(Environment, Genotype, all_of(currenttraitrange))
      for (i in 1:length(currenttraitrange)) {
        j <- i + 2
        IndexSatisfyRange[, j] <-
          as.vector(ifelse((dataBLUPSRange[, j] <= range[i, 2] &
                              dataBLUPSRange[, j] >= range[i, 1]),
                           1,
                           0
          ))
      }
    }
    
    if (length(currenttraithigh) != 0 &
        length(currenttraitlow) == 0 & length(currenttraitrange) == 0) {
      traits <- currenttraithigh
      IndexSatisfy <- dataBLUPSAll %>%
        select(Environment, Genotype) %>%
        left_join(IndexSatisfyHigh) %>%
        select (Environment, Genotype, all_of(traits))
      traitsTest <- paste0(traits, ".Test")
    } else if (length(currenttraithigh) != 0 &
               length(currenttraitlow) != 0 & length(currenttraitrange) == 0) {
      traits <- sort(c(currenttraithigh, currenttraitlow))
      IndexSatisfy <- dataBLUPSAll %>%
        select(Environment, Genotype) %>%
        left_join(IndexSatisfyHigh) %>%
        left_join(IndexSatisfyInf) %>%
        select (Environment, Genotype, all_of(traits))
      traitsTest <- paste0(traits, ".Test")
    } else if (length(currenttraithigh) != 0 &
               length(currenttraitlow) == 0 & length(currenttraitrange) != 0) {
      traits <- sort(c(currenttraitrange, currenttraithigh))
      IndexSatisfy <- dataBLUPSAll %>%
        select(Environment, Genotype) %>%
        left_join(IndexSatisfyHigh) %>%
        left_join(IndexSatisfyRange) %>%
        select (Environment, Genotype, all_of(traits))
      traitsTest <- paste0(traits, ".Test")
    } else if (length(currenttraithigh) == 0 &
               length(currenttraitlow) == 0 & length(currenttraitrange) != 0) {
      traits <- currenttraitrange
      IndexSatisfy <- dataBLUPSAll %>%
        select(Environment, Genotype) %>%
        left_join(IndexSatisfyRange) %>%
        select (Environment, Genotype, all_of(traits))
      traitsTest <- paste0(traits, ".Test")
    } else if (length(currenttraithigh) == 0 &
               length(currenttraitlow) != 0 & length(currenttraitrange) == 0) {
      traits <- currenttraitlow
      IndexSatisfy <- dataBLUPSAll %>%
        select(Environment, Genotype) %>%
        left_join(IndexSatisfyInf) %>%
        select (Environment, Genotype, all_of(traits))
      traitsTest <- paste0(currenttraitlow, ".Test")
    } else if (length(currenttraithigh) == 0 &
               length(currenttraitlow) != 0 & length(currenttraitrange) != 0) {
      traits <- sort(c(currenttraitrange, currenttraitlow))
      IndexSatisfy <- dataBLUPSAll %>%
        select(Environment, Genotype) %>%
        left_join(IndexSatisfyRange) %>%
        left_join(IndexSatisfyInf) %>%
        select (Environment, Genotype, all_of(traits))
      traitsTest <- paste0(traits, ".Test")
    } else {
      traits <-
        sort(c(currenttraitrange, currenttraithigh, currenttraitlow))
      IndexSatisfy <- dataBLUPSAll %>%
        select(Environment, Genotype) %>%
        left_join(IndexSatisfyHigh) %>%
        left_join(IndexSatisfyRange) %>%
        left_join(IndexSatisfyInf) %>%
        select (Environment, Genotype, all_of(traits))
      traitsTest <- paste0(traits, ".Test")
    }
    
    IndexSatisfycurrent <- IndexSatisfy %>%
      as_tibble() %>%
      filter(Environment == myEnvironment) %>%
      arrange(Genotype) %>%
      select(-Environment)
    
    colnames(IndexSatisfycurrent)[-1] <- traitsTest
    nt <- length(traits)
    
    dataBLUPSIndex <- dataBLUPS %>%
      select(Environment, Genotype, all_of(traits)) %>%
      left_join(IndexSatisfycurrent)
    
    if (myEnvironment == "OVERALL" &
        length(unique(dataBLUPSAll$Environment)) > 1) {
      TableFinalSpecific <- dataBLUPSIndex %>%
        arrange(Genotype)
      
      hide1 <- nt + 3
      hide2 <- 2 * nt + 2
      
      traitsRed <- colnames(TableFinalSpecific) %in% traits
      
      TableFinalSpecificK <- as_huxtable(TableFinalSpecific) %>%
        set_text_color(everywhere, traitsRed, 'red') %>%
        slice(2:n())
      
      for (i in 1:length(traitsTest)) {
        j <- 2 + i
        k <- nt + j
        testColor <- as.logical(t(TableFinalSpecificK[, k]) == 1)
        
        TableFinalSpecificK <-
          TableFinalSpecificK %>% insert_column(testColor, after = colnames(TableFinalSpecificK)[ncol(TableFinalSpecificK)])
        colnames(TableFinalSpecificK)[ncol(TableFinalSpecificK)] <-
          "testColor"
        
        TableFinalSpecificK %<>%
          set_text_color(row = testColor, col = j, "green4")
        TableFinalSpecificK <-
          TableFinalSpecificK[, -ncol(TableFinalSpecificK)]
      }
      
      TableFinalSpecificK <- TableFinalSpecificK %>%
        select(-all_of(traitsTest))
      TableFinalSpecificK <- add_colnames(TableFinalSpecificK) %>%
        set_number_format(everywhere,
                          colnames(TableFinalSpecificK),
                          fmt_pretty())
      bold(TableFinalSpecificK)[1, ] <- TRUE
      bottom_border(TableFinalSpecificK)[1, ] <- 1
      bottom_border(TableFinalSpecificK)[nrow(TableFinalSpecificK), ] <-
        1
      
      
    } else {
      TableFinalSpecific <- dataBLUPSIndex
      hide1 <- nt + 3
      hide2 <- ncol(TableFinalSpecific)
      
      traitsRed <- colnames(TableFinalSpecific) %in% traits
      
      TableFinalSpecificK <- as_huxtable(TableFinalSpecific) %>%
        set_text_color(everywhere, traitsRed, 'red') %>%
        slice(2:n())
      
      for (i in 1:length(traitsTest)) {
        j <- 2 + i
        k <- nt + j
        testColor <- as.logical(t(TableFinalSpecificK[, k]) == 1)
        
        TableFinalSpecificK <-
          TableFinalSpecificK %>% insert_column(testColor, after = colnames(TableFinalSpecificK)[ncol(TableFinalSpecificK)])
        colnames(TableFinalSpecificK)[ncol(TableFinalSpecificK)] <-
          "testColor"
        
        TableFinalSpecificK %<>%
          set_text_color(row = testColor, col = j, "green4")
        TableFinalSpecificK <-
          TableFinalSpecificK[, -ncol(TableFinalSpecificK)]
      }
      
      TableFinalSpecificK <- TableFinalSpecificK %>%
        select(-all_of(traitsTest))
      TableFinalSpecificK <- add_colnames(TableFinalSpecificK) %>%
        set_number_format(everywhere,
                          colnames(TableFinalSpecificK),
                          fmt_pretty())
      bold(TableFinalSpecificK)[1, ] <- TRUE
      bottom_border(TableFinalSpecificK)[1, ] <- 1
      bottom_border(TableFinalSpecificK)[nrow(TableFinalSpecificK), ] <-
        1
      
      
    }
    
    if (myEnvironment == "OVERALL")  {
      output$TableSatisfySpecificEnvSupInfo <- renderText({
        if (hideAll$clearAll)
          return()
      })
    }
    
    if (length(Traits) != 0) {
      output$TableSatisfySpecific <- renderText({
        if (hideAll$clearAll)
          return()
        else
          for (i in 1:length(traits)) {
            j <- i + 2
            TableFinalSpecific[[j]] <- case_when(
              TableFinalSpecific[[j + length(traits)]] == 1 ~ cell_spec(round(TableFinalSpecific[[j]], 2), color = "green"),
              TableFinalSpecific[[j + length(traits)]] == 0 ~ cell_spec(round(TableFinalSpecific[[j]], 2), color = "red")
            )
          }
        TableFinalSpecific[, -(hide1:hide2)] %>%
          kable("html", escape = F, align = "c") %>%
          kable_styling(
            bootstrap_options = c("striped", "hover", "condensed", "responsive"),
            full_width = F,
            position = "left",
            fixed_thead = T
          )
        
      })
      
      output$dwlSpecificAllui <- renderUI({
        if (hideAll$clearAll)
          return()
        else
          downloadButton("dwlSpecificAll", "Download")
      })
      
      output$dwlSpecificAll <- downloadHandler(
        filename = function() {
          paste0("Specific-Values-", gsub(" ", "_", gsub(":", ".", Sys.time())), ".xlsx")
        },
        content = function(file) {
          quick_xlsx(TableFinalSpecificK,
                     file = file,
                     open = FALSE)
        }
      )
    }
    hideAll$clearAll <- FALSE
    
  })
  
  observeEvent(input$btnBaseIndex, {
    req(input$fileUp)
    req(input$selSheetBLUPS)
    req(input$InputAnalysisTraits)
    req(input$InputBaseTraits)
    req(input$InputEnvBaseIndex)
    req(input$InputGenotype)
    req(input$InputEnvironment)
    Genotype <- input$InputGenotype
    Environment <- input$InputEnvironment
    currentenv <- input$InputEnvBaseIndex
    currenttraits <- input$InputBaseTraits
    myEnvironment <- reactive({
      currentenv
    })
    myEnvironment <- myEnvironment()
    dataBLUPSAll <- dataBLUPS()
    dataBLUPSAll %<>%
      rename(Environment = all_of(Environment),
             Genotype = all_of(Genotype))
    dataBLUPS <- dataBLUPSAll %>%
      filter(Environment == myEnvironment) %>%
      select(Environment, Genotype, all_of(currenttraits)) %>%
      arrange(Genotype)
    
    currenttraits <-
      currenttraits[colSums(is.na(dataBLUPS[, currenttraits])) != nrow(dataBLUPS[, currenttraits])]
    currenttraitsBase <- paste0(currenttraits, ".Base")
    
    if (length(currenttraits) != 0) {
      values <- NULL
      for (i in 1:length(currenttraits)) {
        values[i] <- paste0('input$', 'TraitSliderBaseIndex', i)
        values[i] <- eval(parse(text = values[i]))
      }
      
      values <- t(as.numeric(values)) %>%
        as.data.frame() %>%
        as_tibble(.name_repair = "unique") %>%
        rename_all(list( ~ str_replace(., ., currenttraits)))
    }
    
    
    if (length(currenttraits) != 0) {
      output$TableBaseInfo <- renderText({
        if (hideAll$clearAll)
          return()
        else
          "Traits are standardized and weighted, their sum is the Base Index (BI)."
      })
      
      values <- as.numeric(values)
      IndexSatisfyBase  <- dataBLUPS %>%
        select(Environment, Genotype, all_of(currenttraits)) %>%
        as.data.frame() %>%
        as_tibble()
      
      IndexSatisfyBaseTraits <- IndexSatisfyBase %>%
        select(-c(Environment, Genotype))  %>%
        scale(.,
              center = FALSE,
              scale = apply(., 2, sd, na.rm = TRUE)) %>%
        as_tibble()
      
      
      if (ncol(IndexSatisfyBaseTraits) > 1) {
        IndexSatisfyBaseTraits <-
          IndexSatisfyBaseTraits[, colSums(is.na(IndexSatisfyBaseTraits)) != nrow(IndexSatisfyBaseTraits)] %>%
          as.data.frame()
      }
      currenttraits <-
        currenttraits[colSums(is.na(IndexSatisfyBaseTraits[, currenttraits])) !=
                        nrow(IndexSatisfyBaseTraits[, currenttraits])]
      colnames(IndexSatisfyBaseTraits) <- currenttraits
      
      currenttraitsBase <- paste0(currenttraits, ".weighted")
      
      IndexSatisfyBaseTraits <-
        t(t(IndexSatisfyBaseTraits) * values) %>%
        as.data.frame()
      
      IndexSatisfyBaseEG <- IndexSatisfyBase %>%
        select(Environment, Genotype)
      
      IndexSatisfyBase.weighted <-
        bind_cols(IndexSatisfyBaseEG, IndexSatisfyBaseTraits)
      
      TableFinalBaseIndex <- dataBLUPS %>%
        select(Genotype) %>%
        left_join(IndexSatisfyBase.weighted, by = "Genotype") %>%
        select(Environment, Genotype, everything())
      
      colnames(TableFinalBaseIndex)[-c(1, 2)] <- currenttraitsBase
      
      TableFinalBaseIndex  %<>%
        mutate(Base.Index = rowSums(.[, -c(1, 2)])) %>%
        arrange(desc(Base.Index)) %>%
        mutate(Rank = 1:nrow(.)) %>%
        left_join(IndexSatisfyBase %>%
                    select(-Environment)) %>%
        select(-all_of(currenttraitsBase)) %>%
        select(Environment,
               Genotype,
               all_of(currenttraits),
               everything())
      
      
      output$TableBaseIndex <- renderText({
        if (hideAll$clearAll)
          return()
        else
          TableFinalBaseIndex %>%
          kable("html", escape = F, align = "c") %>%
          kable_styling(
            bootstrap_options = c("striped", "hover", "condensed", "responsive"),
            full_width = F,
            position = "left",
            fixed_thead = T
          )
        
      })
      
      output$dwlBaseIndexAllui <- renderUI({
        if (hideAll$clearAll)
          return()
        else
          downloadButton("dwlBaseIndexAll", "Download")
      })
      
      output$dwlBaseIndexAll <- downloadHandler(
        filename = function() {
          paste0("Base-Index-", gsub(" ", "_", gsub(":", ".", Sys.time())), ".xlsx")
        },
        content = function(file) {
          quick_xlsx(TableFinalBaseIndex,
                     file = file,
                     open = FALSE)
        }
      )
    }
    
    hideAll$clearAll <- FALSE
    
  })
  
}
