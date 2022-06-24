#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#install.packages("rJava")
#install.packages("xlsx")

library(rJava)
library(xlsx)
library(uuid)
library(htmlwidgets)
library(plotly)

# source("Code/0A_Functions.R", local = TRUE) # when running locally
source("Code/0A_Functions.R") # for online server

Parameters <- c(
  "mu.exp.prior", "u.mu.exp.prior", 
  "sigma.exp.prior", "u.sigma.exp.prior", "mu.tox.prior", "u.mu.tox.prior", "sigma.tox.prior",
  "u.sigma.tox.prior", "VSL", "endpoint", "costPerAcuteCase",
  "N", "r", "t0", "tHorizon", "max.CC.pop",
  "h", "expr.A.UR", "t.A","expr.B.UR",
  "t.B", "decisionRule", "TRL", "By", "ICC.A",
  "ICC.B", "ucl", "lcl", "RS_included")

serverUserFilePath <- "www/"#"user-files/"

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  report_cache <- reactiveValues(filepath = NULL) #This creates a short-term storage location for a filepath

   loadedData <- reactiveValues(
      defaultData = NULL,
      defaultCount = 0,
      defaultIDs = NULL,
      defaultNames = NULL,
      myData =NULL,
      UserFileName = ""
      )
    
    variableValue <- reactiveValues(
      bolUserFileLoaded = FALSE,
      hideUpdate = TRUE,
      fileUUID = ""
      )
    
    getScenarioName <- function(filePath, id)
     {
       theData <- read.xlsx(file = filePath, sheetIndex=1, header=TRUE, check.names = FALSE, stringsAsFactors=FALSE)
        
       lstName <- colnames(theData)
       lstID <- c(1:length(lstName))
       dfData <- data.frame(lstID, lstName, stringsAsFactors=FALSE)
       currentName <- dfData$lstName[dfData$lstID == id]
    }
    
    setDefaultValues <- function()
    { 
      tempData <-  read.xlsx(file ="DefaultScenarios.xlsx", sheetIndex=1, header=TRUE, check.names = FALSE, stringsAsFactors=FALSE)  
      loadedData$defaultData <- tempData
      loadedData$myData <- tempData
      headerNames <- colnames(isolate(loadedData$defaultData))
      
      loadedData$defaultCount <- length(headerNames)-1
      loadedData$defaultIDs <- 1:isolate(loadedData$defaultCount)
      loadedData$defaultNames <- headerNames[-1]
    }
    
    #refresh scenarios dropdown list, and, all inputs are reloaded in observe 
    bindDdlSelectDefault <- function()
    {
      names <- isolate(loadedData$defaultNames)
      keys <- 1:isolate(loadedData$defaultCount)
      scenarios <- data.frame(keys, names, stringsAsFactors=FALSE)
      choiceScenarios <- setNames(as.numeric(scenarios$keys), scenarios$names)                                                 
      updateSelectInput(session, "ddlSelect", choices=choiceScenarios, selected = 1)
    }
    
    rebindDdlSelect <- function(selectedNumber =-1)
    {
     
      userCols <- colnames(isolate(loadedData$myData))
      userScenario <- userCols[-1]
      
      #default data
      defaultScenario <- isolate(loadedData$defaultNames)
      
      #merged
      mergedScenarios <- c(defaultScenario, userScenario)
      colsCount <- length(mergedScenarios)
      
      #set up scenarios dropdown list
      if (selectedNumber == -1)
        selectedValue = colsCount
      else
        selectedValue = selectedNumber
      
      keys <- 1:colsCount
      scenarios <- data.frame(keys, mergedScenarios, stringsAsFactors=FALSE)
      choiceScenarios <- setNames(as.numeric(scenarios$keys), scenarios$mergedScenarios)                                                 
      updateSelectInput(session, "ddlSelect", choices=choiceScenarios, selected = selectedValue)
    }
    
    setControlValues <- function(dataCol)
    {
      #Tab 1: Decision Rules and Prior Uncertainty
      updateRadioButtons(session, "decisionRule", selected = as.numeric(dataCol["decisionRule"]))
      updateRadioButtons(session, "RS_included", selected = as.numeric(dataCol["RS_included"]))
      updateNumericInput(session, "TRL", value = as.numeric(dataCol["TRL"]))
      updateNumericInput(session, "ucl",value = as.numeric(dataCol["ucl"]) * 100)
      updateNumericInput(session, "lcl", value = as.numeric(dataCol["lcl"]) * 100)
      
      
      #Tab 1: Prior Parameters
      updateNumericInput(session, "muMeanExposure", value = as.numeric(dataCol["mu.exp.prior"]))
      updateNumericInput(session, "sigmaMeanExposure", value = as.numeric(dataCol["u.mu.exp.prior"]))
      updateNumericInput(session, "muSDExposure", value = as.numeric(dataCol["sigma.exp.prior"]))
      updateNumericInput(session, "sigmaSDExposure", value = as.numeric(dataCol["u.sigma.exp.prior"]))
      updateNumericInput(session, "muMeanThreshold", value = as.numeric(dataCol["mu.tox.prior"]))
      updateNumericInput(session, "sigmaMeanThreshold", value = as.numeric(dataCol["u.mu.tox.prior"]))
      updateNumericInput(session, "muSDThreshold", value = as.numeric(dataCol["sigma.tox.prior"]))
      updateNumericInput(session, "sigmaSDThreshold", value = as.numeric(dataCol["u.sigma.tox.prior"]))
      
      
      #Tab 2: Economic Parameters
      updateRadioButtons(session, "endpoint", selected =  as.numeric(dataCol["endpoint"]))
      updateNumericInput(session, "costPerAcuteCase", value = as.numeric(dataCol["costPerAcuteCase"]))
      updateNumericInput(session, "vsl", value = as.numeric(dataCol["VSL"]) / 1e6)
      updateNumericInput(session, "By", value = as.numeric(dataCol["By"]))
      updateNumericInput(session, "population", value = as.numeric(dataCol["N"]))
      updateNumericInput(session, "sdr", value = as.numeric(dataCol["r"]) * 100)
      updateNumericInput(session, "t0", value = as.numeric(dataCol["t0"]))
      updateNumericInput(session, "tHorizon", value = as.numeric(dataCol["tHorizon"]))
      updateNumericInput(session, "annualReductionCost", value = as.numeric(dataCol["max.CC.pop"]))
      updateNumericInput(session, "controlCostExponent", value = as.numeric(dataCol["h"]))
      
      #Tab 3: Test Infromation
      #Tab 3, Test A
      updateNumericInput(session, "a_sizeThreshold", value = as.numeric(dataCol["expr.A.UR"]))
      updateNumericInput(session, "a_delay", value = as.numeric(dataCol["t.A"]))
      updateNumericInput(session, "a_costPerTest", value = as.numeric(dataCol["ICC.A"]))
      
      #Tab 3, Test B
      updateNumericInput(session, "b_sizeThreshold", value =  as.numeric(dataCol["expr.B.UR"]))
      updateNumericInput(session, "b_delay", value =  as.numeric(dataCol["t.B"]))
      updateNumericInput(session, "b_costPerTest", value = as.numeric(dataCol["ICC.B"]))
      
    }
    
    
    
  
    newColumn <- function(scenarioName)
    {
      newColumn <- c(scenarioName,
                     input$muMeanExposure,
                     input$sigmaMeanExposure,
                     input$muSDExposure,
                     input$sigmaSDExposure,
                     input$muMeanThreshold,
                     input$sigmaMeanThreshold,
                     input$muSDThreshold, # 10
                     input$sigmaSDThreshold,
                     input$vsl * 1e6,
                     input$endpoint,
                     input$costPerAcuteCase,
                     input$population,
                     input$sdr / 100,
                     input$t0,
                     input$tHorizon,
                     input$annualReductionCost, # 20
                     input$controlCostExponent,
                     input$a_sizeThreshold,
                     input$a_delay,
                     input$b_sizeThreshold,
                     input$b_delay,
                     input$decisionRule,
                     input$TRL,
                     input$By,
                     input$a_costPerTest,
                     input$b_costPerTest,
                     input$ucl / 100,
                     input$lcl / 100,
                     input$RS_included
      )
    }
    
    if (file.exists("DefaultScenarios.xlsx"))
       {
         shinyjs::disable("downloadData")
         # shinyjs::disable("download_report")
      
         setDefaultValues()
         
          #configIn <- NULL
          if (isolate(loadedData$defaultCount) > 0)
          {
            #set up scenarios dropdown list
            bindDdlSelectDefault()
            
            observe({
                    #clear error message
                    output$errorMessage <- renderText({
                    message = ""
                             })
         
                  if (input$ddlSelect == "")
                     return()
              
                  colID <- as.numeric(input$ddlSelect)
                
                  variableName <- NULL
                  dataCol <- NULL
              
                  #get current data column and load to UI
                  if (colID %in% isolate(loadedData$defaultIDs))  
                  {
                    variableValue$hideUpdate <- TRUE
                    messageText <- "( Default scenario cannot be changed )"
                    scenarioID <- colID + 1 #default scenario file
                  
                    variableName <- loadedData$defaultData[[1]]
                    currentDataCol <- loadedData$defaultData[[scenarioID]]
                    #configIn <- loadedData$defaultData[[scenarioID]]
                  }
                  else
                  {
                    variableValue$hideUpdate <- FALSE
                    messageText <- ""
                    scenarioID <- colID - 11 #user file
                 
                    variableName <- loadedData$myData[[1]]
                    currentDataCol <- loadedData$myData[[scenarioID]]
                    #configIn <- loadedData$myData[[scenarioID]]
                  }
              
                  names(currentDataCol) = variableName
                  
                  updateButton(session, "Update", disabled = variableValue$hideUpdate)
                  output$errorMessage2 <- renderText(paste("<span style=\"font-size:small\">", messageText, "</span"),sep = "")
                  
                  #load updateTextInput scenario name 
                  if (colID > isolate(loadedData$defaultCount))
                  {
                    userFilePath <- paste(serverUserFilePath, as.character(variableValue$fileUUID), ".xlsx",sep="")
                    newColName <- getScenarioName(userFilePath, scenarioID)
                    updateTextInput(session, "ScenarioNameUpdate", value=newColName)
                  }
                  else
                  {
                    updateTextInput(session, "ScenarioNameUpdate", value="")
                  }
                  
                  updateTextInput(session, "ScenarioName", value="")
                  
                  #load data to UI controls
                  setControlValues(currentDataCol)
                })
          }
    }
   
    
    report <- reactiveValues(file = NULL)

    output$priorRisk <- renderPlot({
        if (input$muMeanExposure != -1)
          {
          
          mu.tox=input$muMeanThreshold
          u.mu.tox=u.sd(sqrt(input$sigmaMeanThreshold^2+input$sigmaMeanExposure^2), range=0.9995)
          sigma.tox=input$muSDThreshold
          u.sigma.tox=input$sigmaSDThreshold
          mu.exp=input$muMeanExposure
          u.mu.exp=input$sigmaMeanExposure
          sigma.exp=input$muSDExposure
          u.sigma.exp=input$sigmaSDExposure
          t.IC=0
          t.imp=2
          t.eff=0
          u.range.spacing=0.05
          for.range.mu.tox.raw=seq(-6, 6, by=u.range.spacing)
          pdf.orig=dnorm(for.range.mu.tox.raw) # Determine the weights for expectation calculation
          pdf=pdf.orig/sum(pdf.orig) # Normalize the empirical pdf
          
          TRL=input$TRL
          ER.option.A=0.9
          q.UCL=input$ucl/100; q.LCL=input$lcl/100
          TRDM.prior.dat=TRDM_function_RS(mu.tox, u.mu.tox, sigma.tox, mu.exp, sigma.exp, t.IC, t.imp, t.eff, for.range.mu.tox.raw, TRL, q.UCL, q.LCL, ER.option.A=ER.option.A)
          
          plot.main="Prior uncertainty distribution for risk"
          plot.ylab="pdf"
          plot.xlab=bquote(log[10](R))
          col.TRL="purple"
          col.q.UCL="orange"
          col.q.LCL="red"
          plot(TRDM.prior.dat$log10.true.risks.option.A$log10.risk.true.original, main=plot.main, pdf.orig,  type="n", xlab=plot.xlab, ylab=plot.ylab)
          grid()
          lines(TRDM.prior.dat$log10.true.risks.option.A$log10.risk.true.original, pdf.orig)
          abline(v=TRL, col=col.TRL, lwd=2, lty=2)
          lines(rep(TRDM.prior.dat$log10.risk.q.UCL, 2), c(0, dnorm(qnorm(q.UCL))), col=col.q.UCL, lwd=2)
          lines(rep(TRDM.prior.dat$log10.risk.q.LCL, 2), c(0, dnorm(qnorm(q.LCL))), col=col.q.LCL, lwd=2)
          plot.legend=c(as.expression(bquote(TRL)), bquote(q[UCL]), bquote(q[LCL]))
          legend.col=c(col.TRL, col.q.UCL, col.q.LCL)
          legend.lwd=c(2, 2, 2)
          legend.lty=c(2, 1, 1)
          legend("topleft", legend=plot.legend, col=legend.col, lwd=legend.lwd, lty=legend.lty, bg="white")
          }
      })

    output$meanThresholdDistribution <- renderPlot({
      if (!is.na(input$muMeanThreshold)){
        xlimit=c(min(input$muMeanExposure, input$muMeanThreshold)-5, max(input$muMeanExposure, input$muMeanThreshold)+5)
        param1 = seq(xlimit[1], xlimit[2], 0.01)
        param2 = suppressWarnings(dnorm(seq(xlimit[1], xlimit[2], 0.01), input$muMeanThreshold, u.sd(input$sigmaMeanThreshold, range=0.9995)))
        #Do not attempt to plot unless param2 has values
        if(any(is.na(param2)) | any(is.nan(param2))){
          return()
        }
        plot(param1, 
             param2, 
             xlim=xlimit, main = bquote(Prior~Uncertainty~Distribution~About~log[10](ED[50])), xlab = bquote(log[10](ED[50])), ylab = "Density", type = "n")
        grid()
        lines(seq(xlimit[1], xlimit[2], 0.01), dnorm(seq(xlimit[1], xlimit[2], 0.01), input$muMeanThreshold, u.sd(input$sigmaMeanThreshold, range=0.9995)))
      }
    })
    
    
    output$meanExposureDistribution <- renderPlot({
      if (!is.na(input$muMeanExposure)){
        xlimit=c(min(input$muMeanExposure, input$muMeanThreshold)-5, max(input$muMeanExposure, input$muMeanThreshold)+5)
        param2 = suppressWarnings(dnorm(seq(xlimit[1], xlimit[2], 0.01), input$muMeanExposure, u.sd(input$sigmaMeanExposure, range=0.9995)))
        if(any(is.na(param2)) | any(is.nan(param2))){
          return()
        }
        
        plot(seq(xlimit[1], xlimit[2], 0.01), 
             param2, 
             xlim=xlimit, main = bquote(Prior~Uncertainty~Distribution~About~log[10](Mean~Exposure)), xlab = bquote(log[10](Mean~Exposure)), ylab = "Density", type = "n", col=2)
        grid()
        lines(seq(xlimit[1], xlimit[2], 0.01), dnorm(seq(xlimit[1], xlimit[2], 0.01), input$muMeanExposure, u.sd(input$sigmaMeanExposure, range=0.9995)))
      }
        
    })
    
    
    output$sdr <- renderPlot({
      if (input$sdr>=0){
        # For Discount rate calculation
        discount.rate=input$sdr
        y.start=input$t0
        y.end=input$tHorizon
        TH=y.end-y.start+1
        disc.rate.vec=rep(NA, TH+5)
        for(tt in 1:(TH+5)){
          disc.rate.vec[tt]=disc.rate=1/(1+input$sdr/100)^(tt-1)
        }
        plot.title=eval(paste("Present value (in ", y.start, ") of a future cost of 100 USD at ", discount.rate, "%", sep=""))
        if(any(is.na(disc.rate.vec*100)) | any(is.nan(disc.rate.vec*100))){
          return()
        }
        plot(seq(y.start, y.end+5), 
             disc.rate.vec*100, 
             main = plot.title, ylim=c(0,100), xlab = "Year", ylab = "Present value of a future cost of 100 USD", type = "n")
        grid()
        lines(seq(y.start, y.end), disc.rate.vec[1:TH]*100, lwd=2)
        lines(seq(y.end, y.end+5), disc.rate.vec[TH:(TH+5)]*100, lwd=2, lty="dotted")
        points(c(y.start, y.end), disc.rate.vec[c(1, TH)]*100, pch=16, col=2)
        text(x=c(y.start, y.end), y=disc.rate.vec[c(1,TH)]*100-1, labels=c("Year 1", "Final Year"), pos=1)
      } 
    })
    
    output$controlCostExponent <- renderPlot({
      if (input$controlCostExponent != -1){
        # For Control Cost Calculation
        max.CC=input$annualReductionCost # Maximum control cost for completely eliminating the risk (i.e., exposure is 0) - at POPULATION LEVEL, in $Billions
        Overhead.CC=0.0/input$population # Overhead cost of implemetation of regulatory action. - at INDIVIDUAL LEVEL
        h=input$controlCostExponent # steepness of the CC function w.r.t reduction in exposure
        g.e.k=function(e.k){if(h>0){10^(h*e.k)-1} else{e.k}} # sub-function that calculates the control cost
        CC.k=function(e.k){
          CC.k=Overhead.CC+(max.CC-Overhead.CC)*g.e.k(e.k)/g.e.k(1)
          CC.k[which(e.k==0)]=0
          return(CC.k)
        } # Function that claculates control cost for reduction in exposure by e.k (e.k must be in arithmatic scale)
        
        plot(seq(0, 1, 0.01)*100, CC.k(seq(0, 1, 0.01))/1e6, main = "Control Cost as a Function of % Reduction in Exposure", xlab = "% Reduction in Exposure", ylab = "Annual Control Cost ($M)", type = "n")
        grid()
        lines(seq(0, 1, 0.01)*100, CC.k(seq(0, 1, 0.01))/1e6, lwd=2)
      }
    })
    

    observeEvent(input$userFile, 
                 # input$userFile will be NULL initially. After the user selects and uploads a file, it will be a data frame with 'name',
                 # 'size', 'type', and 'datapath' columns. The 'datapath' column will contain the local filenames where the data can
                 # be found.
                 {
                  if (variableValue$bolUserFileLoaded == FALSE)
                     {
                       inFile <- input$userFile
                       
                       if (is.null(inFile))
                           return(NULL)
                       
                       ext <- tools::file_ext(input$userFile$name)
                       
                       if(ext == "xlsx")
                       {
                         loadedData$UserFileName <- sub(".xlsx","",inFile$name,ignore.case = TRUE)
                         tempFile <- read.xlsx(inFile$datapath, header = FALSE, sheetIndex=1, stringsAsFactors=FALSE)
                         
                         selectedID <- 0
                         if (variableValue$fileUUID != "") #it could be created when user created scenario using default scenario
                         {
                           #append
                           userFilePath <- paste(serverUserFilePath, as.character(variableValue$fileUUID), ".xlsx",sep="") 
                           myData2 <- read.xlsx(file = userFilePath, sheetIndex=1, header=FALSE, stringsAsFactors=FALSE)
                           selectedID <- isolate(loadedData$defaultCount) + (ncol(myData2)-1)+ 1
                           
                           tempFile[[1]] <- NULL #delete first colum 
                           mergedData <- data.frame(myData2, tempFile, stringsAsFactors=FALSE)
                           
                           savePath <- paste(serverUserFilePath, as.character(variableValue$fileUUID), ".xlsx",sep="")
                           write.xlsx(x = mergedData, file = savePath, col.names = FALSE, row.names = FALSE)
                           
                           #read updated data
                           loadedData$myData <- read.xlsx(file = userFilePath, header = TRUE, sheetIndex=1, check.names = FALSE, stringsAsFactors=FALSE)
                         }
                         else
                         {
                           #save user file to server
                           variableValue$fileUUID <- UUIDgenerate()
                           savePath <- paste(serverUserFilePath, as.character(variableValue$fileUUID), ".xlsx",sep="")
                           write.xlsx(x=tempFile, file=savePath, col.names = FALSE, row.names = FALSE)
                           
                           selectedID <- isolate(loadedData$defaultCount) + 1
                           
                           #read updated data
                           loadedData$myData <- read.xlsx(inFile$datapath, header = TRUE, sheetIndex=1, check.names = FALSE, stringsAsFactors=FALSE)
                         }
                         
                         # reload dropdown
                         rebindDdlSelect(selectedID)
                         
                         output$errorMessage0 <- renderText({
                           message = ""
                         })
                         
                         variableValue$bolUserFileLoaded <- TRUE
                         
                         shinyjs::disable("userFile")
                         shinyjs::enable("downloadData")
                         shinyjs::enable("download_report")
                       }
                       else
                       {
                         output$errorMessage0 <- renderText({
                           message = "<span style=\"color:red;font-size:small\">Invalid file; Please upload a .xlsx file.</span>"
                         })
                       }
                    }
                   else
                    {
                      output$errorMessage0 <- renderText({
                        message = "<span style=\"color:red;font-size:small\">This file has been loaded.</span>"
                      })
                    }
                })
    
    
    observeEvent(input$Create, {
      output$errorMessage <- renderText({
        if (input$ScenarioName !='') {
          message = ""
        }
        else
        {
          message = "<span style=\"color:red;font-size:small\">Scenario name is required.</span>"
        }
      })
      
      if (input$ScenarioName !=''){
        newCol <- newColumn(input$ScenarioName)
        # print(newCol)
        df <- data.frame(stringsAsFactors = FALSE, newCol)
        myData2 <- NULL
       
        if (variableValue$fileUUID == "")
           {
            variableValue$fileUUID <- UUIDgenerate()
            # myData2 <- data.frame(c("ID", c(1:48)), stringsAsFactors = FALSE)
            myData2 <- data.frame(c("variable", Parameters), stringsAsFactors = FALSE)
            # print(myData2)
           }
        else
           {
             userFilePath <- paste(serverUserFilePath, as.character(variableValue$fileUUID), ".xlsx",sep="") 
             myData2 <- read.xlsx(file = userFilePath, sheetIndex=1, header=FALSE, stringsAsFactors=FALSE)   
           }
       
        result <- data.frame(myData2,df,stringsAsFactors=FALSE)
        savePath <- paste(serverUserFilePath, as.character(variableValue$fileUUID), ".xlsx", sep = "")
        write.xlsx(x=result, file = savePath, col.names = FALSE, row.names = FALSE)
        shinyjs::enable("downloadData")
        shinyjs::enable("download_report")
        
        sendSweetAlert(
          session = session,
          title = "Saved as new scenario Successfully",
          type = "success"
        )
        
        #user data
        loadedData$myData <- read.xlsx(savePath, header = TRUE, sheetIndex=1, check.names = FALSE, stringsAsFactors=FALSE)
        rebindDdlSelect()
      }
    })
    
  
    observeEvent(input$Update, {
       selectedID = as.numeric(input$ddlSelect) 
       userFilePath <- paste(serverUserFilePath, as.character(variableValue$fileUUID), ".xlsx",sep="") 
       if (!(selectedID %in% isolate(loadedData$defaultIDs)))
       {
         #write data
         colID <- as.numeric(input$ddlSelect) - 2
         if (input$ScenarioNameUpdate !='')
         {
            newColName <- input$ScenarioNameUpdate
         }
         else
         {
           #use existing scenario name
           newColName <- getScenarioName(userFilePath, colID)
         }
        
        newValue <- newColumn(newColName)
         
        myData3 <- read.xlsx(file = userFilePath, sheetIndex=1, header=FALSE,stringsAsFactors=FALSE)
        
        tmp <<- myData3
        tmp2 <<- newValue
        tmp3 <<- colID
        #myData3[[colID]] <- newValue
        #Find column that has a variable name that names the variable name in newValue
        myData3[[which(myData3[1,] == newValue[1])]] <- newValue
        write.xlsx(x=myData3, file = userFilePath, col.names = FALSE, row.names = FALSE)
        #user data
        loadedData$myData <- read.xlsx(userFilePath, header = TRUE, sheetIndex=1, check.names = FALSE, stringsAsFactors=FALSE)
        rebindDdlSelect(selectedID)
        shinyjs::enable("downloadData")
        shinyjs::enable("download_report")
        
        sendSweetAlert(
            session = session,
            title = "Saved Successfully",
            type = "success"
        )
       }
    })

    observeEvent(input$endpoint, {
      if(as.numeric(input$endpoint)==1){
        shinyjs::enable("vsl")
        shinyjs::disable("costPerAcuteCase")
      }
      if(as.numeric(input$endpoint)==2){
        shinyjs::disable("vsl")
        shinyjs::enable("costPerAcuteCase")
      }
    })
    
    observeEvent(input$decisionRule, {
      if(input$decisionRule==1){
        shinyjs::disable("TRL")
        shinyjs::disable("ucl")
        shinyjs::disable("lcl")
      }
      if(input$decisionRule==2){
        shinyjs::enable("TRL")
        shinyjs::enable("ucl")
        shinyjs::enable("lcl")
      }
    })
    
    
    observeEvent(input$endpoint, {
      if(!is.na(input$endpoint)){
        shinyjs::disable("sigmaSDExposure")
        shinyjs::disable("sigmaSDThreshold")
      }
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        if (isolate(loadedData$UserFileName) != "")
          paste(isolate(loadedData$UserFileName),"_",Sys.Date(),".xlsx", sep = "")
        else
          paste("Data_",Sys.Date(),".xlsx", sep = "")
      },
      
      content = function(file) {
        if (variableValue$fileUUID != "")
        {
          filePath <- paste(serverUserFilePath, as.character(variableValue$fileUUID), ".xlsx", sep="")
          tempFile <- read.xlsx(filePath, header = FALSE, sheetIndex=1, stringsAsFactors=FALSE)
          write.xlsx(tempFile, file, col.names = FALSE,  row.names = FALSE)
        }
      }
    )
    
    output$generated_report <- renderUI({
      if(file.exists(paste0(tempdir(), "/",report_cache$report_file))){
        shinyjs::enable("download_report")
        #https://stackoverflow.com/questions/61605312/includehtml-conflicting-with-renderplotly
        out_report = tags$iframe(src = report_cache$report_file, #Need to copy result into the www folder
                                 width = "100%", height = "1000", 
                                 style = "margin-top: 70px;")
        return(out_report)
      } else {
        shinyjs::disable("download_report")
        #hideTab(inputId = "tabs", target="Generated Report")
        return(p("No generated report to display."))
      }
    })
    
    observeEvent(input$generateReport, {
      disable("generateReport")
      #Remove generated_report tab if a report is already displayed
      removeTab(inputId = "tabs", target = "generated_report")
      sendSweetAlert(
        session = session,
        title = "Generating Report",
        text = "Your report will now be generated. This may take some time.",
        type = "info"
      )
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message= "Building report...Please Wait",
                   value=0)
      shinyjs::disable("Create")
      updateButton(session, "Update", disabled = TRUE)
      shinyjs::disable("downloadData")
      
      
      renderFilePath <- ""
      renderColID <- 0
      
      colID <- as.numeric(input$ddlSelect)
      if (colID <= isolate(loadedData$defaultCount))
      {
        #default scenario file
        renderFilePath <-  "DefaultScenarios.xlsx"
        renderColID <- colID + 1 
      }
      else
      {
        #user file
        userFilePath <- paste(serverUserFilePath, as.character(variableValue$fileUUID), ".xlsx",sep="") 
        renderFilePath <- userFilePath
        renderColID <- colID - 2 
      }
      
      # # Modified
      params= list(
        muMeanExposure=input$muMeanExposure,
        sigmaMeanExposure=input$sigmaMeanExposure,
        muSDExposure=input$muSDExposure,
        sigmaSDExposure=input$sigmaSDExposure,
        muMeanThreshold=input$muMeanThreshold,
        sigmaMeanThreshold=input$sigmaMeanThreshold,
        muSDThreshold=input$muSDThreshold, # 10
        sigmaSDThreshold=input$sigmaSDThreshold,
        vsl=input$vsl * 1e6,
        endpoint=input$endpoint,
        costPerAcuteCase=input$costPerAcuteCase,
        population=input$population,
        sdr=input$sdr / 100,
        t0=input$t0,
        tHorizon=input$tHorizon,
        annualReductionCost=input$annualReductionCost, # 20
        controlCostExponent=input$controlCostExponent,
        a_sizeThreshold=input$a_sizeThreshold,
        a_delay=input$a_delay,
        b_sizeThreshold=input$b_sizeThreshold,
        b_delay=input$b_delay,
        decisionRule=input$decisionRule,
        RS_included=input$RS_included,
        TRL=input$TRL,
        By=input$By,
        a_costPerTest=input$a_costPerTest,
        b_costPerTest=input$b_costPerTest,
        ucl=input$ucl / 100,
        lcl=input$lcl / 100,
        progress = progress #Add progress bar
        #https://stackoverflow.com/questions/55469928/progress-bar-for-kniting-documents-via-shiny
      )
      
      # This will be overwritten by Rmd to include the Table for 3D plot
      chart.results=0
      report_cache$report_file = paste0("markdown_", UUIDgenerate(), ".html")
      if(as.numeric(input$decisionRule)==2){
        if(as.numeric(input$sigmaMeanExposure)==0){
          if(as.numeric(input$RS_included)!=2){
            #Create temp copy of .Rmd file to bypass write restrictions during render
            tempReport <- file.path(tempdir(), "epavoi_TRDM_SC1.Rmd")
            file.copy("Code/Rmd/epavoi_TRDM_SC1.Rmd", tempReport, overwrite = TRUE)
            
            rmarkdown::render(tempReport, 
                              output_file = paste0("./",report_cache$report_file),#test_markdown.html",
                              params = params)
          }
          
          if(as.numeric(input$RS_included)==2){
            #Create temp copy of .Rmd file to bypass write restrictions during render
            tempReport <- file.path(tempdir(), "epavoi_TRDM_SC1_NoRS.Rmd")
            file.copy("Code/Rmd/epavoi_TRDM_SC1_NoRS.Rmd", tempReport, overwrite = TRUE)
            
            rmarkdown::render(tempReport, 
                              output_file = paste0("./",report_cache$report_file),#test_markdown.html",
                              params = params)
          }
        }
        if(as.numeric(input$sigmaMeanExposure)>0){
          if(as.numeric(input$RS_included)!=2){
            #Create temp copy of .Rmd file to bypass write restrictions during render
            tempReport <- file.path(tempdir(), "epavoi_TRDM_SC3.Rmd")
            file.copy("Code/Rmd/epavoi_TRDM_SC3.Rmd", tempReport, overwrite = TRUE)
            
            rmarkdown::render(tempReport, 
                              output_file = paste0("./",report_cache$report_file),#test_markdown.html",
                              params = params)
          }
          if(as.numeric(input$RS_included)==2){
            #Create temp copy of .Rmd file to bypass write restrictions during render
            tempReport <- file.path(tempdir(), "epavoi_TRDM_SC3_NoRS.Rmd")
            file.copy("Code/Rmd/epavoi_TRDM_SC3_NoRS.Rmd", tempReport, overwrite = TRUE)
            
            rmarkdown::render(tempReport, 
                              output_file = paste0("./",report_cache$report_file),#test_markdown.html",
                              params = params)
          }
          
        }
      }
      if(as.numeric(input$decisionRule)==1){
        if(as.numeric(input$sigmaMeanExposure)==0){
          if(as.numeric(input$RS_included)==2){
            #Create temp copy of .Rmd file to bypass write restrictions during render
            tempReport <- file.path(tempdir(), "epavoi_BRDM_SC1_NoRS.Rmd")
            file.copy("Code/Rmd/epavoi_BRDM_SC1_NoRS.Rmd", tempReport, overwrite = TRUE)
            rmarkdown::render(tempReport,
                              output_file = paste0("./",report_cache$report_file),#test_markdown.html",
                              params = params)
          }
          if(as.numeric(input$RS_included)!=2){
            #Create temp copy of .Rmd file to bypass write restrictions during render
            tempReport <- file.path(tempdir(), "epavoi_BRDM_SC1.Rmd")
            file.copy("Code/Rmd/epavoi_BRDM_SC1.Rmd", tempReport, overwrite = TRUE)
            rmarkdown::render(tempReport,
                              output_file = paste0("./",report_cache$report_file),#test_markdown.html",
                              params = params)
          }
        }
        if(as.numeric(input$sigmaMeanExposure)>0){
          if(as.numeric(input$RS_included)!=2){
            #Create temp copy of .Rmd file to bypass write restrictions during render
            tempReport <- file.path(tempdir(), "epavoi_BRDM_SC3.Rmd")
            file.copy("Code/Rmd/epavoi_BRDM_SC3.Rmd", tempReport, overwrite = TRUE)
            rmarkdown::render(tempReport, 
                              output_file = paste0("./",report_cache$report_file),#test_markdown.html",
                              params = params)
          }
          if(as.numeric(input$RS_included)==2){
            #Create temp copy of .Rmd file to bypass write restrictions during render
            tempReport <- file.path(tempdir(), "epavoi_BRDM_SC3_NoRS.Rmd")
            file.copy("Code/Rmd/epavoi_BRDM_SC3_NoRS.Rmd", tempReport, overwrite = TRUE)
            rmarkdown::render(tempReport, 
                              output_file = paste0("./",report_cache$report_file),#test_markdown.html",
                              params = params)
          }
        }
      }
      
      
      #---------------------------------------------------#
      # Include a line of code here to update the 3D plot #
      #---------------------------------------------------#
      file.copy(paste0(tempdir(), "/", report_cache$report_file), paste0("www/", report_cache$report_file))
      shinyjs::show("renderNote")
      shinyjs::enable("Create")
      shinyjs::enable("downloadData")
      shinyjs::enable("download_report")
      shinyjs::enable("generateReport")
      updateButton(session, "Update", disabled = variableValue$hideUpdate)
      #showTab(inputId = "tabs", target="Generated Report")
      
      appendTab(inputId = "tabs",
                select=TRUE,
                tabPanel(title="Generated Report",
                         value="generated_report",
                         fluidPage(h3("Generated Report"),
                                   hr(),
                                   uiOutput("generated_report")))
                )
      
      # updateTabsetPanel(session=session, inputId="tabs", 
      #                   selected="generated_report")
      sendSweetAlert(
        session = session,
        title = "Report Complete",
        type = "success"
      )
    })

    #https://github.com/rstudio/shiny/issues/2152 - separating Rmd render and download
    output$download_report <- downloadHandler(
      filename <- function() {
        paste("output", "html", sep=".")
      },

      content <- function(file) {
        file.copy(paste0(tempdir(), "/", report_cache$report_file),
                  file)
      },
      contentType = "html"
    )
  
})
    

