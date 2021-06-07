ML_Comp_UI <- function(id) {
  ns <- NS(id)
  tagList(  
    sidebarLayout( 
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 250, left = "auto", right = 17, bottom = "auto",
                    width = 330, height = "auto",
                    h2(strong("Navigation Menu")),  
                    HTML('<button data-toggle="collapse" data-target="#demo5">Collapsible</button>'),
                    tags$div(id = 'demo5',  class="collapse",
                             helpText(strong("Sample the last n years of the data")),  
                             numericInput(ns("yearsnumber"), label = "Years",2, min = 1, max = 10),
                             helpText(strong("Select the security symbol")), 
                             textInput(ns("secsymbol"), label = "Security Symbol", 
                                       value = "FB",placeholder = "Please input the security"),
                             helpText(strong("Select ML model")), 
                             selectInput(ns("model_in"),"ML Model ",c("Decision Trees","Support Vector Machine"),
                                         selected = "Decision Trees"),
                             helpText(strong("Security preview range filter")),  
                             dateRangeInput(ns("daterange1"),label = "Date Range",
                                            start = Sys.Date()-360, 
                                            end   = Sys.Date()),
                             helpText(strong("Training data % input")),  
                             sliderInput(ns("percentage_train"),"% Range", min = 0.25, max = 0.9, step = 0.05, value = 0.75)
                    )
      ),
      mainPanel(
        column(width = 6, 
               column(width = 12,
                      gradientBox(
                        title = strong("Security Price Preview"),status="info",
                        solidHeader = T,closable = F,width = 12, icon = "fa fa-line-chart", 
                        withSpinner(plotlyOutput(ns("plot1dash"),height = "30vh"))
                      )), 
               column(width = 12,
                      gradientBox(
                        title = strong("Splits Plot"),status="info",
                        solidHeader = T,closable = F,width = 12, icon = "fa fa-line-chart",
                        withSpinner(plotOutput(ns('distimage_DT'),height =  "30vh" )) 
                      )),
               
               column(width = 12,
                      gradientBox(
                        title = strong("ROC curve"),status="info",
                        solidHeader = T,closable = F,width = 12, icon = "fa fa-bar-chart",
                        withSpinner(plotOutput(ns("plotash2"),height = "30vh"))
                      ))),
        column(width = 6,
               column(width = 12,
                      gradientBox(
                        title = strong("Training Dataset Preview"),status="info",
                        solidHeader = T,closable = F,width = 12, icon = "fa fa-bar-chart",
                        withSpinner(rHandsontableOutput(ns('table_tb'),height =  "30vh" )) 
                      )),
               
               column(width = 12,
                      gradientBox( 
                        title = strong("Model Insights"),status="info",
                        solidHeader = T,closable = F,width = 12, icon = "fa fa-bar-chart", 
                        withSpinner(verbatimTextOutput(ns("textash")))
                      )) )
        ,width = 12), 
      position = c("right") )
  )
}

ML_Comp <- function(input, output, session, pool) { 
  
  #reactive input controls
  secutiy <- reactive({ input$secsymbol }) 
  yearscount <- reactive({ input$yearsnumber }) 
  SelectedDateR<-reactive({ input$daterange1 }) 
  percentTraining<-reactive({ input$percentage_train }) 
  modelcontrol<-reactive({ input$model_in }) 
  #server functions
  output$textash<-renderPrint({   
    source("algorithms/DDT.R",local=TRUE)
    
    if(modelcontrol() == "Decision Trees"){
      
      print(paste("----------------------MODEL OUTPUT---------------------------"))
      printcp(DecisionTree)
      print(paste("---------------------- CONFUSION MATRIX OUTPUT---------------------------"))
      print(confmat)
      print(paste("---------------------- MODEL ACCURACY OUTPUT---------------------------"))
      print(xy)  
      print(yz) 
      print(paste("---------------------- MODEL AUC & RMSE OUTPUT---------------------------"))
      print (AUC)  
      print(RMSE)  
    }else if(modelcontrol() == "Support Vector Machine"){ 
      
      print(paste("----------------------MODEL OUTPUT---------------------------"))
      print(SVM)
      print(paste("---------------------- CONFUSION MATRIX OUTPUT---------------------------"))
      print(confmat)
      print(paste("---------------------- MODEL ACCURACY OUTPUT---------------------------"))
      print(xy)  
      print(yz) 
      print(paste("---------------------- MODEL AUC & RMSE OUTPUT---------------------------"))
      print (AUC)
      print(RMSE)
    }
    
  })
  
  
  output$plot1dash<-renderPlotly({   
    
    source("algorithms/DDT.R",local=TRUE)
    df <- data.frame(Date=index(STOCK),coredata(STOCK))
    df <- tail(df, 60)
    #make open and close numbers dynamic
    df[df$Date>=SelectedDateR()[1] & df$Date<=SelectedDateR()[2],]
    fig <- df %>% plot_ly(x = ~Date, type="candlestick",
                          open = ~df[,2], close = ~df[,5],
                          high = ~df[,3], low = ~df[,4]) 
    fig <- fig %>% layout(title = paste(SYM,"Basic Candlestick Chart",sep=" "),
                          xaxis = list(rangeslider = list(visible = F))) 
    fig
    
  })
  
  output$table_tb<-renderRHandsontable({ 
    
    source("algorithms/DDT.R",local=TRUE)
    #move date from index to column 1
    df <- cbind(newColName = rownames(DataSet), DataSet)
    rownames(df) <- 1:nrow(df)
    colnames(df) = c("Date","Class","RSI","EMAcross","MACD","SMI","WPR","ADX","CCI","CMO","ROC")
    
    rhandsontable(df[df$Date>=SelectedDateR()[1] & df$Date<=SelectedDateR()[2],]
                  ,
                  readOnly = TRUE,search = TRUE)%>%
      hot_cols(columnSorting = TRUE,highlightCol = TRUE, highlightRow = TRUE,
               manualColumnResize = T)%>%
      hot_cols(fixedColumnsLeft = 1) %>% 
      hot_cols(renderer = " 
      function (instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.TextRenderer.apply(this, arguments);  
        }")%>%  
      hot_context_menu(
        customOpts = list(
          csv = list(name = "Download to CSV",
                     callback = htmlwidgets::JS(
                       "function (key, options) {
             var csv = csvString(this, sep=',', dec='.');
             var link = document.createElement('a');
             link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
             encodeURIComponent(csv));
             link.setAttribute('download', 'data.csv');
             document.body.appendChild(link);
             link.click();
             document.body.removeChild(link);}")))) %>%
      hot_cell(1, 3, "")   
  })
  
  output$distimage_DT<-renderPlot({   
    
    source("algorithms/DDT.R",local=TRUE)
    if(modelcontrol() == "Decision Trees"){
      splits.plot = plotcp(DecisionTree,upper = "splits") #----------------- OUTPUT 4
      splits.plot
    }else if(modelcontrol() == "Support Vector Machine"){
      #Do nothing
      #classification model doesn't have splits
    }
    
  })
  
  output$plotash2<-renderPlot({   
    source("algorithms/DDT.R",local=TRUE)
    if(modelcontrol() == "Decision Trees"){
      plot(perf,col=1:10)
      abline(a=0,b=1, col='red') #----------------- output 6
    }else if(modelcontrol() == "Support Vector Machine"){
      plot(perf,col=1:10)
      abline(a=0,b=1, col='red') #----------------- output 6
    }
    
  })
} 