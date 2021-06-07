
shinyUI(dashboardPage(title= "Model Testing System", 
                      #--------------------------------------------------------------------
                      ## HEADERBAR
                      #------------------------------------------------------------------
                      #You might need to get a different version of the logo for the tool which looks nicer
                      dashboardHeader(title = span(img(src = "images/logo.png")),  
                                      tags$li(class = "dropdown", 
                                              popify(
                                                tags$a(img(height = "18px", 
                                                           src = "images/refresh.png"),href = "javascript:history.go(0)"),
                                                title = "Refresh",  
                                                placement = "left"
                                              )
                                      )     
                      ), 
                      #--------------------------------------------------------------------
                      ## SIDE NAVIGATION BAR
                      #--------------------------------------------------------------------
                      dashboardSidebar(
                        collapsed = TRUE,
                        sidebarMenu(
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(), 
                          menuItem("ML Model Testing", tabName = "MLComp", icon = icon("search",lib = "font-awesome")),    
                          br(),
                          br(),
                          br() 
                        )), 
                      #--------------------------------------------------------------------
                      ## MAINBODY
                      #--------------------------------------------------------------------
                      dashboardBody(  
                        setShadow(class = "box"),
                        tags$head(
                          tags$link(rel = "icon", type = "image/png", href = "OISweb.png"), 
                          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                          tags$style(HTML(".main-sidebar { font-size: 13px; }")),
                          tags$style(HTML(".main-sidebar {  font-weight:bold; }")),
                          tags$style(HTML(".skin-blue .main-sidebar {background-color: #0d1919;} ")) 
                        ),
                        tabItems( 
                          #--------------------------------------------------------------------
                          ### SCRIPT UI 
                          #--------------------------------------------------------------------
                          tabItem(tabName = "MLComp",  
                                  verticalLayout( 
                                    tabsetPanel(   
                                      tabPanel( "Machine Learning Model Comparison on Financial Data",value=1, 
                                                ML_Comp_UI("ML_Comp-module")
                                      ), 
                                      #usefull if you have want multiple tabs ran on different scripts 
                                      id = "conditionedPanels"
                                    )
                                  )) 
                        ))  
)) 