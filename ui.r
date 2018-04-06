library(shinycssloaders)

shinyUI(fluidPage(
  titlePanel("DataViz"),
  tags$b("Author:"),(" Dr. Fabio Veronesi (fveronesi[at]harper-adams.ac.uk)"),
  br(),
  tags$b("Info at:"), tags$a(href="https://github.com/fveronesi/Shiny_DataViz", "https://github.com/fveronesi/Shiny_DataViz"),
  
  absolutePanel(top="2%", right="2%",
                tags$a(href="https://paypal.me/DrFabioVeronesi/1", target="_blank", img(src='https://static1.squarespace.com/static/547a9276e4b0b93ff702ee6d/t/591b3a87bf629ae4dc55f884/1494956679650/paypal.png', width=125, height=70))
                ),
  
  
  sidebarLayout(
    sidebarPanel(
      
      #tags$b("Here you can select a file (csv or txt):"),
      
      selectInput("separator", "Data Separator:",
        c(Comma = ",",BlankSpace = "\t",Semicolon = ";")),
      
      fileInput(inputId = "Data", label = "File Selection (csv or txt):", multiple = F),
      
      selectInput("TypePlot", "Type of Plot", 
                  c("None",
                    BarChart = "bar",
                    InteractionPlot = "inter",
                    Histogram = "hist",
                    BoxPlot = "box",
                    Scatterplot ="points",
                    TimeSeries = "ts")),
      
      
      
      #HISTOGRAM
      conditionalPanel(
        condition = "input.TypePlot == 'hist'",
        uiOutput("HISTnames.selector"),
        uiOutput("HISTmulti.selector"),
        actionButton("hist.button", "Plot!"),
        downloadButton("hist.DW", "Download Plot")
      ),
      
      
      #BAR CHART
      conditionalPanel(
        condition = "input.TypePlot == 'bar'",
        uiOutput("BARx.selector"),
        uiOutput("BARy.selector"),
        uiOutput("BARfac.selector"),
        uiOutput("BARcol.selector"),
        uiOutput("Bar.order"),
        actionButton("barchart.button", "Plot!"),
        downloadButton("bar.DW", "Download Plot")
      ),
      
      
      #INTERACTION PLOT
      conditionalPanel(
        condition = "input.TypePlot == 'inter'",
        uiOutput("INTER_F1.selector"),
        uiOutput("INTER_F2.selector"),
        uiOutput("INTER_RES.selector"),
        uiOutput("INTER_ERR.selector"),
        actionButton("inter.button", "Plot!"),
        downloadButton("inter.DW", "Download Plot")
      ),
      
      
      #BOX PLOT
      conditionalPanel(
        condition = "input.TypePlot == 'box'",
        uiOutput("BOXx.selector"),
        uiOutput("BOXy.selector"),
        uiOutput("BOXcol.selector"),
        uiOutput("BOXnotch.selector"),
        actionButton("box.button", "Plot!"),
        downloadButton("box.DW", "Download Plot")
      ),
      
      
      #SCATTERPLOT
      conditionalPanel(
        condition = "input.TypePlot == 'points'",
        uiOutput("SCPx.selector"),
        uiOutput("SCPy.selector"),
        uiOutput("SCPcol.selector"),
        uiOutput("SCPsize.selector"),
        uiOutput("SCPfacet.selector"),
        actionButton("sct.button", "Plot!"),
        downloadButton("sct.DW", "Download Plot")
      ),
      
    
      
      
      #TIME SERIES
      conditionalPanel(
        condition = "input.TypePlot == 'ts'",
        uiOutput("TSx.selector"),
        uiOutput("TSy.selector"),
        uiOutput("TScol.selector"),
        uiOutput("TS.format"),
        actionButton("ts.button", "Plot!"),
        downloadButton("ts.DW", "Download Plot")
      ),
      br(),
      br(),
      tags$b("Specify Width, Height and dpi of TIFF file"), ("(default are 17.8 x 17.8 cm - 300dpi):"),
      br(),
      br(),
      textInput("wdt", "Width of the TIFF file", ""),
      textInput("hgt", "Height of the TIFF file", ""),
      selectInput("unt", "Units", 
                  c("cm", "mm", "in")),
      selectInput("dpi", "Resolution", 
                  c("300", "600", "150"))
      
    ),
    
    
    #MAIN
    mainPanel(
      #HISTOGRAM
      conditionalPanel(condition = "input.TypePlot == 'hist'",
                       withSpinner(plotOutput('histogram'))),
      
      #BAR CHART
      conditionalPanel(condition = "input.TypePlot == 'bar'",
                       withSpinner(plotOutput('barchart'))),
      
      #Interaction PLOT
      conditionalPanel(condition = "input.TypePlot == 'inter'",
                       withSpinner(plotOutput('interaction'))),
      
      #BOX PLOT
      conditionalPanel(condition = "input.TypePlot == 'box'",
                       withSpinner(plotOutput('boxplot'))),
      
      
      #SCATTERPLOT
      conditionalPanel(condition = "input.TypePlot == 'points'",
                       withSpinner(plotOutput('scatterplot'))),
      
      
      #TIME-SERIES
      conditionalPanel(condition = "input.TypePlot == 'ts'",
                       withSpinner(plotOutput('time_series')))
      
    )
    
  )
))
