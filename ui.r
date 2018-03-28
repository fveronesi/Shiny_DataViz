library(shinycssloaders)

shinyUI(fluidPage(
  titlePanel("Data Visualization in R"),
  
  sidebarLayout(
    sidebarPanel(
      
      helpText("Here you can select a file (csv or txt) and plot your data!"),
      
      selectInput("separator", "Data Separator:",
        c(Comma = ",",BlankSpace = " ",Semicolon = ";")),
      
      fileInput(inputId = "Data", label = "Select a CSV file:", multiple = F),
      
      selectInput("TypePlot", "Type of Plot", 
                  c("None",
                    BarChart = "bar",
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
        actionButton("barchart.button", "Plot!"),
        downloadButton("bar.DW", "Download Plot")
      ),
      
      
      #BOX PLOT
      conditionalPanel(
        condition = "input.TypePlot == 'box'",
        uiOutput("BOXx.selector"),
        uiOutput("BOXy.selector"),
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
        actionButton("ts.button", "Plot!"),
        downloadButton("ts.DW", "Download Plot")
      )
      
      
    ),
    
    
    #MAIN
    mainPanel(
      #HISTOGRAM
      conditionalPanel(condition = "input.TypePlot == 'hist'",
                       withSpinner(plotOutput('histogram'))),
      
      #BAR CHART
      conditionalPanel(condition = "input.TypePlot == 'bar'",
                       withSpinner(plotOutput('barchart'))),
      
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
