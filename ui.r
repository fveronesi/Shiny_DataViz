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
        actionButton("hist.button", "Plot!")
      ),
      
      
      #BAR CHART
      conditionalPanel(
        condition = "input.TypePlot == 'bar'",
        uiOutput("BARx.selector"),
        uiOutput("BARy.selector"),
        uiOutput("BARfac.selector"),
        uiOutput("BARcol.selector"),
        actionButton("barchart.button", "Plot!")
      ),
      
      
      #BOX PLOT
      conditionalPanel(
        condition = "input.TypePlot == 'box'",
        uiOutput("BOXx.selector"),
        uiOutput("BOXy.selector"),
        actionButton("box.button", "Plot!")
      ),
      
      
      #SCATTERPLOT
      conditionalPanel(
        condition = "input.TypePlot == 'points'",
        uiOutput("SCPx.selector"),
        uiOutput("SCPy.selector"),
        uiOutput("SCPcol.selector"),
        uiOutput("SCPsize.selector"),
        uiOutput("SCPfacet.selector"),
        actionButton("sct.button", "Plot!")
      ),
      
    
      
      
      #TIME SERIES
      conditionalPanel(
        condition = "input.TypePlot == 'ts'",
        uiOutput("TSx.selector"),
        uiOutput("TSy.selector"),
        actionButton("ts.button", "Plot!")
      )
      
      
    ),
    
    
    #MAIN
    mainPanel(
      #HISTOGRAM
      conditionalPanel(condition = "input.TypePlot == 'hist'",
                       plotOutput('histogram')),
      
      #BAR CHART
      conditionalPanel(condition = "input.TypePlot == 'bar'",
                       plotOutput('barchart')),
      
      #BOX PLOT
      conditionalPanel(condition = "input.TypePlot == 'box'",
                       plotOutput('boxplot')),
      
      
      #SCATTERPLOT
      conditionalPanel(condition = "input.TypePlot == 'points'",
                       plotOutput('scatterplot')),
      
      
      #TIME-SERIES
      conditionalPanel(condition = "input.TypePlot == 'ts'",
                       plotOutput('time_series'))
      
    )
    
  )
))
