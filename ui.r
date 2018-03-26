library(shiny)
  
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
      ),
      
      tags$hr(),
      
      h5(strong("Plot specifications for download")),
      
      radioButtons(inputId = "var3", label = strong("Select the file type"), choices = list("pdf", "png"),
                   selected = "pdf", inline = TRUE),
      
      # sliderInput("width",
      #             strong("Plot width (in inch)"),
      #             1, 15, 8, .1),
      
      sliderInput("height",
                  strong("Plot height (in inch)"),
                  1, 15, 5, .1),
      
      selectInput("ratio", label = strong("Aspect ratio"),
                  choices = list("2.414:1" = "2.414",
                                 "1.618:1" = "1.618",
                                 "3:2" = "1.5",
                                 "4:3" = "1.333333",
                                 "1:1" = "1",
                                 "3:4" = "0.75",
                                 "2:3" = "0.6666667",
                                 "1:1.618" = "0.618047",
                                 "1:2.414" = "0.4142502"),
                  selected = "1.618"),
      downloadButton("downPlot", "Download current plot")
      
      
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
