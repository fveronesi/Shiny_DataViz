shinyServer(function(input, output) {
  require(ggplot2)
  require(Rmisc)
  
  
  #LOADING DATA
  DATA = eventReactive(input$Data, {
    FILE <- input$Data
    
    if (is.null(FILE))
      return(NULL)
    
    read.table(FILE$datapath, header = T, sep = input$separator)
  })
  
  CLASSES = eventReactive(input$Data, {
    sapply(DATA(), class)
  })
  
  
  
  
  #HISTOGRAM
  hist.plot <- eventReactive(input$hist.button, {
    
    if (input$multi != "None") {
      data.histogram <-
        data.frame(var = DATA()[,input$hist.x], multi = DATA()[,input$multi])
      
      plot <- ggplot(data = data.histogram,aes(x = var)) +
        geom_histogram() +
        xlab(paste(input$hist.x)) +
        ylab("Frequency") +
        ggtitle(paste("Histogram of",input$hist.x)) +
        facet_wrap(~ multi) +
        theme_minimal()
      
      print(plot)
    } else {
      data.histogram <- data.frame(var = DATA()[,input$hist.x])
      
      plot <- ggplot(data = data.histogram,aes(x = var)) +
        geom_histogram() +
        xlab(paste(input$hist.x)) +
        ylab("Frequency") +
        ggtitle(paste("Histogram of",input$hist.x)) +
        theme_minimal()
      
      print(plot)
    }
    
    
  })
  
  #BARCHART
  barchart.plot <- eventReactive(input$barchart.button, {
    
    
    #Here we add a condition to allow users not to fill the bar-chart
    if (input$fac.bar != 0 & input$col.bar == 0) {
      data.barchart <-
        data.frame(x = DATA()[,input$x.bar],
                   y = DATA()[,input$y.bar],
                   facet = DATA()[,input$fac.bar])
      
      
      ggplot(data.barchart, aes(x=reorder(x,y), y=y)) +
        stat_summary(geom = "bar", fun.y = mean, fill="grey66") +
        stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.5) +
        facet_wrap(~facet) + 
        theme_minimal() +
        xlab(input$x.bar) + 
        ylab(input$y.bar)
      
    } else if(input$fac.bar == 0 & input$col.bar == 0){
      data.barchart <-
        data.frame(x = DATA()[,input$x.bar],
                   y = DATA()[,input$y.bar])
      
      
      ggplot(data.barchart, aes(x=reorder(x,y), y=y)) +
        stat_summary(geom = "bar", fun.y = mean, fill="grey66") +
        stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.5) +
        theme_minimal() +
        xlab(input$x.bar) + 
        ylab(input$y.bar)
      
    } else if(input$fac.bar == 0 & input$col.bar != 0){
      data.barchart <-
        data.frame(x = DATA()[,input$x.bar],
                   y = DATA()[,input$y.bar],
                   color = DATA()[,input$col.bar])
      
      ggplot(data.barchart, aes(x=reorder(x,y), y=y, fill=color)) +
        stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
        stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.5) +
        theme_minimal() +
        xlab(input$x.bar) + 
        ylab(input$y.bar)
      
    } else {
      data.barchart <-
        data.frame(x = DATA()[,input$x.bar],
                   y = DATA()[,input$y.bar],
                   color = DATA()[,input$col.bar],
                   facet = DATA()[,input$fac.bar])
      
      ggplot(data.barchart, aes(x=reorder(x,y), y=y, fill=color)) +
        stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
        stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
        facet_wrap(~facet) +
        theme_minimal() +
        xlab(input$x.bar) + 
        ylab(input$y.bar)
      
    }
  })
  
  #BOXPLOT
  boxplot.plot <- eventReactive(input$box.button, {
    
    
    data.boxplot <-
      data.frame(x = DATA()[,input$x.box],
                 y = DATA()[,input$y.box])
    
    plot <- ggplot(data = data.boxplot,aes(x = x, y = y)) +
      geom_boxplot() +
      xlab(paste(input$x.box)) +
      ylab(paste(input$y.box)) +
      theme_minimal()
    
    print(plot)
  })
  
  
  #SCATTERPLOT
  scatterplot.plot <- eventReactive(input$sct.button, {
    
    
    if (input$col.sct != 0 & input$size.sct == 0 & input$facet.sct == 0) {
      data.scatterplot <-
        data.frame(x = DATA()[,input$x.sct],
                   y = DATA()[,input$y.sct],
                   color = DATA()[,input$col.sct])
      
      plot <- ggplot(data = data.scatterplot, aes(x = x, y = y)) +
        geom_point(mapping = aes(color = color)) +
        ggtitle(paste0("Scatterplot ",input$x.sct, " vs. ", input$y.sct)) +
        labs(color = paste(input$col.sct)) +
        xlab(paste(input$x.sct)) +
        ylab(paste(input$y.sct)) +
        theme_minimal()
      
      print(plot)
      
    } else if (input$col.sct != 0 & input$size.sct != 0 & input$facet.sct == 0) {
      data.scatterplot <-
        data.frame(
          x = DATA()[,input$x.sct],
          y = DATA()[,input$y.sct],
          color = DATA()[,input$col.sct],
          size = DATA()[,input$size.sct]
        )
      
      plot <- ggplot(data = data.scatterplot, aes(x = x, y = y)) +
        geom_point(mapping = aes(color = color, size = size)) +
        ggtitle(paste0("Scatterplot ",input$x.sct, " vs. ", input$y.sct)) +
        labs(color = paste(input$col.sct), size = paste(input$size.sct)) +
        xlab(paste(input$x.sct)) +
        ylab(paste(input$y.sct)) +
        theme_minimal()
      
      
      print(plot)
    } else if(input$col.sct != 0 & input$size.sct != 0 & input$facet.sct != 0){
      data.scatterplot <-
        data.frame(
          x = DATA()[,input$x.sct],
          y = DATA()[,input$y.sct],
          color = DATA()[,input$col.sct],
          size = DATA()[,input$size.sct],
          facet = DATA()[,input$facet.sct]
        )
      
      plot <- ggplot(data = data.scatterplot, aes(x = x, y = y)) +
        geom_point(mapping = aes(color = color, size = size)) +
        ggtitle(paste0("Scatterplot ",input$x.sct, " vs. ", input$y.sct)) +
        xlab(paste(input$x.sct)) +
        ylab(paste(input$y.sct)) +
        theme_bw()+
        facet_wrap(~facet) +
        geom_smooth(method = "lm", se = TRUE)
      
      print(plot)
    } else if(input$col.sct == 0 & input$size.sct == 0 & input$facet.sct == 0){
      data.scatterplot <-
        data.frame(x = DATA()[,input$x.sct],
                   y = DATA()[,input$y.sct])
      
      plot <- ggplot(data = data.scatterplot, aes(x = x, y = y)) +
        geom_point() +
        ggtitle(paste0("Scatterplot ",input$x.sct, " vs. ", input$y.sct)) +
        xlab(paste(input$x.sct)) +
        ylab(paste(input$y.sct)) +
        theme_minimal()
      
      print(plot)
      
    } else if(input$col.sct != 0 & input$size.sct == 0 & input$facet.sct != 0){
      data.scatterplot <-
        data.frame(
          x = DATA()[,input$x.sct],
          y = DATA()[,input$y.sct],
          color = DATA()[,input$col.sct],
          facet = DATA()[,input$facet.sct]
        )
      
      plot <- ggplot(data = data.scatterplot, aes(x = x, y = y)) +
        geom_point(mapping = aes(color = color)) +
        ggtitle(paste0("Scatterplot ",input$x.sct, " vs. ", input$y.sct)) +
        xlab(paste(input$x.sct)) +
        ylab(paste(input$y.sct)) +
        theme_bw()+
        facet_wrap(~facet) +
        geom_smooth(method = "lm", se = TRUE)
      
      print(plot)
      
    } else if(input$col.sct == 0 & input$size.sct == 0 & input$facet.sct != 0){
      data.scatterplot <-
        data.frame(
          x = DATA()[,input$x.sct],
          y = DATA()[,input$y.sct],
          facet = DATA()[,input$facet.sct]
        )
      
      plot <- ggplot(data = data.scatterplot, aes(x = x, y = y)) +
        geom_point() +
        ggtitle(paste0("Scatterplot ",input$x.sct, " vs. ", input$y.sct)) +
        xlab(paste(input$x.sct)) +
        ylab(paste(input$y.sct)) +
        theme_bw()+
        facet_wrap(~facet) +
        geom_smooth(method = "lm", se = TRUE)
      
      print(plot)
      
    }
  })
  
  #TIME-SERIES
  time_series.plot <- eventReactive(input$ts.button, {

    data.time_series <-
      data.frame(x = as.Date(DATA()[,input$x.ts]),
                 y = DATA()[,input$y.ts])
    
    plot <- ggplot(data = data.time_series,aes(x = x, y = y)) +
      geom_line() +
      xlab("Date") +
      ylab(paste(input$y.ts)) +
      theme_minimal()
    
    print(plot)
  })
  
  #PLOTTING OUTPUTS
  
  output$histogram <- renderPlot({
    output$HISTnames.selector <- renderUI({
      selectInput(
        inputId = "hist.x", label = "Select the variable to plot:", 
        choices = names(DATA())[CLASSES() == "numeric"])
    })
    
    output$HISTmulti.selector <- renderUI({
      selectInput(
        inputId = "multi", label = "Select the facets variable:", 
        choices = c("None",names(DATA())[CLASSES() == "factor"]))
    })
    
    print(hist.plot())
  })
  
  
  output$barchart <- renderPlot({
  output$BARx.selector <- renderUI({
      selectInput(
        inputId = "x.bar", label = "Select the variable for X:",
        choices = c(None = 0,names(DATA())[CLASSES() == "factor"])
      )
    })
    
    output$BARy.selector <- renderUI({
      selectInput(
        inputId = "y.bar", label = "Select the variable for Y:",
        choices = c(None = 0,names(DATA())[CLASSES() == "numeric"])
      )
    })
    
    output$BARfac.selector <- renderUI({
      selectInput(
        inputId = "fac.bar", label = "Select the variable for faceting:",
        choices = c(None = 0,names(DATA())[CLASSES() == "factor"])
      )
    })
    
    output$BARcol.selector <- renderUI({
      selectInput(
        inputId = "col.bar", label = "Select the variable for color:",
        choices = c(None = 0,names(DATA())[CLASSES() == "factor"])
      )
    })
	
    print(barchart.plot())
  })
  
  
  output$boxplot <- renderPlot({
  output$BOXx.selector <- renderUI({
      selectInput(
        inputId = "x.box", label = "Select the variable for X:",
        choices = names(DATA())[CLASSES() == "factor"]
      )
    })
    
    output$BOXy.selector <- renderUI({
      selectInput(
        inputId = "y.box", label = "Select the variable for Y:",
        choices = names(DATA())[CLASSES() == "numeric"]
      )
    })
	
    print(boxplot.plot())
  })
  
  
  output$scatterplot <- renderPlot({
  output$SCPx.selector <- renderUI({
      selectInput(
        inputId = "x.sct", label = "Select the variable for X:",
        choices = c(None = 0,names(DATA())[CLASSES() == "numeric"])
      )
    })
    
    output$SCPy.selector <- renderUI({
      selectInput(
        inputId = "y.sct", label = "Select the variable for Y:",
        choices = c(None = 0,names(DATA())[CLASSES() == "numeric"])
      )
    })
    
    output$SCPcol.selector <- renderUI({
      selectInput(
        inputId = "col.sct", label = "Select the variable for color:",
        choices = c(None = 0,names(DATA())[CLASSES() == "numeric"])
      )
    })
    
    output$SCPsize.selector <- renderUI({
      selectInput(
        inputId = "size.sct", label = "Select the variable for size:",
        choices = c(None = 0,names(DATA())[CLASSES() == "numeric"])
      )
    })
    
    output$SCPfacet.selector <- renderUI({
      selectInput(
        inputId = "facet.sct", label = "Select the variable for faceting:",
        choices = c(None = 0,names(DATA())[CLASSES() == "factor"])
      )
    })
	
    print(scatterplot.plot())
  })
  
  
  output$time_series <- renderPlot({
  output$TSx.selector <- renderUI({
      selectInput(
        inputId = "x.ts", label = "Select the variable for X:",
        choices = names(DATA())[CLASSES() == "factor"]
      )
    })
    
    output$TSy.selector <- renderUI({
      selectInput(
        inputId = "y.ts", label = "Select the variable for Y:",
        choices = names(DATA())[CLASSES() == "numeric"]
      )
    })
	
    print(time_series.plot())
  })
  
  
  #DOWNLOAD
  output$hist.DW <- downloadHandler(
      filename =  function() {
        paste0("Histogram.tiff")

      },
      content = function(file) {
        ggsave(filename=file, dpi = 600)
      }
    )
  
  
  output$box.DW <- downloadHandler(
    filename =  function() {
      paste0("BoxPlot.tiff")
      
    },
    content = function(file) {
      ggsave(filename=file, dpi = 600)
    }
  )
  
  
  output$bar.DW <- downloadHandler(
    filename =  function() {
      paste0("Barchart.tiff")
      
    },
    content = function(file) {
      ggsave(filename=file, dpi = 600)
    }
  )
  
  
  output$sct.DW <- downloadHandler(
    filename =  function() {
      paste0("Scatterplot.tiff")
      
    },
    content = function(file) {
      ggsave(filename=file, dpi = 600)
    }
  )
  
  
  output$ts.DW <- downloadHandler(
    filename =  function() {
      paste0("TimeSeries.tiff")
      
    },
    content = function(file) {
      ggsave(filename=file, dpi = 600)
    }
  )
  
  
  
})
