shinyServer(function(input, output) {
  require(ggplot2)
  require(Rmisc)
  require(Hmisc)
  
  options(shiny.sanitize.errors = F)
  
  med.CI = function(x){
    y=median(x)
    CI=1.57*(IQR(x)/sqrt(length(x)))
    data.frame(y=y, ymin=y-CI, ymax=y+CI)
  }
  
  
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
  
  
  
  #UI
  observeEvent(input$TypePlot == "hist",{
    output$HISTnames.selector <- renderUI({
      selectInput(
        inputId = "hist.x", label = "Select the variable to plot:", 
        choices = names(DATA())[CLASSES() %in% c("numeric", "integer")])
    })
    
    output$HISTmulti.selector <- renderUI({
      selectInput(
        inputId = "multi", label = "Select the facets variable:", 
        choices = c("None",names(DATA())[CLASSES() == "factor"]))
    })
    
  })
  
  
  observeEvent(input$TypePlot == "bar", {

    output$BARx.selector <- renderUI({
      selectInput(
        inputId = "x.bar", label = "Select the variable for X:",
        choices = c(None = 0,names(DATA())[CLASSES() == "factor"])
      )
    })
    
    output$BARy.selector <- renderUI({
      selectInput(
        inputId = "y.bar", label = "Select the variable for Y:",
        choices = c(None = 0,names(DATA())[CLASSES() %in% c("numeric", "integer")])
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
    
    
  })
  
  
  observeEvent(input$TypePlot == "inter", {
    output$INTER.normal <- renderUI({
      checkboxInput(inputId="inter.norm", label="Normal Distribution", value=T)
    })
    
    output$INTER_F1.selector <- renderUI({
      selectInput(
        inputId = "F1.inter", label = "Select the first categorical variable:",
        choices = c(None = 0,names(DATA())[CLASSES() == "factor"])
      )
    })
    
    output$INTER_F2.selector <- renderUI({
      selectInput(
        inputId = "F2.inter", label = "Select the second categorical variable:",
        choices = c(None = 0,names(DATA())[CLASSES() == "factor"])
      )
    })
    
    output$INTER_RES.selector <- renderUI({
      selectInput(
        inputId = "Resp.inter", label = "Select the response variable:",
        choices = c(None = 0,names(DATA())[CLASSES() %in% c("numeric", "integer")])
      )
    })
    
    
    output$INTER_ERR.selector <- renderUI({
      checkboxInput(
        inputId = "Err.inter", label = "Error bars",value = FALSE
      )
    })
    
  })
  
  
  
  observeEvent(input$TypePlot == "box", {
    output$BOXx.selector <- renderUI({
      selectInput(
        inputId = "x.box", label = "Select the variable for X:",
        choices = names(DATA())[CLASSES() == "factor"]
      )
    })
    
    output$BOXy.selector <- renderUI({
      selectInput(
        inputId = "y.box", label = "Select the variable for Y:",
        choices = names(DATA())[CLASSES() %in% c("numeric", "integer")]
      )
    })
    
    output$BOXcol.selector <- renderUI({
      selectInput(
        inputId = "col.box", label = "Select the variable for Color:",
        choices = c(None = 0,names(DATA())[CLASSES() == "factor"])
      )
    })
    
    output$BOXnotch.selector <- renderUI({
      checkboxInput(
        inputId = "notch.box", label = "Include Notches",
        value=F
      )
    })
    
  })
  
  observeEvent(input$TypePlot == "points", {
    
    output$SCPx.selector <- renderUI({
      selectInput(
        inputId = "x.sct", label = "Select the variable for X:",
        choices = c(None = 0,names(DATA())[CLASSES() %in% c("numeric", "integer")])
      )
    })
    
    output$SCPy.selector <- renderUI({
      selectInput(
        inputId = "y.sct", label = "Select the variable for Y:",
        choices = c(None = 0,names(DATA())[CLASSES() %in% c("numeric", "integer")])
      )
    })
    
    output$SCPcol.selector <- renderUI({
      selectInput(
        inputId = "col.sct", label = "Select the variable for color:",
        choices = c(None = 0,names(DATA())[CLASSES() %in% c("numeric", "integer")])
      )
    })
    
    output$SCPsize.selector <- renderUI({
      selectInput(
        inputId = "size.sct", label = "Select the variable for size:",
        choices = c(None = 0,names(DATA())[CLASSES() %in% c("numeric", "integer")])
      )
    })
    
    output$SCPfacet.selector <- renderUI({
      selectInput(
        inputId = "facet.sct", label = "Select the variable for faceting:",
        choices = c(None = 0,names(DATA())[CLASSES() == "factor"])
      )
    })
  })
  
  
  observeEvent(input$TypePlot == "ts", {
    
    output$TSx.selector <- renderUI({
      selectInput(
        inputId = "x.ts", label = "Select the variable for X:",
        choices = names(DATA())[CLASSES() == "factor"]
      )
    })
    
    output$TSy.selector <- renderUI({
      selectInput(
        inputId = "y.ts", label = "Select the variable for Y:",
        choices = names(DATA())[CLASSES() %in% c("numeric", "integer")]
      )
    })
    
    output$TScol.selector <- renderUI({
      selectInput(
        inputId = "col.ts", label = "Select the variable for color:",
        choices = c("None",names(DATA())[CLASSES() == "factor"])
      )
    })
    
    output$TS.format <- renderUI({
      textInput(
        inputId = "format.ts", label = tags$a(href="https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html", target="_blank","Date format:"),
        value = "%Y-%m-%d"
      )
    })
    
    
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
    
    if(input$vert.bar!=TRUE){
      if(input$bar.norm==T){
        #Here we add a condition to allow users not to fill the bar-chart
        if (input$fac.bar != 0 & input$col.bar == 0) {
          data.barchart <-
            data.frame(x = DATA()[,input$x.bar],
                       y = DATA()[,input$y.bar],
                       facet = DATA()[,input$fac.bar])
          
          if(input$order.bar){
            ggplot(data.barchart, aes(x=reorder(x,y), y=y)) +
              stat_summary(geom = "bar", fun.y = mean, fill="grey66") +
              stat_summary(geom = "errorbar", fun.data = mean_cl_normal, fun.args=list(mult=1.96), width = 0.2) +
              facet_wrap(~facet) + 
              xlab(input$x.bar) + 
              ylab(input$y.bar) +
              theme_minimal()
          } else {
            ggplot(data.barchart, aes(x=x, y=y)) +
              stat_summary(geom = "bar", fun.y = mean, fill="grey66") +
              stat_summary(geom = "errorbar", fun.data = mean_cl_normal, fun.args=list(mult=1.96), width = 0.2) +
              facet_wrap(~facet) + 
              xlab(input$x.bar) + 
              ylab(input$y.bar) +
              theme_minimal()
          }
          
        } else if(input$fac.bar == 0 & input$col.bar == 0){
          data.barchart <-
            data.frame(x = DATA()[,input$x.bar],
                       y = DATA()[,input$y.bar])
          
          if(input$order.bar){
            ggplot(data.barchart, aes(x=reorder(x,y), y=y)) +
              stat_summary(geom = "bar", fun.y = mean, fill="grey66") +
              stat_summary(geom = "errorbar", fun.data = mean_cl_normal, fun.args=list(mult=1.96), width = 0.2) +
              xlab(input$x.bar) + 
              ylab(input$y.bar) +
              theme_minimal()
            
          } else {
            ggplot(data.barchart, aes(x=x, y=y)) +
              stat_summary(geom = "bar", fun.y = mean, fill="grey66") +
              stat_summary(geom = "errorbar", fun.data = mean_cl_normal, fun.args=list(mult=1.96), width = 0.2) +
              xlab(input$x.bar) + 
              ylab(input$y.bar) +
              theme_minimal()
          }
          
        } else if(input$fac.bar == 0 & input$col.bar != 0){
          data.barchart <-
            data.frame(x = DATA()[,input$x.bar],
                       y = DATA()[,input$y.bar],
                       color = DATA()[,input$col.bar])
          
          if(input$order.bar){
            ggplot(data.barchart, aes(x=reorder(x,y), y=y, fill=color)) +
              stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
              stat_summary(geom = "errorbar", fun.data = mean_cl_normal, fun.args=list(mult=1.96), width=.2,position=position_dodge(.9)) +
              labs(fill=paste(input$col.bar)) +
              xlab(input$x.bar) + 
              ylab(input$y.bar) +
              theme_minimal()
            
          } else {
            ggplot(data.barchart, aes(x=x, y=y, fill=color)) +
              stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
              stat_summary(geom = "errorbar", fun.data = mean_cl_normal, fun.args=list(mult=1.96), width=.2,position=position_dodge(.9)) +
              labs(fill=paste(input$col.bar)) +
              xlab(input$x.bar) + 
              ylab(input$y.bar) +
              theme_minimal()
          }
          
        } else {
          data.barchart <-
            data.frame(x = DATA()[,input$x.bar],
                       y = DATA()[,input$y.bar],
                       color = DATA()[,input$col.bar],
                       facet = DATA()[,input$fac.bar])
          
          if(input$order.bar){
            ggplot(data.barchart, aes(x=reorder(x,y), y=y, fill=color)) +
              stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
              stat_summary(geom = "errorbar", fun.data = mean_cl_normal, fun.args=list(mult=1.96), width=.2,position=position_dodge(.9)) +
              labs(fill=paste(input$col.bar)) +
              facet_wrap(~facet) +
              xlab(input$x.bar) + 
              ylab(input$y.bar) + 
              theme_minimal()
            
          } else {
            ggplot(data.barchart, aes(x=x, y=y, fill=color)) +
              stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
              stat_summary(geom = "errorbar", fun.data = mean_cl_normal, fun.args=list(mult=1.96), width=.2,position=position_dodge(.9)) +
              labs(fill=paste(input$col.bar)) +
              facet_wrap(~facet) +
              xlab(input$x.bar) + 
              ylab(input$y.bar) + 
              theme_minimal()
          }
          
          
        }
      } else {
        
        #NON-NORMAL
        if (input$fac.bar != 0 & input$col.bar == 0) {
          data.barchart <-
            data.frame(x = DATA()[,input$x.bar],
                       y = DATA()[,input$y.bar],
                       facet = DATA()[,input$fac.bar])
          
          if(input$order.bar){
            ggplot(data.barchart, aes(x=reorder(x,y,FUN=median), y=y)) +
              stat_summary(geom = "bar", fun.y = median, fill="grey66") +
              stat_summary(geom = "errorbar", fun.data = med.CI, width = 0.2) +
              facet_wrap(~facet) + 
              xlab(input$x.bar) + 
              ylab(input$y.bar) +
              theme_minimal()
          } else {
            ggplot(data.barchart, aes(x=x, y=y)) +
              stat_summary(geom = "bar", fun.y = median, fill="grey66") +
              stat_summary(geom = "errorbar", fun.data = med.CI, width = 0.2) +
              facet_wrap(~facet) + 
              xlab(input$x.bar) + 
              ylab(input$y.bar) +
              theme_minimal()
          }
          
        } else if(input$fac.bar == 0 & input$col.bar == 0){
          data.barchart <-
            data.frame(x = DATA()[,input$x.bar],
                       y = DATA()[,input$y.bar])
          
          if(input$order.bar){
            ggplot(data.barchart, aes(x=reorder(x,y,FUN=median), y=y)) +
              stat_summary(geom = "bar", fun.y = median, fill="grey66") +
              stat_summary(geom = "errorbar", fun.data = med.CI, width = 0.2) +
              xlab(input$x.bar) + 
              ylab(input$y.bar) +
              theme_minimal()
            
          } else {
            ggplot(data.barchart, aes(x=x, y=y)) +
              stat_summary(geom = "bar", fun.y = median, fill="grey66") +
              stat_summary(geom = "errorbar", fun.data = med.CI, width = 0.2) +
              xlab(input$x.bar) + 
              ylab(input$y.bar) +
              theme_minimal()
          }
          
        } else if(input$fac.bar == 0 & input$col.bar != 0){
          data.barchart <-
            data.frame(x = DATA()[,input$x.bar],
                       y = DATA()[,input$y.bar],
                       color = DATA()[,input$col.bar])
          
          if(input$order.bar){
            ggplot(data.barchart, aes(x=reorder(x,y,FUN=median), y=y, fill=color)) +
              stat_summary(geom = "bar", fun.y = median, position = "dodge") +
              stat_summary(geom = "errorbar", fun.data = med.CI, width=.2,position=position_dodge(.9)) +
              labs(fill=paste(input$col.bar)) +
              xlab(input$x.bar) + 
              ylab(input$y.bar) +
              theme_minimal()
            
          } else {
            ggplot(data.barchart, aes(x=x, y=y, fill=color)) +
              stat_summary(geom = "bar", fun.y = median, position = "dodge") +
              stat_summary(geom = "errorbar", fun.data = med.CI, width=.2,position=position_dodge(.9)) +
              labs(fill=paste(input$col.bar)) +
              xlab(input$x.bar) + 
              ylab(input$y.bar) +
              theme_minimal()
          }
          
        } else {
          data.barchart <-
            data.frame(x = DATA()[,input$x.bar],
                       y = DATA()[,input$y.bar],
                       color = DATA()[,input$col.bar],
                       facet = DATA()[,input$fac.bar])
          
          if(input$order.bar){
            ggplot(data.barchart, aes(x=reorder(x,y,FUN=median), y=y, fill=color)) +
              stat_summary(geom = "bar", fun.y = median, position = "dodge") +
              stat_summary(geom = "errorbar", fun.data = med.CI, width=.2,position=position_dodge(.9)) +
              labs(fill=paste(input$col.bar)) +
              facet_wrap(~facet) +
              xlab(input$x.bar) + 
              ylab(input$y.bar) + 
              theme_minimal()
            
          } else {
            ggplot(data.barchart, aes(x=x, y=y, fill=color)) +
              stat_summary(geom = "bar", fun.y = median, position = "dodge") +
              stat_summary(geom = "errorbar", fun.data = med.CI, width=.2,position=position_dodge(.9)) +
              labs(fill=paste(input$col.bar)) +
              facet_wrap(~facet) +
              xlab(input$x.bar) + 
              ylab(input$y.bar) + 
              theme_minimal()
          }
          
          
        }
      }
    } else {
      if(input$bar.norm==T){
        #Here we add a condition to allow users not to fill the bar-chart
        if (input$fac.bar != 0 & input$col.bar == 0) {
          data.barchart <-
            data.frame(x = DATA()[,input$x.bar],
                       y = DATA()[,input$y.bar],
                       facet = DATA()[,input$fac.bar])
          
          if(input$order.bar){
            ggplot(data.barchart, aes(x=reorder(x,y), y=y)) +
              stat_summary(geom = "bar", fun.y = mean, fill="grey66") +
              stat_summary(geom = "errorbar", fun.data = mean_cl_normal, fun.args=list(mult=1.96), width = 0.2) +
              facet_wrap(~facet) + 
              xlab(input$x.bar) + 
              ylab(input$y.bar) +
              theme_minimal()+
              theme(axis.text.x = element_text(angle = 90, hjust = 1))
          } else {
            ggplot(data.barchart, aes(x=x, y=y)) +
              stat_summary(geom = "bar", fun.y = mean, fill="grey66") +
              stat_summary(geom = "errorbar", fun.data = mean_cl_normal, fun.args=list(mult=1.96), width = 0.2) +
              facet_wrap(~facet) + 
              xlab(input$x.bar) + 
              ylab(input$y.bar) +
              theme_minimal()+
              theme(axis.text.x = element_text(angle = 90, hjust = 1))
          }
          
        } else if(input$fac.bar == 0 & input$col.bar == 0){
          data.barchart <-
            data.frame(x = DATA()[,input$x.bar],
                       y = DATA()[,input$y.bar])
          
          if(input$order.bar){
            ggplot(data.barchart, aes(x=reorder(x,y), y=y)) +
              stat_summary(geom = "bar", fun.y = mean, fill="grey66") +
              stat_summary(geom = "errorbar", fun.data = mean_cl_normal, fun.args=list(mult=1.96), width = 0.2) +
              xlab(input$x.bar) + 
              ylab(input$y.bar) +
              theme_minimal()+
              theme(axis.text.x = element_text(angle = 90, hjust = 1))
            
          } else {
            ggplot(data.barchart, aes(x=x, y=y)) +
              stat_summary(geom = "bar", fun.y = mean, fill="grey66") +
              stat_summary(geom = "errorbar", fun.data = mean_cl_normal, fun.args=list(mult=1.96), width = 0.2) +
              xlab(input$x.bar) + 
              ylab(input$y.bar) +
              theme_minimal()+
              theme(axis.text.x = element_text(angle = 90, hjust = 1))
          }
          
        } else if(input$fac.bar == 0 & input$col.bar != 0){
          data.barchart <-
            data.frame(x = DATA()[,input$x.bar],
                       y = DATA()[,input$y.bar],
                       color = DATA()[,input$col.bar])
          
          if(input$order.bar){
            ggplot(data.barchart, aes(x=reorder(x,y), y=y, fill=color)) +
              stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
              stat_summary(geom = "errorbar", fun.data = mean_cl_normal, fun.args=list(mult=1.96), width=.2,position=position_dodge(.9)) +
              labs(fill=paste(input$col.bar)) +
              xlab(input$x.bar) + 
              ylab(input$y.bar) +
              theme_minimal()+
              theme(axis.text.x = element_text(angle = 90, hjust = 1))
            
          } else {
            ggplot(data.barchart, aes(x=x, y=y, fill=color)) +
              stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
              stat_summary(geom = "errorbar", fun.data = mean_cl_normal, fun.args=list(mult=1.96), width=.2,position=position_dodge(.9)) +
              labs(fill=paste(input$col.bar)) +
              xlab(input$x.bar) + 
              ylab(input$y.bar) +
              theme_minimal()+
              theme(axis.text.x = element_text(angle = 90, hjust = 1))
          }
          
        } else {
          data.barchart <-
            data.frame(x = DATA()[,input$x.bar],
                       y = DATA()[,input$y.bar],
                       color = DATA()[,input$col.bar],
                       facet = DATA()[,input$fac.bar])
          
          if(input$order.bar){
            ggplot(data.barchart, aes(x=reorder(x,y), y=y, fill=color)) +
              stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
              stat_summary(geom = "errorbar", fun.data = mean_cl_normal, fun.args=list(mult=1.96), width=.2,position=position_dodge(.9)) +
              labs(fill=paste(input$col.bar)) +
              facet_wrap(~facet) +
              xlab(input$x.bar) + 
              ylab(input$y.bar) + 
              theme_minimal()+
              theme(axis.text.x = element_text(angle = 90, hjust = 1))
            
          } else {
            ggplot(data.barchart, aes(x=x, y=y, fill=color)) +
              stat_summary(geom = "bar", fun.y = mean, position = "dodge") +
              stat_summary(geom = "errorbar", fun.data = mean_cl_normal, fun.args=list(mult=1.96), width=.2,position=position_dodge(.9)) +
              labs(fill=paste(input$col.bar)) +
              facet_wrap(~facet) +
              xlab(input$x.bar) + 
              ylab(input$y.bar) + 
              theme_minimal()+
              theme(axis.text.x = element_text(angle = 90, hjust = 1))
          }
          
          
        }
      } else {
        
        #NON-NORMAL
        if (input$fac.bar != 0 & input$col.bar == 0) {
          data.barchart <-
            data.frame(x = DATA()[,input$x.bar],
                       y = DATA()[,input$y.bar],
                       facet = DATA()[,input$fac.bar])
          
          if(input$order.bar){
            ggplot(data.barchart, aes(x=reorder(x,y,FUN=median), y=y)) +
              stat_summary(geom = "bar", fun.y = median, fill="grey66") +
              stat_summary(geom = "errorbar", fun.data = med.CI, width = 0.2) +
              facet_wrap(~facet) + 
              xlab(input$x.bar) + 
              ylab(input$y.bar) +
              theme_minimal()+
              theme(axis.text.x = element_text(angle = 90, hjust = 1))
          } else {
            ggplot(data.barchart, aes(x=x, y=y)) +
              stat_summary(geom = "bar", fun.y = median, fill="grey66") +
              stat_summary(geom = "errorbar", fun.data = med.CI, width = 0.2) +
              facet_wrap(~facet) + 
              xlab(input$x.bar) + 
              ylab(input$y.bar) +
              theme_minimal()+
              theme(axis.text.x = element_text(angle = 90, hjust = 1))
          }
          
        } else if(input$fac.bar == 0 & input$col.bar == 0){
          data.barchart <-
            data.frame(x = DATA()[,input$x.bar],
                       y = DATA()[,input$y.bar])
          
          if(input$order.bar){
            ggplot(data.barchart, aes(x=reorder(x,y,FUN=median), y=y)) +
              stat_summary(geom = "bar", fun.y = median, fill="grey66") +
              stat_summary(geom = "errorbar", fun.data = med.CI, width = 0.2) +
              xlab(input$x.bar) + 
              ylab(input$y.bar) +
              theme_minimal()+
              theme(axis.text.x = element_text(angle = 90, hjust = 1))
            
          } else {
            ggplot(data.barchart, aes(x=x, y=y)) +
              stat_summary(geom = "bar", fun.y = median, fill="grey66") +
              stat_summary(geom = "errorbar", fun.data = med.CI, width = 0.2) +
              xlab(input$x.bar) + 
              ylab(input$y.bar) +
              theme_minimal()+
              theme(axis.text.x = element_text(angle = 90, hjust = 1))
          }
          
        } else if(input$fac.bar == 0 & input$col.bar != 0){
          data.barchart <-
            data.frame(x = DATA()[,input$x.bar],
                       y = DATA()[,input$y.bar],
                       color = DATA()[,input$col.bar])
          
          if(input$order.bar){
            ggplot(data.barchart, aes(x=reorder(x,y,FUN=median), y=y, fill=color)) +
              stat_summary(geom = "bar", fun.y = median, position = "dodge") +
              stat_summary(geom = "errorbar", fun.data = med.CI, width=.2,position=position_dodge(.9)) +
              labs(fill=paste(input$col.bar)) +
              xlab(input$x.bar) + 
              ylab(input$y.bar) +
              theme_minimal()+
              theme(axis.text.x = element_text(angle = 90, hjust = 1))
            
          } else {
            ggplot(data.barchart, aes(x=x, y=y, fill=color)) +
              stat_summary(geom = "bar", fun.y = median, position = "dodge") +
              stat_summary(geom = "errorbar", fun.data = med.CI, width=.2,position=position_dodge(.9)) +
              labs(fill=paste(input$col.bar)) +
              xlab(input$x.bar) + 
              ylab(input$y.bar) +
              theme_minimal()+
              theme(axis.text.x = element_text(angle = 90, hjust = 1))
          }
          
        } else {
          data.barchart <-
            data.frame(x = DATA()[,input$x.bar],
                       y = DATA()[,input$y.bar],
                       color = DATA()[,input$col.bar],
                       facet = DATA()[,input$fac.bar])
          
          if(input$order.bar){
            ggplot(data.barchart, aes(x=reorder(x,y,FUN=median), y=y, fill=color)) +
              stat_summary(geom = "bar", fun.y = median, position = "dodge") +
              stat_summary(geom = "errorbar", fun.data = med.CI, width=.2,position=position_dodge(.9)) +
              labs(fill=paste(input$col.bar)) +
              facet_wrap(~facet) +
              xlab(input$x.bar) + 
              ylab(input$y.bar) + 
              theme_minimal()+
              theme(axis.text.x = element_text(angle = 90, hjust = 1))
            
          } else {
            ggplot(data.barchart, aes(x=x, y=y, fill=color)) +
              stat_summary(geom = "bar", fun.y = median, position = "dodge") +
              stat_summary(geom = "errorbar", fun.data = med.CI, width=.2,position=position_dodge(.9)) +
              labs(fill=paste(input$col.bar)) +
              facet_wrap(~facet) +
              xlab(input$x.bar) + 
              ylab(input$y.bar) + 
              theme_minimal()+
              theme(axis.text.x = element_text(angle = 90, hjust = 1))
          }
          
          
        }
      }
      
    }
    
    
  })
  
  
  #interaction
  INTERACTION.plot <- eventReactive(input$inter.button, {

      data.interaction <-
        data.frame(F1 = DATA()[,input$F1.inter],
                   F2 = DATA()[,input$F2.inter],
                   RESP = DATA()[,input$Resp.inter])
      
      if(input$inter.norm==T){
        if(input$Err.inter==TRUE){
          ggplot(data = data.interaction, aes(x = F1, y = RESP, color=F2, group=F2)) +
            stat_summary(fun.y = mean, geom = "point") +
            stat_summary(fun.y = mean, geom = "line") +
            stat_summary(geom = "errorbar", fun.data = mean_cl_normal, fun.args=list(mult=1.96), width=0.1) +
            labs(color=paste(input$F2.inter)) +
            xlab(paste(input$F1.inter)) +
            ylab(paste(input$Resp.inter)) +
            theme_minimal()
        } else {
          ggplot(data = data.interaction, aes(x = F1, y = RESP, color=F2, group=F2)) +
            stat_summary(fun.y = mean, geom = "point") +
            stat_summary(fun.y = mean, geom = "line") +
            labs(color=paste(input$F2.inter)) +
            xlab(paste(input$F1.inter)) +
            ylab(paste(input$Resp.inter)) +
            theme_minimal()
        }
      } else {
        if(input$Err.inter==TRUE){
          ggplot(data = data.interaction, aes(x = F1, y = RESP, color=F2, group=F2)) +
            stat_summary(fun.y = median, geom = "point") +
            stat_summary(fun.y = median, geom = "line") +
            stat_summary(geom = "errorbar", fun.data = med.CI, width=0.1) +
            labs(color=paste(input$F2.inter)) +
            xlab(paste(input$F1.inter)) +
            ylab(paste(input$Resp.inter)) +
            theme_minimal()
        } else {
          ggplot(data = data.interaction, aes(x = F1, y = RESP, color=F2, group=F2)) +
            stat_summary(fun.y = mean, geom = "point") +
            stat_summary(fun.y = mean, geom = "line") +
            labs(color=paste(input$F2.inter)) +
            xlab(paste(input$F1.inter)) +
            ylab(paste(input$Resp.inter)) +
            theme_minimal()
        }
      }
      
      
     
  })
  
  
  
  #BOXPLOT
  boxplot.plot <- eventReactive(input$box.button, {
    
    if(input$col.box == 0){
      data.boxplot <-
        data.frame(x = DATA()[,input$x.box],
                   y = DATA()[,input$y.box])
      
      if(input$notch.box){
        ggplot(data = data.boxplot,aes(x = x, y = y)) +
          geom_boxplot(notch=T) +
          xlab(paste(input$x.box)) +
          ylab(paste(input$y.box)) +
          theme_minimal()
      } else {
        ggplot(data = data.boxplot,aes(x = x, y = y)) +
          geom_boxplot() +
          xlab(paste(input$x.box)) +
          ylab(paste(input$y.box)) +
          theme_minimal()
      }
      
    } else {
      data.boxplot <-
        data.frame(x = DATA()[,input$x.box],
                   y = DATA()[,input$y.box],
                   col = DATA()[,input$col.box])
      
      if(input$notch.box){
        ggplot(data = data.boxplot,aes(x = x, y = y, fill=col)) +
          labs(fill=paste(input$col.box)) + 
          geom_boxplot(position = "dodge", notch=T) +
          xlab(paste(input$x.box)) +
          ylab(paste(input$y.box)) +
          theme_minimal()
      } else {
        ggplot(data = data.boxplot,aes(x = x, y = y, fill=col)) +
          geom_boxplot(position = "dodge") +
          xlab(paste(input$x.box)) +
          ylab(paste(input$y.box)) +
          theme_minimal()
      }
      
    }
    
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
        #ggtitle(paste0("Scatterplot ",input$x.sct, " vs. ", input$y.sct)) +
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
        #ggtitle(paste0("Scatterplot ",input$x.sct, " vs. ", input$y.sct)) +
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
        #ggtitle(paste0("Scatterplot ",input$x.sct, " vs. ", input$y.sct)) +
        labs(colour=paste(input$col.sct), size=paste(input$size.sct)) +
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
        #ggtitle(paste0("Scatterplot ",input$x.sct, " vs. ", input$y.sct)) +
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
        #ggtitle(paste0("Scatterplot ",input$x.sct, " vs. ", input$y.sct)) +
        labs(colour=input$col.sct) + 
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
        #ggtitle(paste0("Scatterplot ",input$x.sct, " vs. ", input$y.sct)) +
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
    
    if(input$col.ts=="None"){
      data.time_series <-
        data.frame(x = as.Date(DATA()[,input$x.ts], format=input$format.ts),
                   y = DATA()[,input$y.ts])
      
      ggplot(data = data.time_series,aes(x = x, y = y)) +
        geom_line() +
        xlab("Date") +
        ylab(paste(input$y.ts)) +
        theme_minimal()
    } else {
      data.time_series <-
        data.frame(x = as.Date(DATA()[,input$x.ts], format=input$format.ts),
                   y = DATA()[,input$y.ts],
                   col = DATA()[,input$col.ts])
      
      ggplot(data = data.time_series,aes(x = x, y = y, color=col)) +
        geom_line() +
        labs(color=paste(input$col.ts)) +
        xlab("Date") +
        ylab(paste(input$y.ts)) +
        theme_minimal()
    }
    
    
  })
  
  #PLOTTING OUTPUTS
  
  output$histogram <- renderPlot({
    print(hist.plot())
  })
  
  
  output$barchart <- renderPlot({
    print(barchart.plot())
  })
  
  
  output$interaction <- renderPlot({
    print(INTERACTION.plot())
  })
  
  
  output$boxplot <- renderPlot({
    print(boxplot.plot())
  })
  
  
  output$scatterplot <- renderPlot({
    print(scatterplot.plot())
  })
  
  
  output$time_series <- renderPlot({
    print(time_series.plot())
  })
  
  
  WIDTH = eventReactive(input$wdt,{
     if(input$wdt!=""){as.numeric(input$wdt)}else{NA}
    
  })
  
  HEIGTH = eventReactive(input$hgt,{
    if(input$hgt!=""){as.numeric(input$hgt)}else{NA}
  })
  

  
  #DOWNLOAD
  output$hist.DW <- downloadHandler(
    filename =  function() {
      paste0("Histogram",input$hist.x,".tiff")
      
    },
    content = function(file) {
      ggsave(filename=file, dpi = as.numeric(input$dpi), width=WIDTH(), height=HEIGTH(), units=input$unt)
    }
  )
  
  
  output$box.DW <- downloadHandler(
    filename =  function() {
      paste0("BoxPlot",input$x.box,"_",input$y.box,".tiff")
      
    },
    content = function(file) {
      ggsave(filename=file, dpi = as.numeric(input$dpi), width=WIDTH(), height=HEIGTH(), units=input$unt)
    }
  )
  
  
  output$bar.DW <- downloadHandler(
    filename =  function() {
      paste0("Barchart",input$x.bar,"_",input$y.bar,".tiff")
      
      
    },
    content = function(file) {
      ggsave(filename=file, dpi = as.numeric(input$dpi), width=WIDTH(), height=HEIGTH(), units=input$unt)
    }
  )
  
  
  output$inter.DW <- downloadHandler(
    filename =  function() {
      paste0("Interaction",input$F1.inter,"_",input$F2.inter,".tiff")
      
      
    },
    content = function(file) {
      ggsave(filename=file, dpi = as.numeric(input$dpi), width=WIDTH(), height=HEIGTH(), units=input$unt)
    }
  )
  
  
  output$sct.DW <- downloadHandler(
    filename =  function() {
      paste0("Scatterplot",input$x.sct,"_",input$y.sct,".tiff")
      
    },
    content = function(file) {
      ggsave(filename=file, dpi = as.numeric(input$dpi), width=WIDTH(), height=HEIGTH(), units=input$unt)
    }
  )
  
  
  output$ts.DW <- downloadHandler(
    filename =  function() {
      paste0("TimeSeries",input$y.ts,".tiff")
      
    },
    content = function(file) {
      ggsave(filename=file, dpi = as.numeric(input$dpi), width=WIDTH(), height=HEIGTH(), units=input$unt)
    }
  )
  
  
  
})
