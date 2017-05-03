if (!"deSolve" %in% installed.packages()) install.packages("deSolve")
library(deSolve)
if (!"shiny" %in% installed.packages()) install.packages("shiny")
library(shiny)
if (!"shinythemes" %in% installed.packages()) install.packages("shinythemes")
library(shiny)

## Comments 05/04/2017 ----
# seems to work at present; not yet sure how to change parameters in two places

## navbarpage returns input$select.tab which contains the name of the current tab ----

## Attention! ----
# if display is needed in two tabs, it needs to be generated twice, i.e. 
# output$tracePlot1 and output$tracePlot2 ...

# cost should be made proportional to y


source("model.r")

## a helper function for plotting ----
density.new<-function (data, nclass = 21, probability = F,...) 
{
  xx <- pretty(data, n = nclass)
  xx.h <- hist(data, breaks = xx, plot = F)
  xx.h.x <- xx.h$breaks
  xx.h.x <- xx.h.x[1:(length(xx.h.x) - 1)] + 0.5 * diff(xx.h.x)
  if (probability) {
    xx.h.y <- xx.h$density
  }
  else {
    xx.h.y <- xx.h$counts
  }
  return(list(x = xx.h.x, y = xx.h.y))
}

shinyServer(function(input, output) {

  ## parameters ----
  baseParams<-reactive({
    # fixed through scenario
    params<-scores.map[selectParams(),]
    beta.base<-params[4]
    y0<-params[5]
    contr<-params[6] # effectiveness of control
    value<-params[7]
    return(list(
      beta.base=beta.base,
      y0=y0,
      contr=contr,
      value=value
    ))
  })
  
  get.params<-function(){
    params<-baseParams()
    # general parameters
    Nrep<-input$Nrep
    N<-input$Nsize
    d.rate<-input$d.rate
    if (is.null(input$control.varmodel)) control.varmodel<-1 else control.varmodel<-input$control.varmodel
    if (is.null(input$control.prop)) control.prop<-0 else control.prop<-input$control.prop
    return(list(
         d.rate=d.rate,
         N=N,        
         y0=params$y0,
         beta.base=params$beta.base,
         beta=params$beta.base*input$beta.scale,
         beta.var=params$beta.base*input$beta.var,
         y0.var=params$y0*input$y0.var,
         effort=input$effort,
         contr=params$contr,
         control.cost=input$Cost,
         control.varmodel=control.varmodel,
         control.prop=control.prop,
         value=params$value,
         Nrep=Nrep,
         Nrep1=1:Nrep,             # replicates with control
         Nrep2=Nrep+1:Nrep,        # replicates without control
         Nrep0=1:min(10,Nrep)      # replicates to show
      ))
  }
  
  ## main simulation engine  ----
  DEoutput<-function(){
    # solve equations
    # this is very badly coded
    params<-get.params()
    S<-matrix(NA,nrow=length(times),ncol=2*params$Nrep)
    I<-matrix(NA,nrow=length(times),ncol=2*params$Nrep)
    for (i in params$Nrep1){
      params.run<-variability(params,contr.switch=1)
      yinit<-c(S=params$N-params.run$y0,I=params.run$y0)
      result<-ode(yinit,times,forest,params.run)
      S[,i]<-result[,2]
      I[,i]<-result[,3]
    }
    for (i in params$Nrep2){
      params.run<-variability(params,contr.switch=0)
      yinit<-c(S=params$N-params.run$y0,I=params.run$y0)
      result<-ode(yinit,times,forest,params.run)
      S[,i]<-result[,2]
      I[,i]<-result[,3]
    }
    return(list(S=S,I=I))
  }
  
  ## runtime; so that we can choose the type of the model ----
  get.output<-function(){
    return(DEoutput())
  }
  
  ## outputs; they need submitButton ----
  # plots - traces
  output$tracePlot <- renderPlot({
    params<-get.params()
    out<-get.output()
    par(mfrow = c(2, 1), mar = c(3, 1, 1, 1), oma = c(0, 3,0, 0))
    par(tcl = -0.2, pty = "m", cex = 1, las = 1, cex.axis = 1, 
        cex.lab = 1, mgp = c(3, 0.5, 0))
    plot(times,out$S[,1],type="l",ylim=c(0,100),xlab="",ylab="Healthy/Infected trees (%)")
    matlines(times,out$I[,params$Nrep0],type="l",col=2,lty=1)
    matlines(times,out$S[,params$Nrep0],type="l",col=3,lty=1)
    abline(h=0,lty=2,col=4)
    z<-profit(out$S[,params$Nrep0],out$I[,params$Nrep0],params,contr.switch=1,control.prop=params$control.prop)
    matplot(times,z,ylim=range(0,z),type="l",xlab="",ylab="Profit (weekly)",lty=1,col=1)
    abline(h=0,lty=2,col=4)
    mtext(side=1,line=2,"Time (weeks)",cex=1)
#    mtext(side=3,line=2,input$select.tab)
  })
  
  output$parsPlot <- renderPlot({
    params<-get.params()
    params.control<-variability(params,contr.switch=1,runs=1000)
#    params.no<-variability(params,contr.switch=0,runs=500)
    par(mfrow = c(3, 1), mar = c(3, 1, 1, 1), oma = c(0, 3,0, 0))
    par(tcl = -0.2, pty = "m", cex = 1, las = 1, cex.axis = 1, 
        cex.lab = 1, mgp = c(2, 0.5, 0))
    z<-density.new(params.control$beta,adjust=1)
    plot(z$x,z$y/max(z$y),xlim=range(z$x),col=1,lwd=2,type="s",ylab="Density",xlab="rate of spread")
    z<-density.new(params.control$y0,adjust=0.5)
    plot(z$x,z$y/max(z$y),xlim=range(0,params.control$N,z$x),col=1,lwd=2,type="s",ylab="Density",xlab="initial load")
    z<-density.new(params.control$effort*params.control$contr,adjust=1)
    plot(z$x,z$y/max(z$y),xlim=range(0,max(z$x),0.5),col=1,lwd=2,type="s",ylab="Density",xlab="control rate")
  })
  
  # plots - distributions
  output$distPlot <- renderPlot({
    params<-get.params()
    out<-get.output()
    par(mfrow = c(1, 1), mar = c(3, 1, 1, 1), oma = c(0, 3,0, 0))
    par(tcl = -0.2, pty = "m", cex = 1, las = 1, cex.axis = 1, 
        cex.lab = 1, mgp = c(3, 0.5, 0))
    z<-density(profit(out$S[,params$Nrep1],out$I[,params$Nrep1],params,1,control.prop=params$control.prop)[52,],adjust=1) # was 0.5
    z0<-density(profit(out$S[,params$Nrep2],out$I[,params$Nrep2],params,0,control.prop=params$control.prop)[52,],adjust=1)
    plot(z$x,z$y/max(z$y),xlim=range(z$x,z0$x),type="s",main="",col=2,lwd=3)
    lines(z0$x,z0$y/max(z0$y),type="s",col=4,lwd=3)
  })
  
  output$distPlot.report <- renderPlot({
    params<-get.params()
    out<-get.output()
    par(mfrow = c(1, 1), mar = c(3, 1, 1, 2), oma = c(0, 1,0, 1))
    par(tcl = -0.2, pty = "m", cex = 1, las = 1, cex.axis = 1, 
        cex.lab = 1, mgp = c(3, 0.5, 0))
    z<-density(profit(out$S[,params$Nrep1],out$I[,params$Nrep1],params,1,control.prop=params$control.prop)[52,],adjust=1) # was 0.5
    z0<-density(profit(out$S[,params$Nrep2],out$I[,params$Nrep2],params,0,control.prop=params$control.prop)[52,],adjust=1)
    plot(z$x,z$y/max(z$y),xlim=range(z$x,z0$x),type="s",main="",col=2,lwd=3)
    lines(z0$x,z0$y/max(z0$y),type="s",col=4,lwd=3)
  })
  
  # table
  output$table <- renderTable({
    params<-get.params()
    out<-get.output()
    x<-mean(out$S[52,params$Nrep1],na.rm=T)
    y<-mean(out$I[52,params$Nrep1],na.rm=T)
    y0<-mean(out$I[52,params$Nrep2],na.rm=T)
    z<-mean(profit(out$S[,params$Nrep1],out$I[,params$Nrep1],params,1,control.prop=params$control.prop)[52,])
    z0<-mean(profit(out$S[,params$Nrep2],out$I[,params$Nrep2],params,0,control.prop=params$control.prop)[52,])
    x<-rbind(      
      c("Healthy stock (%):",round(100*x/params$N,0)),
      c("Infected stock (%):",round(100*y/params$N,0)),
      c("Profit (control):",round(z,0)),
      c("Profit (do nothing):",round(z0,0))
    )
    colnames(x)<-c("Indicator","Value")
    return(x)
  },bordered=TRUE)
  
  output$table.report <- renderTable({
    params<-get.params()
    out<-get.output()
    x<-mean(out$S[52,params$Nrep1],na.rm=T)
    y<-mean(out$I[52,params$Nrep1],na.rm=T)
    y0<-mean(out$I[52,params$Nrep2],na.rm=T)
    z<-mean(profit(out$S[,params$Nrep1],out$I[,params$Nrep1],params,1,control.prop=params$control.prop)[52,])
    z0<-mean(profit(out$S[,params$Nrep2],out$I[,params$Nrep2],params,0,control.prop=params$control.prop)[52,])
    x<-rbind(      
      c("Healthy stock (%):",round(100*x,0)),
      c("Infected stock (%):",round(100*y,0)),
      c("Profit (control):",round(z,0)),
      c("Profit (do nothing):",round(z0,0))
    )
    colnames(x)<-c("Indicator","Value")
    return(x)
  },bordered=TRUE)
  
  output$parametersTable <- renderTable({
    params<-get.params()
      x<-rbind(
        c("Total area:",round(params$N,2)),
        c("Rate of spread:",round(params$beta.base,2)),
        c("Initial infected population:",round(params$y0,2)),
        c("Cost of control:",round(params$control.cost,5)),
        c("Value:",round(params$value,2))
      )
      colnames(x)<-c("Parameter","Value")
      return(x)
  },bordered=TRUE)
  
  output$parametersTable.report <- renderTable({
    params<-get.params()
    x<-rbind(
      c("Total area:",round(params$N,2)),
      c("Rate of spread:",round(params$beta.base,2)),
      c("Initial infected population:",round(params$y0,2)),
      c("Cost of control:",round(params$control.cost,5)),
      c("Value:",round(params$value,2))
    )
    colnames(x)<-c("Parameter","Value")
    return(x)
  },bordered=TRUE)
  
  output$scenarioTable.upload <- renderTable({
    params<-get.params()
    scores.in<-scoresInput()
    if (is.null(scores.in)) 
      return() 
    else {
        x<-scores.in
        colnames(x)<-c("Pest","Host","UK","Regulation","Likelihood","Impact","Value","Risk")
        return(x)
    }
  },bordered=TRUE)
  
  output$scenarioTable.scenarios <- renderTable({
    params<-get.params()
    scores.in<-scoresInput()
    if (is.null(scores.in)) 
      return() 
    else {
      x<-scores.in
      colnames(x)<-c("Pest","Host","UK","Regulation","Likelihood","Impact","Value","Risk")
      return(x)
    }
  },bordered=TRUE)
  
## helpers to output data
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$dataset, '.csv', sep='') },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )

  datasetInput <- reactive({
    params<-get.params()
    out<-get.output()
    switch(input$dataset,
           "Healthy" = params$N-out[52,1+params$Nrep1],
           "Infected" = out[52,1+params$Nrep1])
  })
  
  defaultScenario<-function(){
    return(data.frame("Pest"="Generic","Host"="Generic","UK"="Present","Regulation"="FC","Likelihood"=1,"Impact"=3,"Value"=1,"Risk"=40))
  }
  
  scoresInput <- reactive({
    inFile <- input$uploadScoresFile
    if (is.null(inFile)){
      uploadScores<-NULL
    } else {
      uploadScores<-read.csv(inFile$datapath,header=F,skip=1,colClasses=c(rep("character",4),rep("integer",4)),
               col.names=c("Pest","Host","UK","Regulation","Likelihood","Impact","Value","Risk"))
    }
    return(rbind.data.frame(defaultScenario(), uploadScores))
  })
  
  output$selectScenario <- renderUI({
    scores.in<-scoresInput()
    scores.choices<-setNames(1:nrow(scores.in),scores.in[,1])
    selectInput("scenarios", 
                label = h5("Select scenario:"), 
                choices = scores.choices,
                selected = 1)
  })
  
  selectParams <- reactive({
    scores.in<-scoresInput()
    if (is.null(input$scenarios)) 
      scores.base<-sum((as.numeric(scores.in[1,5:7])-c(0,1,1))*c(1,5,25))
    else
      scores.base<-sum((as.numeric(scores.in[input$scenarios,5:7])-c(0,1,1))*c(1,5,25))
    return(scores.base)
  })
  
  output$test.scenarios <- renderText({
    if (is.null(input$scenarios)) c(1,selectParams())
      else c(paste(input$scenarios),selectParams())
  })
  
})




