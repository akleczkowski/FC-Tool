## this is a single Shiny file version - although we keep model.r as a separate file for convenience ----

## install if needed and load required libraries ----
if (!"deSolve" %in% installed.packages()) install.packages("deSolve")
library(deSolve)
if (!"shiny" %in% installed.packages()) install.packages("shiny")
library(shiny)
if (!"shinythemes" %in% installed.packages()) install.packages("shinythemes")
library(shinythemes)
if (!"triangle" %in% installed.packages()) install.packages("triangle")
library(triangle)

## load the model ----
source("model.r")

## initiate the random generators ----
globalSeed<-NULL

## helper functions for plotting ----
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
bars.mine.sd = function (x, y, sd, loc.x = NULL, loc.y = NULL, col = 1, lwd = 1, 
                         lty = 1, width = NULL,na.rm=F,xbar=F) 
{
  if (is.null(loc.x)) 
    loc.x = x
  if (is.null(loc.y)) 
    loc.y = y
  if (is.null(width)) 
    w = 0.01 * mean(loc.x,na.rm=T)
  else w = width
  xx1 = loc.x - w
  xx2 = loc.x
  xx3 = loc.x + w
  yy = sd
  yy1 = loc.y - yy
  yy2 = loc.y
  yy3 = loc.y + yy
  segments(xx2, yy1, xx2, yy2, col = col, lwd = lwd, lty = lty)
  segments(xx2, yy2, xx2, yy3, col = col, lwd = lwd, lty = lty)
  segments(xx1, yy1, xx3, yy1, col = col, lwd = lwd, lty = lty)
  segments(xx1, yy3, xx3, yy3, col = col, lwd = lwd, lty = lty)
}

## for setting and saving random seed - ensuring that the simulations are random but reproducible ----
# the implementation is not as generic as I would like to, as it precludes stochastic models
# this is done whenever variability() is called and a new set of params.run is generated
# the seed is saved in parameters - currently not used
seed.init<-function(globalSeed){
  if (is.null(globalSeed)){ 
    as.numeric(Sys.time())-> t; set.seed((t - floor(t)) * 1e8 -> seed) 
    } else seed<-globalSeed
  return(seed)
}

## Server - this is where all calculations take place ----
server<-function(input, output) {
  
  ## base parameters ----
  baseParams<-reactive({
    # fixed through scenario
    params<-selectParams()
    N<-params[1]
    beta.base<-params[2] # to save the current value, we need to read in beta not beta base - not 4
    y0.base<-params[3] # ditto for y0 - not 8
    value.timber<-params[4]
    value.recreation<-params[5]
    value.landscape<-params[6]
    value.biodiversity<-params[7]
    value.carbon<-params[8]
    contr<-params[9] # effectiveness of control
    control.costs<-params[10]
    Nrep<-params[13]
    d.rate<-params[14]
    globalSeed<-seed.init(globalSeed)
    return(list(
      N=N,
      control.costs=control.costs,
      Nrep=Nrep,
      d.rate=d.rate,
      seed=globalSeed,
      beta.base=beta.base,
      y0.base=y0.base,
      contr=contr,
      value.timber=value.timber,
      value.recreation=value.recreation,
      value.landscape=value.landscape,
      value.biodiversity=value.biodiversity,
      value.carbon=value.carbon
    ))
  })

check.in<-function(xin,xbase){
  return(ifelse(is.null(xin),xbase,xin))
}
  
## load parameters and update them in real time ----     
  get.params<-function(){
    params.base<-baseParams()
    Nrep<-check.in(input$Nrep,params.base$Nrep)
    N<-check.in(input$Nsize,params.base$N)
    d.rate<-check.in(input$d.rate,params.base$d.rate)
#    if (is.null(input$control.varmodel)) control.varmodel<-1 else control.varmodel<-input$control.varmodel
#    if (is.null(input$control.prop)) control.prop<-1 else control.prop<-input$control.prop
# this is a quick fudge to make things work; needs to be sorted out
    if (!is.null(input$y0)) params.base$y0.base<-input$y0
    control.varmodel<-0
    control.prop<-1
    return(list(
      N=N,        
      beta=params.base$beta.base*mean(input$beta.var)/100, # for triangular assume it is symmetric
      y0=params.base$y0.base*mean(input$y0.var)/100,
      value.timber=check.in(input$Timber,params.base$value.timber),
      value.recreation=check.in(input$Recreation,params.base$value.recreation),
      value.landscape=check.in(input$Landscape,params.base$value.landscape),
      value.biodiversity=check.in(input$Biodiversity,params.base$value.biodiversity),
      value.carbon=check.in(input$Carbon,params.base$value.carbon),
      contr=params.base$contr,
      control.cost=check.in(input$Cost,params.base$control.cost),
      control.varmodel=control.varmodel,
      control.prop=control.prop,
      Nrep=Nrep,
      d.rate=d.rate,
      effort=input$effort,
      beta.base=params.base$beta.base,
      beta.var1=params.base$beta.base*input$beta.var[1]/100,
      beta.var2=params.base$beta.base*input$beta.var[2]/100,
      y0.base=params.base$y0.base,
      y0.var1=params.base$y0.base*input$y0.var[1]/100,
      y0.var2=params.base$y0.base*input$y0.var[2]/100,
      time.start=input$time.control[1],
      time.stop=input$time.control[2],
      seed=params.base$seed
    ))
  }
  
  ## main simulation engine  ----
  DEoutput<-function(params){
    # solve equations
    # this is very badly coded
#    params<-get.params()
    seed.now<-seed.init(NULL)
    withProgress(message = 'Simulating:', detail=paste(params$Nrep,"reps with control..."),value = 0, {
    Nrep1<-1:params$Nrep             # replicates with control
    Nrep2<-params$Nrep+1:params$Nrep        # replicates without control
    set.seed(params$seed)
    S<-matrix(NA,nrow=length(times),ncol=2*params$Nrep)
    I<-matrix(NA,nrow=length(times),ncol=2*params$Nrep)
    set.seed(seed.now)
    for (i in Nrep1){
      params.run<-variability(params,contr.switch=1)
      yinit<-c(S=params$N-params.run$y0,I=params.run$y0)
      result<-ode(yinit,times,forest,params.run,method="bdf")
      S[,i]<-result[,2]
      I[,i]<-result[,3]
    }
    incProgress(1/2,detail=paste(params$Nrep,"reps w/o control..."))
    set.seed(seed.now)
    for (i in Nrep2){
      params.run<-variability(params,contr.switch=0)
      yinit<-c(S=params$N-params.run$y0,I=params.run$y0)
      result<-ode(yinit,times,forest,params.run,method="bdf")
      S[,i]<-result[,2]
      I[,i]<-result[,3]
    }
    incProgress(1/2)
    })
    return(list(S=S,I=I))
  }
  
  ## runtime; so that we can choose the type of the model - at the moment this simply calls DEoutput ----
  get.output<-reactive({
    params<-get.params()
    return(DEoutput(params))
  })

  ## inputs ----
  output$selectNsize <- renderUI({
    params.base<-get.params()
    numericInput("Nsize","Area (ha):",params.base$N,min=min(params.base$N,50),max=max(params.base$N,1000),step=50)
  })
  output$selectNrep <- renderUI({
    params.base<-baseParams()
    numericInput("Nrep","Number of replicates (large number means more reliable results but slower simulations):",params.base$Nrep,min=50,max=1000,step=50)
  })
  output$selecty0 <- renderUI({
    params.base<-get.params()
    numericInput("y0","Baseline initial infected area (ha):",params.base$y0,min=min(params.base$y0,1),max=max(params.base$y0,params.base$N),step=1)
  })
  output$selectcontr <- renderUI({
    params.base<-get.params()
    numericInput("contr","Control efficiency (infected area clear-felled by one person in one year, ha):",params.base$contr,min=min(params.base$contr,0.1),max=max(params.base$contr,10),step=0.1)
  })
  output$selectCost <- renderUI({
    params.base<-baseParams()
    numericInput("Cost","Cost per person-year (GBP):",params.base$control.costs,min=0,max=100,step=0.1)
  })
  output$selectd.rate <- renderUI({
    params.base<-baseParams()
    numericInput("d.rate","Annual discount rate:",params.base$d.rate,min=0,max=0.5,step=0.01)
  })
  output$selectTimber <- renderUI({
    params.base<-baseParams()
    numericInput("Timber","Timber (GBP per ha):",params.base$value.timber,min=50,max=10000,step=50)
  })
  output$selectRecreation <- renderUI({
    params.base<-baseParams()
    numericInput("Recreation","Recreation (GBP per ha per yr):",params.base$value.recreation,min=0,max=100,step=5)
  })
  output$selectLandscape <- renderUI({
    params.base<-baseParams()
    numericInput("Landscape","Landscape (GBP per ha per yr):",params.base$value.landscape,min=0,max=100,step=5)
  })
  output$selectBiodiversity <- renderUI({
    params.base<-baseParams()
    numericInput("Biodiversity","Biodiversity (GBP per ha per yr):",params.base$value.biodiversity,min=0,max=100,step=5)
  })
  output$selectCarbon <- renderUI({
    params.base<-baseParams()
    numericInput("Carbon","Carbon (GBP per ha per yr):",params.base$value.carbon,min=0,max=100,step=5)
  })
  output$selectBudget <- renderUI({
    params.base<-baseParams()
    numericInput("Budget","Available budget for control (m GBP):",0.2,min=0,max=1000,step=0.1)
  })
  
  
  ## outputs ----
  # traces ----
  output$tracePlot <- renderPlot({
    params<-get.params()
    Nrep1<-1:params$Nrep             # replicates with control
    out<-get.output()
    par(mfrow = c(2, 1), mar = c(3, 3.5, 1, 1), oma = c(1, 1,0, 0))
    par(tcl = -0.2, pty = "m", cex = 1, las = 1, cex.axis = 1, 
        cex.lab = 1, mgp = c(2.5, 0.5, 0))
    plot(times,out$I[,1]/params$N*100,type="n",ylim=c(0,100),xlab="",ylab="Infected area (%)")
    infected.mean<-apply(out$I[,Nrep1]/params$N*100,1,function(x) mean(x,na.rm=T))
    infected.sd<-apply(out$I[,Nrep1]/params$N*100,1,function(x) sd(x,na.rm=T))
    lines(times,infected.mean,col=1,lwd=1,type="b",pch=16,cex=0.8)
    bars.mine.sd(times,infected.mean,infected.sd)
    abline(h=0,lty=2,col=4)
    if (params$effort>0) abline(v=c(params$time.start,params$time.stop),lty=2,col=4)
    z<-damage(out$S[,Nrep1],out$I[,Nrep1],params,contr.switch=1,control.prop=params$control.prop)/1e6
    z.mean<-apply(z,1,function(x) mean(x,na.rm=T))
    z.sd<-apply(z,1,function(x) sd(x,na.rm=T))
    plot(times,z.mean,type="n",ylim=range(0,z),xlab="",ylab="Damage (m GBP)")
    lines(times,z.mean,col=1,lwd=1,type="b",pch=16,cex=0.8)
    bars.mine.sd(times,z.mean,z.sd)
    abline(h=0,lty=2,col=4)
    if (params$effort>0) abline(v=c(params$time.start,params$time.stop),lty=2,col=4)
    mtext(side=1,line=2,"Time (years)",cex=1)
  })
  output$tracePlotReport <- renderPlot({
    params<-get.params()
    #    Nrep0<-1:min(showReps,params$Nrep)
    Nrep0<-1:params$Nrep
    out<-get.output()
    par(mfrow = c(2, 1), mar = c(3, 3.5, 1, 1), oma = c(1, 1,0, 0))
    par(tcl = -0.2, pty = "m", cex = 1, las = 1, cex.axis = 1, 
        cex.lab = 1, mgp = c(2.5, 0.5, 0))
    plot(times,out$S[,1]/params$N,type="n",ylim=c(0,100),xlab="",ylab="Healthy/Infected/Treated area (%)")
    matlines(times,out$I[,Nrep0]/params$N*100,type="l",col=2,lty=1)
    matlines(times,out$S[,Nrep0]/params$N*100,type="l",col=3,lty=1)
    if (params$effort>0) matlines(times,(params$N-out$I[,Nrep0]-out$S[,Nrep0])/params$N*100,type="l",col=4,lty=1)
    abline(h=0,lty=2,col=4)
    if (params$effort>0) abline(v=c(params$time.start,params$time.stop),lty=2,col=4)
    z<-damage(out$S[,Nrep0],out$I[,Nrep0],params,contr.switch=1,control.prop=params$control.prop)/1e6
    matplot(times,z,ylim=range(0,z),type="l",xlab="",ylab="Damage (m GBP)",lty=1,col=1)
    abline(h=0,lty=2,col=4)
    if (params$effort>0) abline(v=c(params$time.start,params$time.stop),lty=2,col=4)
    mtext(side=1,line=2,"Time (years)",cex=1)
  })
  
  # distributions of parameters ----
  output$parsPlot <- renderPlot({
    params<-get.params()
    params.control<-variability(params,contr.switch=1,runs=1000)
    #    params.no<-variability(params,contr.switch=0,runs=500)
    par(mfrow = c(3, 1), mar = c(3, 3.5, 1, 1), oma = c(1, 1,0, 0))
    par(tcl = -0.2, pty = "m", cex = 1, las = 1, cex.axis = 1, 
        cex.lab = 1, mgp = c(2, 0.5, 0))
    z<-density.new(params.control$beta,adjust=1)
    plot(z$x,z$y/max(z$y),xlim=range(0,z$x),col=1,lwd=2,type="s",ylab="Density",xlab="rate of spread")
    z<-density.new(params.control$y0,adjust=0.5)
    plot(z$x,z$y/max(z$y),xlim=range(0,params.control$N,z$x),col=1,lwd=2,type="s",ylab="Density",xlab="initial load")
    z<-density.new(params.control$effort*params.control$contr,adjust=1)
    plot(z$x,z$y/max(z$y),xlim=range(0,max(z$x),0.5),col=1,lwd=2,type="s",ylab="Density",xlab="control rate")
  })
  
  # distribution of damage ----
  output$distPlot <- renderPlot({
    params<-get.params()
    Nrep1<-1:params$Nrep             # replicates with control
    Nrep2<-params$Nrep+1:params$Nrep        # replicates without control
    out<-get.output()
    par(mfrow = c(1, 1), mar = c(3.5, 3.5, 1, 1), oma = c(1, 1,0, 0))
    par(tcl = -0.2, pty = "m", cex = 1, las = 1, cex.axis = 1, 
        cex.lab = 1, mgp = c(2.5, 0.5, 0))
#    z<-density(damage(out$S[,Nrep1],out$I[,Nrep1],params,1,control.prop=params$control.prop)[time.end,]/1e6,adjust=1) # was 0.5
#    z0<-density(damage(out$S[,Nrep2],out$I[,Nrep2],params,0,control.prop=params$control.prop)[time.end,]/1000,adjust=1)
#    plot(z$x,z$y/max(z$y),xlim=range(z$x,z0$x),type="s",main="",col=3,lwd=3,xlab="Damages (m GBP)",ylab="Probability (scaled)")
#    lines(z0$x,z0$y/max(z0$y),type="s",col=2,lwd=3)
#    z<-density.new(damage(out$S[,Nrep1],out$I[,Nrep1],params,1,control.prop=params$control.prop)[time.end,]/1e6)
#    z0<-density.new(damage(out$S[,Nrep2],out$I[,Nrep2],params,1,control.prop=params$control.prop)[time.end,]/1e6)
#    plot(z0$x,z0$y,xlim=range(z$x,z0$x),ylim=range(z$y,z0$y),type="s",main="",col=3,lwd=3,xlab="Damages (m GBP)",ylab="Probability")
#    if (params$effort>0.1) lines(z$x,z$y,type="s",col=2,lwd=3)
    z<-damage(out$S[,Nrep1],out$I[,Nrep1],params,1,control.prop=params$control.prop)[time.end,]/1e6
    z0<-damage(out$S[,Nrep2],out$I[,Nrep2],params,0,control.prop=params$control.prop)[time.end,]/1e6
    zd<-density.new(z0-z)
    plot(zd$x,zd$y,xlim=range(0,-1,1,zd$x,0.1*abs(zd$x)),ylim=c(0,1.2*max(zd$y)),type="s",main="",col=1,lwd=3,xlab="Avoided losses (m GBP)",ylab="Probability")
    abline(v=0,lty=2,col=4)
    abline(h=0,lty=2,col=4)
    if (mean(zd$x)<0.2*max(abs(zd$x))) text("Costs>benefits:\nno action",x=0.6*min(0,-1,1,zd$x,0.1*abs(zd$x)),y=1.15*max(zd$y),cex=1)
    if (mean(zd$x)>-0.2*max(abs(zd$x))) text("Costs<benefits:\naction",x=0.6*max(0,-1,1,zd$x,0.1*abs(zd$x)),y=1.15*max(zd$y),cex=1)
  })
  
  # version for report ----
  output$distPlot.report <- renderPlot({
    params<-get.params()
    Nrep1<-1:params$Nrep             # replicates with control
    Nrep2<-params$Nrep+1:params$Nrep        # replicates without control
    out<-get.output()
    par(mfrow = c(1, 1), mar = c(3.5, 3.5, 1, 1), oma = c(1, 1,0, 0))
    par(tcl = -0.2, pty = "m", cex = 1, las = 1, cex.axis = 1, 
        cex.lab = 1, mgp = c(2.7, 0.5, 0))
    z<-density(damage(out$S[,Nrep1],out$I[,Nrep1],params,1,control.prop=params$control.prop)[time.end,]/1e6,adjust=1) # was 0.5
    z0<-density(damage(out$S[,Nrep2],out$I[,Nrep2],params,0,control.prop=params$control.prop)[time.end,]/1e6,adjust=1)
    #    plot(z$x,z$y/max(z$y),xlim=range(z$x,z0$x),type="s",main="",col=3,lwd=3,xlab="Damages (m GBP)",ylab="Probability (scaled)")
    #    lines(z0$x,z0$y/max(z0$y),type="s",col=2,lwd=3)
    z<-density.new(damage(out$S[,Nrep1],out$I[,Nrep1],params,1,control.prop=params$control.prop)[time.end,]/1e6)
    z0<-density.new(damage(out$S[,Nrep2],out$I[,Nrep2],params,0,control.prop=params$control.prop)[time.end,]/1e6)
    plot(z0$x,z0$y,xlim=range(z$x,z0$x),ylim=range(z$y,z0$y),type="s",main="",col=3,lwd=3,xlab="Damages (m GBP)",ylab="Probability")
    if (params$effort>0.1) lines(z$x,z$y,type="s",col=2,lwd=3)
    abline(h=0,lty=2,col=4)
  })
  
  # table ----
  output$table <- renderTable({
    params<-get.params()
    Nrep1<-1:params$Nrep             # replicates with control
    Nrep2<-params$Nrep+1:params$Nrep        # replicates without control
    out<-get.output()
    x<-mean(out$S[time.end,Nrep1],na.rm=T)
    y<-mean(out$I[time.end,Nrep1],na.rm=T)
    r<-mean(params$N-out$I[time.end,Nrep1]-out$S[time.end,Nrep1],na.rm=T)
    y0<-mean(out$I[time.end,Nrep2],na.rm=T)
    z<-mean(damage(out$S[,Nrep1],out$I[,Nrep1],params,1,control.prop=params$control.prop)[time.end,])
    z0<-mean(damage(out$S[,Nrep2],out$I[,Nrep2],params,0,control.prop=params$control.prop)[time.end,])
    z.costs<-costs(out$S[,Nrep1],out$I[,Nrep1],params,1,control.prop=params$control.prop)/1e6
    z.costs.pa<-apply(z.costs,1,max)
    x<-rbind(      
      c("Healthy area (%):",round(100*x/params$N,0)),
      c("Infected area (%):",round(100*y/params$N,0)),
      c("Treated area (%):",round(100*r/params$N,0)),
      c("Peak control cost per year (m GBP):",round(max(z.costs.pa),3)),
# This is a quick fudge      
      c("Total budget per year (m GBP):",ifelse(is.null(input$Budget),0.2,input$Budget))
    )
    colnames(x)<-c("Indicator","Value")
    return(x)
  },bordered=TRUE)
  
  # table for report ----
  output$table.report <- renderTable({
    params<-get.params()
    Nrep1<-1:params$Nrep             # replicates with control
    Nrep2<-params$Nrep+1:params$Nrep        # replicates without control
    out<-get.output()
    x<-mean(out$S[time.end,Nrep1],na.rm=T)
    y<-mean(out$I[time.end,Nrep1],na.rm=T)
    r<-mean(params$N-out$I[time.end,Nrep1]-out$S[time.end,Nrep1],na.rm=T)
    y0<-mean(out$I[time.end,Nrep2],na.rm=T)
    z<-mean(damage(out$S[,Nrep1],out$I[,Nrep1],params,1,control.prop=params$control.prop)[time.end,])
    z0<-mean(damage(out$S[,Nrep2],out$I[,Nrep2],params,0,control.prop=params$control.prop)[time.end,])
    z.costs<-costs(out$S[,Nrep1],out$I[,Nrep1],params,1,control.prop=params$control.prop)/1e6
    z.costs.pa<-apply(z.costs,1,max)
    x<-rbind(      
      c("Healthy area (%):",round(100*x/params$N,0)),
      c("Infected area (%):",round(100*y/params$N,0)),
      c("Treated area (%):",round(100*r/params$N,0)),
      c("Peak control cost per year (m GBP):",round(max(z.costs.pa),3)),
      # This is a quick fudge      
      c("Total budget per year (m GBP):",ifelse(is.null(input$budget),0.2,input$budget))
    )
    colnames(x)<-c("Indicator","Value")
    return(x)
  },bordered=TRUE)
  
  # table showing (some) parameters ----
  output$parametersTable <- renderTable({
    params<-get.params()
    x<-rbind(
      c("Total area:",round(params$N,2)),
      c("Baseline rate of spread:",round(params$beta.base,2)),
      c("Baseline initial infected population:",round(params$y0,2)),
      c("Cost of control:",round(params$control.cost,5))
    )
    colnames(x)<-c("Parameter","Value")
    return(x)
  },bordered=TRUE)
  
  # same for report ----
  output$parametersTable.report <- renderTable({
    params<-get.params()
    x<-rbind(
      c("Total area:",round(params$N,2)),
      c("Baseline rate of spread:",round(params$beta.base,2)),
      c("Baseline initial infected population:",round(params$y0,2)),
      c("Cost of control:",round(params$control.cost,5))
    )
    colnames(x)<-c("Parameter","Value")
    return(x)
  },bordered=TRUE)
  
  # show scenarios for upload tab ----
  output$currentscenarioTable.scenarios <- renderTable({
    params<-get.params()
      x<-data.frame("ID"=1,
                    "Pest"="Generic",
                    "Host"="Generic",
                    "UK"="Present",
                    "N"=params$N,
                    "beta"=params$beta,
                    "y0"=params$y0,
                    "value.timber"=params$value.timber,
                    "value.recreation"=params$value.recreation,
                    "value.landscape"=params$value.landscape,
                    "value.biodiversity"=params$value.biodiversity,
                    "value.carbon"=params$value.carbon,
                    "contr"=params$contr,
                    "control.cost"=params$control.cost,
                    "control.varmodel"=params$control.varmodel,
                    "control.prop"=params$control.prop,
                    "Nrep"=params$Nrep,
                    "d.rate"=params$d.rate,
                    "effort"=params$effort,
                    "beta.base"=params$beta.base,
                    "beta.var1"=params$beta.var1,
                    "beta.var2"=params$beta.var2,
                    "y0.base"=params$y0.base,
                    "y0.var1"=params$y0.var1,
                    "y0.var2"=params$y0.var2,
                    "time.start"=params$time.start,
                    "time.stop"=params$time.stop,
                    "seed"=params$seed
      )[5:28]
      colnames(x)<-scenario.headings[5:28]
      return(x)
  },bordered=TRUE)
  output$currentscenarioTable.upload <- renderTable({
    params<-get.params()
    x<-data.frame("ID"=1,
                  "Pest"="Generic",
                  "Host"="Generic",
                  "UK"="Present",
                  "N"=params$N,
                  "beta"=params$beta,
                  "y0"=params$y0,
                  "value.timber"=params$value.timber,
                  "value.recreation"=params$value.recreation,
                  "value.landscape"=params$value.landscape,
                  "value.biodiversity"=params$value.biodiversity,
                  "value.carbon"=params$value.carbon,
                  "contr"=params$contr,
                  "control.cost"=params$control.cost,
                  "control.varmodel"=params$control.varmodel,
                  "control.prop"=params$control.prop,
                  "Nrep"=params$Nrep,
                  "d.rate"=params$d.rate,
                  "effort"=params$effort,
                  "beta.base"=params$beta.base,
                  "beta.var1"=params$beta.var1,
                  "beta.var2"=params$beta.var2,
                  "y0.base"=params$y0.base,
                  "y0.var1"=params$y0.var1,
                  "y0.var2"=params$y0.var2,
                  "time.start"=params$time.start,
                  "time.stop"=params$time.stop,
                  "seed"=params$seed
    )[5:28]
    colnames(x)<-scenario.headings[5:28]
    return(x)
  },bordered=TRUE)
  
    output$scenarioTable.upload <- renderTable({
    params<-get.params()
    pars.in<-parsInput()[5:28]
    if (is.null(pars.in)) 
      return() 
    else {
      x<-pars.in
      colnames(x)<-scenario.headings[5:28]
      return(x)
    }
  },bordered=TRUE)
  
  # scenarios for scenarios tab ----
  output$scenarioTable.scenarios <- renderTable({
    params<-get.params()
    pars.in<-parsInput()[2:28]
    if (is.null(pars.in)) 
      return() 
    else {
      x<-pars.in
      colnames(x)<-scenario.headings[2:28]
      return(x)
    }
  },bordered=TRUE)
  
  ## helpers to output data ----
  # dump simulation results ----
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$dataset, '.csv', sep='') },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )
  datasetInput <- reactive({
    params<-get.params()
    Nrep1<-1:params$Nrep             # replicates with control
    Nrep2<-params$Nrep+1:params$Nrep        # replicates without control
    out<-get.output()
    switch(input$dataset,
           "Healthy" = out$S,
           "Infected" = out$I)
  })
  
  # dump parameters/scenarios ----
  output$downloadPars <- downloadHandler(
    filename = function() { paste("State", '.csv', sep='') },
    content = function(file) {
      write.csv(paramsInput(), file)
    }
  )
  paramsInput <- reactive({
    params<-get.params()
    return(rbind(as.vector(scenario.headings)[2:28],c(selectParamsNames(),params)))
  })
  
  ## management of scenarios ----
  # upload scenarios ----
  parsInput <- reactive({
    inFile <- input$uploadPars
    if (is.null(inFile)){
      uploadPars<-NULL
    } else {
      uploadPars<-read.csv(inFile$datapath,header=F,skip=3,
                             col.names=c(
                               "ID","Pest","Host","UK",
                               "N","beta","y0","value.timber","value.recreation","value.landscape","value.biodiversity","value.carbon","contr","control.cost","control.varmodel","control.prop","Nrep","d.rate","effort",
                               "beta.base","beta.var1","beta.var2","y0.base","y0.var1","y0.var2","time.start","time.stop","seed"
                            )
                  )
    }
    return(rbind.data.frame(defaultScenario(), uploadPars))
  })
  

  # select scenarios ----
  output$selectScenario <- renderUI({
    scores.in<-parsInput()
    scores.choices<-setNames(1:nrow(scores.in),scores.in[,2])
    selectInput("scenarios", 
                label = h5("Select scenario:"), 
                choices = scores.choices,
                selected = 1)
  })
  
  # populate parameters ----
  # this really should be done better, based on column names not on hard wired column numbers
  selectParams <- reactive({
    scores.in<-parsInput()
    if (is.null(input$scenarios)) 
      scores.base<-as.numeric(scores.in[1,5:28])
    else
      scores.base<-as.numeric(scores.in[input$scenarios,5:28])
    return(scores.base)
  })
  
  # populate the rest of the scenarios to match Plant Health Risk Register format ----
  selectParamsNames <- reactive({
    scores.in<-parsInput()
    if (is.null(input$scenarios)) 
      scores.base<-scores.in[1,2:4]
    else
      scores.base<-scores.in[input$scenarios,2:4]
    return(scores.base)
  })
  
  # helper for selecting scenarios ----
  output$test.scenarios <- renderText({
    if (is.null(input$scenarios)) c(1,selectParams())
    else c(paste(input$scenarios),selectParams())
  })
  
}


## User interface part - this is where the output is constructed ----
ui <- navbarPage("FC Tool",id="select.tab",
           theme=shinytheme("superhero"),
           #           shinythemes::themeSelector(),
## welcome tab ----
           tabPanel("Welcome",
                    helpText(h4("Pest and disease decision support tool")),
                    helpText("Created by Adam Kleczkowski and Morag Macpherson"),
                    helpText("Project led by Glyn Jones, FERA"),
                    helpText("Collaboration with Julia Touza and Piran White, York, and Stephen Parnell, Salford"),
                    hr(),
                    helpText("This tool is designed to simulate spread of a pest or disease through a population of trees; control options can be implemented and the values at risk calculated."),
                    helpText("Use Dashboard to run the simulations or other tabs to manipulate parameters.")
           ),
## dashboard - where most action takes place ----
           tabPanel("Dashboard",
                    sidebarLayout(
                      sidebarPanel(
                        fluidPage(
                          helpText(h4("Choose the parameters:")),
                          helpText(h6("(please use the sliders to select lower and upper boundary; separate the sliders if necessary)")),
                          fluidRow(
                            sliderInput("beta.var",
                                        h5("Select uncertainty range around baseline (100%) rate of spread:"),
                                        min = 0,
                                        max = 500,
                                        step=5,
                                        value = c(100,100))
                          ),
                          fluidRow(
                            sliderInput("y0.var",
                                        h5("Select uncertainty range around baseline (100%) initial area of infection"),
                                        min = 0,
                                        max = 500,
                                        step=5,
                                        value = c(100,100))
                          ),
                          tags$hr(),
                          fluidRow(
                            sliderInput("time.control",
                                        h5("Select period in which control is applied:"),
                                        min = 0,
                                        max = 25,
                                        step=1,
                                        value = c(0,5))
                          ),
                          fluidRow(
                            sliderInput("effort",
                                        h5("Select control effort (number of personnel per year per ha infected) [variability not currently implemented]:"),
                                        min = 0,
                                        max = 10,
                                        step=0.5,
                                        value = 0.0)
                          ),
                          submitButton("Update")
                        ) # fluid page
                      ), # sidebar panel
                      # Show a plot of the generated distribution
                      mainPanel(
                        fluidPage(
                          fluidRow(
                            #                          helpText(h4("Predictions:")),
                            column(6,
                                   h5("Distribution of avoided damages (accumulated over 25 years)"),
#                                   plotOutput("distPlot",width="275px",height="200px")
                                   plotOutput("distPlot",width="350px",height="260px")
                            ),
                            column(6,
                                   h5("Proportion of area of healthy and infected trees in year 25:"),
                                   h4(tableOutput("table"))
                            )
                          ),
                          fluidRow(
                            column(6,
                                   helpText(h5("Simulation trace:\n(points=average, bars=+/- st. dev.)")),
                                   plotOutput("tracePlot",width="350px",height="350px")
                            ),
                            column(6,
                                   h5("Current list of key parameters:"),
                                   h3(tableOutput("parametersTable"))
                            )
                          ) # fluid row
                          ) # fluid page
                    ) # main panel
                    )
           ), # Dashboard
## settings menu ----
           navbarMenu("Settings",
                      tabPanel("Parameters",
                               fluidPage(
                                 helpText(h4("General parameters")),
                                 fluidRow(
                                   column(6,
                                          uiOutput("selectNsize"),
                                          uiOutput("selectCost"),
                                          uiOutput("selectd.rate"),
                                          uiOutput("selectBudget")
                                   ),
                                   column(6,
#                                          selectInput("control.varmodel", 
#                                                      label = h5("Select model for control variability:"), 
#                                                      choices = c("No variability"=0,"Exponential distribution"=1),
#                                                      selected = 0)
                                          uiOutput("selecty0"),
                                          uiOutput("selectcontr"),
                                          uiOutput("selectNrep")
                                   )
                                 ), # fluidRow
                        tags$hr(),
                        helpText(h4("Values at risk")),
                        fluidRow(
                          column(6,
                                 helpText(h5("Specify the value of timber per ha. This will only be included once per crop (i.e. if trees were harvested at a given time)"))
                          ),
                          column(6,
                                uiOutput("selectTimber")
                          )
                        ),
                        fluidRow(
                          column(6,
                                 helpText(h5("Specify the value of recreation:"))
                          ),
                          column(6,
                                 uiOutput("selectRecreation")
                          )
                        ),
                        fluidRow(
                          column(6,
                                 helpText(h5("Specify the value of landscape:"))
                          ),
                          column(6,
                                 uiOutput("selectLandscape")
                          )
                        ),
                        fluidRow(
                          column(6,
                                 helpText(h5("Specify the value of biodiversity:"))
                          ),
                          column(6,
                                 uiOutput("selectBiodiversity")
                          )
                        ),
                        fluidRow(
                          column(6,
                                 helpText(h5("Specify the value of carbon:"))
                          ),
                          column(6,
                                 uiOutput("selectCarbon")
                          )
                        ),
                                tags$hr(),
                                 submitButton("Update")
                        )
                      ),
                      tabPanel("Initial prevalence when found",
                               helpText(h4("Set initial prevalence using rule of thumb")),
                               helpText(h5("Currently implemented in General parameters tab")),
                               tags$hr(),
                               submitButton("Update")
                      ),
                      tabPanel("Scenarios",
                               helpText(h4("Setting scenarios")),
                               tags$hr(),
                               h5("Current parameters:"),
                               h4(tableOutput("currentscenarioTable.scenarios")),
                               tags$hr(),
                               fluidRow(column(6,uiOutput("selectScenario")),column(6,h5("and press:"),submitButton("Update"))),
                               tags$hr(),
                               h5("Available scenarios:"),
                               h4(tableOutput("scenarioTable.scenarios")),
                               tags$hr(),
                               h5("Upload saved scenarios"),
                               fileInput("uploadPars", "Choose CSV File",
                                         accept = c(
                                           "text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv")
                               ),
                               tags$hr(),
                               h5("Download current scenario:"),                              
                               downloadButton('downloadPars', 'Download scenario'),
                               tags$hr(),
                               p("Sample spreadsheet (examples.csv) can be found",a(href="https://app.box.com/s/4ygp06xyujks2b5ye548xdn12v9xmjap","here.",target="_blank")," Please download it to your computer and then use the above menu to upload it to the FC Tool")
                      )
           ),
## input and output menu ----
           navbarMenu("Input and output",
                      tabPanel("Download results",
                               h5("Choose a dataset (press Update after selection and only then press Download):"),
                               fluidRow(
                                 column(4,selectInput("dataset", NULL, 
                                           choices = c("Healthy", "Infected"))),
                               column(3,submitButton("Update choice"))),
                               downloadButton('downloadData', 'Download data')
                      ),
                      tabPanel("Generate report",
                               fluidPage(
                                 h3("Simulation report"),
                                 tags$hr(),
                                 fluidRow(
                                   column(6,
                                          h5("Distribution of combined damage:\ngreen=do nothing, red=with control if applicable"),
                                          plotOutput("distPlot.report",width="350px",height="255px")
                                   ),
                                   column(6,
                                          h5("Proportion of area of healthy and infected trees in year 25:"),
                                          h4(tableOutput("table.report"))
                                   )
                                 ),
                                 fluidRow(
                                   column(6,
                                          helpText(h5("Simulation trace (all replicates):")),
                                          #            plotOutput("tracePlot",width="time.end5px",height="350px")
                                          plotOutput("tracePlotReport",width="350px",height="350px"),
                                          helpText(h5("(green=healthy, red=infected, blue=clear felled)"))
                                   )
                                 ),
                                 tags$hr(),
                                 fluidRow(
                                   column(6,
                                          h5("Current list of key parameters:"),
                                          h3(tableOutput("parametersTable.report"))
                                   ),
                                   column(6,
                                          helpText(h5("Parameter distributions")),
                                          plotOutput("parsPlot",width="275px",height="350px")
                                   )
                                 )
                              ) # fluid page
           )
           ),
## help menu ----
           tabPanel("Help",
                    fluidPage(
                      tags$hr(),
                      helpText(h5("Warning: This is a development version. Only limited checking of variables and parameters is performed, hence the tool can break down or produce unreliable results if parameters are entered outside the realistic range.")),
                      tags$hr(),
                      helpText(h5("Version history:")),
                      helpText("Version 1.1: single zone; eradication control; control cost for whole area; value proportional to healthy area."),
                      helpText("Version 1.2: improved GUI and additional inputs/outputs; linked to PHRR"),
                      helpText("Version 1.3: changes to menu; system of equations rather than a single one; discounted economics"),
                      helpText("Version 2.0: moved to a single file; use triangular distributions; changed to years; control only the first 5 years; profit changed to damage; changed from scenarios to parameters"),
                      helpText("Version 2.1: damage function now includes two classes of values; expanded values input; improved front end and saving and loading scenarios"),
                      helpText("Version 3.0: June 2017 changes for SG meeting")
                    )
           )
)

## required for running ----
shinyApp(ui = ui, server = server, options = list(launch.browser=T))
