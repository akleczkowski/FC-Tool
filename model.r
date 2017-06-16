## model ----

# time output is in years; time span is hard coded for 5 years for control followed by 20 years
#time.stop<-5 # now moved to params (not yet implemeneted in data dump)
time.end<-25
times<-seq(0,time.end,by=1)

# differential equation(s)
# at the moment it is a simple SIR model with harvesting; there is no threshold
forest<-function(t,y,params){
  with(as.list(c(params, y)), {
    # a fudge to mimic extinction of disease
    Ivar<-ifelse(I<1/N,1e-10,I)
    # RHS of equations come here
    dS<- -beta*S*Ivar/N
    dI<-beta*S*Ivar/N-effort*contr*Ivar*ifelse((t>=time.start) & (t<=time.stop),1,0)
    doty<-c(dS,dI)
    return(list(doty))
  })
}

# profit functions
damage<-function(S,I,params,contr.switch=1,control.prop=0){
  with(as.list(c(params)), {
    # profit function
    # assumes that S and I are vectors over time, not single snapshots
    # losses are of two types: single snapshot at time (timber) or accumulating over time (recreation, landscape, biodiversity, carbon)
    # all are assumed to be linear with number of infected
    value.lost.timber<-value.timber*(N-S)
    value.lost.recreation<-value.recreation*(N-S)
    value.lost.landscape<-value.landscape*(N-S)
    value.lost.biodiversity<-value.biodiversity*(N-S)
    value.lost.carbon<-value.carbon*(N-S)
    value.lost<-value.lost.timber*exp(-d.rate*times)+apply((value.lost.recreation+value.lost.landscape+value.lost.biodiversity+value.lost.carbon)*exp(-d.rate*times),2,cumsum)
    # value is current but discounted
    # control is discounted and then integrated 
    # two possibilities selected by control.prop:
    # is control proportional to the number of infected trees, or to all trees?
    if (control.prop==0){
#      damage.run<-value.lost+contr.switch*cumsum(exp(-d.rate*times)*effort*control.cost*ifelse(times<=time.stop,1,0))
      damage.run<-value.lost+contr.switch*apply(exp(-d.rate*times)*effort*control.cost*ifelse(times<=time.stop,1,0),2,cumsum)
    } else {
#      damage.run<-value.lost+contr.switch*cumsum(exp(-d.rate*times)*effort*control.cost*ifelse(times<=time.stop,1,0)*I)
      damage.run<-value.lost+contr.switch*apply(exp(-d.rate*times)*effort*control.cost*ifelse(times<=time.stop,1,0)*I,2,cumsum)
    }
    return(damage.run)
  })
}
## this needs to be done better ----
# as now costs arer enetered twice
costs<-function(S,I,params,contr.switch=1,control.prop=0){
  with(as.list(c(params)), {
    if (control.prop==0){
      #      damage.run<-value.lost+contr.switch*cumsum(exp(-d.rate*times)*effort*control.cost*ifelse(times<=time.stop,1,0))
      costs.run<-contr.switch*effort*control.cost*ifelse(times<=time.stop,1,0)
    } else {
      #      damage.run<-value.lost+contr.switch*cumsum(exp(-d.rate*times)*effort*control.cost*ifelse(times<=time.stop,1,0)*I)
       costs.run<-contr.switch*effort*control.cost*ifelse(times<=time.stop,1,0)*I
    }
    return(costs.run)
  })
}

# model variability: select parameters to switch
variability<-function(params,contr.switch=1,runs=1){
  params.run<-params # load other parameters which do not change
  # rate of spread ~ triangular
  params.run$beta<-rtriangle(runs,a=params$beta.var1,b=params$beta.var2,c=params$beta)
  # initial load ~ triangular
  params.run$y0<-rtriangle(runs,a=params$y0.var1,b=params$y0.var2,c=params$y0)
  # control effectiveness - fixed or ~ triangular - not implemented
  if (params$control.varmodel==0){
    params.run$contr<-rep(ifelse(contr.switch==0,0,params$contr),runs)
  } else {
    if (contr.switch==0){
      params.run$contr<-rep(0,runs)
    } else {
      params.run$contr<-rexp(runs,1/params$contr)
    }
  }
  return(params.run)
}

# default set of parameters
defaultScenario<-function(){
  return(data.frame("ID"=1,
                    "Pest"="Generic",
                    "Host"="Generic",
                    "UK"="Present",
                    "N"=100000,
                    "beta"=0.5,
                    "y0"=10000,
                    "value.timber"=171,
                    "value.recreation"=445,
                    "value.landscape"=160,
                    "value.biodiversity"=470,
                    "value.carbon"=246,
                    "contr"=0.6,
                    "control.cost"=30000,
                    "control.varmodel"=0,
                    "control.prop"=1,
                    "Nrep"=100,
                    "d.rate"=0.035,
                    "effort"=0,
                    "beta.base"=0.5,
                    "beta.var1"=0.375,
                    "beta.var2"=0.625,
                    "y0.base"=10,
                    "y0.var1"=7.5,
                    "y0.var2"=12.5,
                    "time.start"=0,
                    "time.stop"=5,
                    "seed"=1500
  ))
}

scenario.headings<-c("ID","Name of pest","Type of host","Present or not",
                     "Total area","Rate of spread (per year)","Initial infected area (ha)","Value of timber (GBP)","Recreational value (GBP)","Landscape value (GBP)",
                     "Biodiversity value (GBP)","Carbon value (GBP)","Control effectiveness","Control cost (person-year)","TBA",
                     "TBA","Number of replicates","Annual discount rate","Current effort",
                     "Current avg rate","Current low rate","Current high rate","Current avg inf. area",
                     "Current low inf. area","Current high inf. area","Start of treatment (yrs)","End of treatment (yrs)","TBA")

