## model ----

# time is in weeks and timespan is a year
times<-seq(0,52,by=1)

# differential equation(s)
# at the moment it is a simple logistic equation with control
forest<-function(t,y,params){
  with(as.list(c(params, y)), {
    # RHS of equations come here
    dS<- -beta*S*I/N
    dI<-beta*S*I/N-effort*contr*I
    doty<-c(dS,dI)
    return(list(doty))
  })
}

# profit functions
profit<-function(S,I,params,contr.switch=1,control.prop=0){
  with(as.list(c(params)), {
    # profit function here
    # two possibilities selected by control.prop:
    # is control proportional to the number of infected trees, or to all trees?
  if (control.prop==0){
    profit.run<-value*S-contr.switch*cumsum(exp(-d.rate*times/52)*effort*control.cost)
  } else {
    profit.run<-value*S-contr.switch*cumsum(exp(-d.rate*times/52)*effort*control.cost*I/N)
  }
  return(profit.run)
  })
}

# model variability: select parameters to switch
variability<-function(params,contr.switch=1,runs=1){
  params.run<-params # load other parameters which do not change
# rate of spread
  if (params$beta.var==0){
    params.run$beta<-rep(params$beta,runs)
  } else {
    params.run$beta<-exp(rnorm(runs,log(params$beta),params$beta.var*params$beta))
  }
# initial load
  if (params$y0.var==0){
    params.run$y0<-rep(params$y0,runs)
  } else {
    params.run$y0<-exp(rnorm(runs,log(params$y0),params$y0.var))
  }
# control effectiveness
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

# mapping scenarios onto parameters
# in score.map: likelihood, impact, value
scores.map <- matrix(c(
  rep(1:5,25),
  rep(rep(1:5,rep(5,5)),5),
  rep(1:5,rep(25,5)),
#  rep(c(0.1,0.2,0.3,0.5,0.8),25), # beta
#  rep(1,125), # y0
  rep(rep(c(0.1,0.2,0.3,0.5,0.8),rep(5,5)),5), # beta
  rep(c(1,2,5,10,20),25), # y0
  rep(1,125)/1500, # control effectiveness
  5000*rep(1:5,rep(25,5)) # value
),byrow=F,ncol=7)


