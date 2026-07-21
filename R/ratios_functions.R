# functions for adhoc
Frec.over.Fbar = function(stk.obj){
  # print(stk.obj@name)
  min.age_Frec = as.numeric(rownames(rec(stk.obj)))
  max.age_Fbar = dims(stk.obj@harvest)$max
  #max.age_Frec = stk.obj@range["minfbar"]-1
  #min.age_Frec = dims(stk.obj@harvest)$min
  F_rec = apply(stk.obj@harvest[ac(min.age_Frec),],2,mean)
  F_bar = fbar(stk.obj)
  ind = F_rec/F_bar
  attributes(ind)$name = stk.obj@name
  attributes(ind)$indicator = "Frec/Fbar"
  return(ind)
} 

Fjuv.over.Fbar = function(stk.obj){
  # print(stk.obj@name)
  if(length(range(stk.obj)['minyear']:range(stk.obj)['maxyear']) > 20){
    yearRange = c((an(range(stk.obj)['maxyear'])-20):an(range(stk.obj)['maxyear']))
  }
  else{
    yearRange = range(stk.obj)['minyear']:range(stk.obj)['maxyear']
  }
  mean_juv <- yearMeans(mat(stk.obj[,ac(yearRange)])) # check which are the juvenile age classes in the last 20 years (i.e. maturity<50%) - for NS cod it's 1 and 2
  juv.age <- which(mean_juv < 0.5)
  max.age_Fbar = dims(stk.obj@harvest)$max
  #max.age_Frec = stk.obj@range["minfbar"]-1
  #min.age_Frec = dims(stk.obj@harvest)$min
  F_juv = apply(stk.obj@harvest[an(juv.age),],2,mean)
  F_bar = fbar(stk.obj)
  ind = F_juv/F_bar
  attributes(ind)$name = stk.obj@name
  attributes(ind)$indicator = "Fjuv/Fbar"
  return(ind)
} 

# this function should be like this: quantMeans(harvest(stk)[mat(stk) < 0.5])%/%apply(harvest(stk),2,max)
# I don't remember why it had to be so complicated
Fjuv.over.Fmax = function(stk.obj){
  # print(stk.obj@name)
  if(length(range(stk.obj)['minyear']:range(stk.obj)['maxyear']) > 20){
    yearRange = c((an(range(stk.obj)['maxyear'])-20):an(range(stk.obj)['maxyear']))
  }
  else{
    yearRange = range(stk.obj)['minyear']:range(stk.obj)['maxyear']
  }
  mean_juv <- yearMeans(mat(stk.obj[,ac(yearRange)])) # check which are the juvenile age classes in the last 20 years (i.e. maturity<50%) - for NS cod it's 1 and 2
  juv.age <- which(mean_juv < 0.5)
  max.age_Fbar = dims(stk.obj@harvest)$max
  F_juv = apply(stk.obj@harvest[an(juv.age),],2,mean)
  F_max = apply(harvest(stk.obj),2,max)
  ind = F_juv/F_max
  attributes(ind)$name = stk.obj@name
  attributes(ind)$indicator = "Fjuv/Fmax"
  return(ind)
}

Fjuv.over.Fadult = function(stk.obj){
  # print(stk.obj@name)
  # apply(mat(stk),2:6,function(x){x<.5}) # put an option for by age juv
  if(length(range(stk.obj)['minyear']:range(stk.obj)['maxyear']) > 20){
    yearRange = c((an(range(stk.obj)['maxyear'])-20):an(range(stk.obj)['maxyear']))
  }
  else{
    yearRange = range(stk.obj)['minyear']:range(stk.obj)['maxyear']
  }
  mean_juv <- yearMeans(mat(stk.obj[,ac(yearRange)])) # check which are the juvenile age classes in the last 20 years (i.e. maturity<50%) - for NS cod it's 1 and 2
  juv.age <- which(mean_juv < 0.5)
  adult.age <- which(mean_juv >= 0.5)
  F_juv = apply(stk.obj@harvest[an(juv.age),],2,mean)
  F_adult = apply(stk.obj@harvest[an(adult.age),],2,mean)
  ind = F_juv/F_adult
  attributes(ind)$name = stk.obj@name
  attributes(ind)$indicator = "Fjuv/F_adult"
  return(ind)
}

a50.by.year <- function(stk){
  stk_by_year_l <- list()
  for (i in an(range(stk)['minyear']):an(range(stk)['maxyear'])){
    stk_by_year_l[[paste0(i)]] <- window(stk,
                                         start = i,
                                         end = i)
  }
  myfun <- function(stk){
    tmp <- fitselex(selage(stk, nyears = 1))
    return(tmp$par[1])
  }
  x<- lapply(stk_by_year_l,myfun)
  flq <- FLQuant(as.numeric(x), 
                 dimnames=list(age="all", 
                               year=an(range(stk)['minyear']):an(range(stk)['maxyear'])), 
                 units="")
  return(flq)
}

a50.by.year2 <- function(stk) {
  years <- an(range(stk)['minyear']):an(range(stk)['maxyear'])
  
  a50_values <- sapply(years, function(y) {
    stk_y <- window(stk, start = y, end = y)
    fit <- fitselexTMB(selage(stk_y, nyears = 1))
    fit$par[1]
  })
  
  FLQuant(as.numeric(a50_values), 
          dimnames = list(age = "all", year = years), 
          units = "")
}

# Maximum yield under current F reference point
threshold_fun2 <- function(sel_scen, brps, stk) {
  Fsel_table <- Fselex(brps, what = 'Fref')
  msy_point = Fsel_table$sel[which(Fsel_table$rel.yield == 1)]
  if(msy_point == 'obs'){
    opt_sel <- selage(stk)
    juv.age <- which(yearMeans(mat(stk)) < 0.5)
    thres <- mean(opt_sel[an(juv.age)])/max(opt_sel)
    return(thres)
  }
  else if (length(msy_point)>1 || length(msy_point) == 0) {
    return(NA)
  }
  else {
    opt_sel <- sel_scen[[msy_point]]
    
    stk <- as(brps[[1]],"FLStock")
    juv.age <- which(yearMeans(mat(stk)) < 0.5)
    thres <- mean(opt_sel[an(juv.age)])/max(opt_sel)
    return(thres)
  }
  
}

# optimal selection curve
optimal_sel <- function(sel_scen, brps){
  Fsel_table <- Fselex(brps, what = 'Fref')
  msy_point = Fsel_table$sel[which(Fsel_table$rel.yield == 1)]
  if(length(msy_point)>1 || length(msy_point) == 0){
    return(NA)
  }
  else if(msy_point == 'obs'){
    Sa=brps[[1]]@landings.sel/max(brps[[1]]@landings.sel)
    S50obs = s50(Sa)
    opt_sel <- selage(as(brps[[1]], "FLStock"))
    attr(opt_sel, "my_point") <- c(S50obs,as.numeric(Fsel_table$F[1]))
    return(opt_sel)
  }
  else {
    opt_sel <- sel_scen[[msy_point]]
    attr(opt_sel, "my_point") <- c(as.numeric(msy_point),as.numeric(Fsel_table$F[1]))
    return(opt_sel)
  }
}


isFbarFjuv <- function(stk.obj){
  juv_age <- apply(mat(stk.obj),2:6,function(x){x<.5})
  if(range(stk.obj)['min'] == range(stk.obj)['minfbar']){
    l_range <- as.character(range(stk.obj)['min'])
  } else {
    l_range <- c(as.character(range(stk.obj)['min']:(range(stk.obj)['minfbar']-1)))
  }
  if(range(stk.obj)['max'] == range(stk.obj)['maxfbar']){
    r_range <- as.character(range(stk.obj)['max'])
  } else {
    r_range <- c(as.character((range(stk.obj)['maxfbar']+1):range(stk.obj)['max']))
  }
  tmp_flq <- FLQuant(1,dimnames = dimnames(mat(stk.obj)))
  tmp_flq[c(l_range,r_range)]<-0
  return(sum(juv_age*tmp_flq))
}