# plot functions
# Check for changing selectivity
plotSels <- function(stk){
  sel <- harvest(stk)%/%apply(harvest(stk), c(2:6),max)
  ss <- as.data.frame(sel)
  ss$unique = 'Selectivity'
  p_sel <- ggplot(data = ss, aes(x = age, y = data, group = year,color = year)) +
    geom_line() + scale_colour_viridis_c(option = 'C') + facet_wrap(~unique) +
    theme(legend.position = 'bottom', axis.title.y=element_blank())+
    scale_x_continuous(expand = c(0,0), 
                       breaks = seq(as.numeric(range(stk)['min']), as.numeric(range(stk)['max']),1))+
    labs(colour = "")+ggtitle(name(stk))
  return(p_sel)
}

ploteqselex2 = function(brps,Fmax=2.,panels=NULL, ncol=NULL,colours=NULL,Ftrg=c("none","msy","f0.1")){
  # Colour function
  if(is.null(colours)){colf = r4col} else {colf = colours}
  if(is.null(panels)) panels=1:4
  if(is.null(ncol)){
    if(length(panels)%in%c(1,3)){ncol=length(panels)} else {ncol=2}
  }
  
  # Check range
  if(paste(brps[[1]]@model)[3]%in%c("a + ssb/ssb - 1")){
    pr = TRUE
    # lim = min(Fmax,max(2*refpts(brps[[1]])["f0.1","harvest"],refpts(brps[[1]])["Fref","harvest"]*1.5,dims(brps[[1]])[["min"]]))
    # @Danai 24/10/2023
    lim = Fmax
    quants = c("YPR","SPR")
    labs = c("Relative YPR",expression(SPR/SPR[0]))
    
  } else {
    pr = FALSE
    # Deleted for demonstration of aopt on isopleth
    # lim = min(Fmax,max(2*refpts(brps[[3]])["msy","harvest"],refpts(brps[[1]])["Fref","harvest"]*1.05,dims(brps[[1]])[["min"]]))
    # @Danai 31/08/2023
    lim = Fmax
    quants = c("Yield","SSB")
    labs = c("Relative Yield",expression(SBB/SSB[0]))
  }
  
  # Prep some data for plotting
  fbar(brps[[1]]) = seq(0,lim,lim/101)[1:101]
  obs = data.frame(obs="obs",model.frame(metrics(brps[[1]],list(ssb=ssb, harvest=fbar, rec=rec, yield=landings)),drop=FALSE))
  obs[,8:11][obs[,8:11]<0] <- 0
  S50 = as.list(an(brps[-1]@names))
  isodat = do.call(rbind,Map(function(x,y){
    fbar(x) = seq(0,lim,lim/101)[1:101]
    mf =  model.frame(metrics(x,list(ssb=ssb, harvest=fbar, rec=rec, yield=landings)),drop=FALSE)
    data.frame(S50=y,as.data.frame(mf))  
  },brps[-1],S50))
  isodat$yield = isodat$yield#/max(isodat$yield)
  isodat[,8:11][isodat[,8:11]<0] <- 0
  Fobs = an(refpts(brps[[1]])["Fref","harvest"])
  Yobs = an(refpts(brps[[1]])["Fref","yield"])
  Sobs = an(refpts(brps[[1]])["Fref","ssb"])#/an(refpts(brps[[1]])["virgin","ssb"])
  Sa=brps[[1]]@landings.sel/max(brps[[1]]@landings.sel)
  S50obs = s50(Sa)
  isodat$Fo = c(obs$harvest,rep(NA,nrow(isodat)-nrow(obs)))
  isodat$Yo = c(obs$yield,rep(NA,nrow(isodat)-nrow(obs)))/max(isodat$yield)
  isodat$So = c(obs$ssb,rep(NA,nrow(isodat)-nrow(obs)))/max(isodat$ssb)
  
  if(Ftrg[1]!="none"){
    ftrg = do.call(rbind,Map(function(x,y){
      frp= c(refpts(x)[Ftrg,"harvest"])
      data.frame(s50=y,ftrg=frp)
    },brps[-1],S50))
    ftrg = ftrg[ftrg$ftrg<lim,]
  }
  
  ylab = "Age-at-50%-Selectivity"
  
  
  
  # F vs Yield
  P1 = ggplot(data=isodat,aes(x=harvest,y=yield/max(yield),group=S50))+
    geom_line(aes(color=S50))+geom_line(aes(x=Fo,y=Yo),size=0.7,linetype="dashed", na.rm=TRUE)+
    scale_color_gradientn(colours=rev(colf(20)))+ylab(labs[1])+
    geom_segment(aes(x = Fobs, xend = Fobs, y = 0, yend = Yobs/max(yield)), colour = "black",size=0.3,linetype="dotted")+
    geom_segment(aes(x = 0, xend = Fobs, y = Yobs/max(yield), yend = Yobs/max(yield)), colour = "black",size=0.3,linetype="dotted")+
    geom_point(aes(x=Fobs,y=Yobs/max(yield)),size=2)+
    xlab("Fishing Mortality")+
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'),
          legend.text = element_text(size=7),
          legend.key.width = unit(0.6, 'cm'),
          axis.title=element_text(size=10),
          legend.title=element_text(size=9)
    )+
    scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0),limits=c(0,1))
  # F vs SSB  
  P2 = ggplot(data=isodat,aes(x=harvest,y=ssb/max(ssb),group=S50))+
    geom_line(aes(color=S50))+geom_line(aes(x=Fo,y=So),size=0.7,linetype="dashed", na.rm=TRUE)+
    scale_color_gradientn(colours=rev(colf(20)))+
    geom_segment(aes(x = Fobs, xend = Fobs, y = 0, yend = Sobs/max(ssb)), colour = "black",size=0.3,linetype="dotted")+
    geom_segment(aes(x = 0, xend = Fobs, y = Sobs/max(ssb), yend = Sobs/max(ssb)), colour = "black",size=0.3,linetype="dotted")+
    geom_point(aes(x=Fobs,y=Sobs/max(ssb)),size=2)+
    ylab(labs[2])+xlab("Fishing Mortality")+
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'),
          legend.key.width = unit(0.6, 'cm'),
          legend.text = element_text(size=7),
          axis.title=element_text(size=10),
          legend.title=element_text(size=9)
    )+ 
    scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0),limits=c(0,1))
  
  # Isopleth plot Yield
  colbr = c(seq(0,0.6,0.2),seq(0.7,0.9,0.1),0.95,1)
  conbr = c(0,0.2,0.4,seq(0.5,0.9,0.1),0.95,1)
  nbr = length(colbr)
  P3=ggplot(isodat, aes(x=harvest,y=S50))+
    geom_raster(aes(fill = yield/max(yield)), 
                interpolate = F, hjust = 0.5, vjust = 0.5)+ 
    metR::geom_contour2(aes(z=yield/max(yield)),color = grey(0.4,1),breaks =conbr )+ 
    metR::geom_text_contour(aes(z=yield/max(yield)),stroke = 0.2,size=3,skip=0,breaks = conbr)+
    scale_fill_gradientn(colours=rev(colf(nbr+3))[-c(10:11,13)],limits=c(-0.03,1), breaks=colbr, name=paste(quants[1]))+
    geom_point(aes(x=Fobs,y=S50obs),size=2.5)+
    geom_segment(aes(x = Fobs, xend = Fobs, y = min(S50), yend = S50obs), colour = "black",size=0.3,linetype="dotted")+
    geom_segment(aes(x = 0, xend = Fobs, y = S50obs, yend = S50obs), colour = "black",size=0.3,linetype="dotted")+
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'),
          legend.key.width = unit(0.6, 'cm'),
          legend.text = element_text(size=7),
          axis.title=element_text(size=10),
          legend.title=element_text(size=9)
    )+
    scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(-0.03, 0))+
    ylab(ylab)+xlab("Fishing Mortality")
  
  # Isopleth SSB
  colbr = c(0.05,seq(0,1,0.1))
  conbr = c(seq(0.05,0.4,0.05),seq(0.5,0.6,1),1)
  nbr = length(colbr)
  P4 = ggplot(isodat, aes(x=harvest,y=S50))+
    geom_raster(aes(fill = ssb/max(ssb)), 
                interpolate = T, hjust = 0.5, vjust = 0.5)+ 
    metR::geom_contour2(aes(z=ssb/max(ssb)),color = grey(0.4,1),breaks =conbr )+
    metR::geom_text_contour(aes(z=ssb/max(ssb)),stroke = 0.2,size=3,skip=0,breaks = conbr)+
    scale_fill_gradientn(colours=rev(colf(nbr+4))[-c(4:7)],limits=c(-0.03,1), breaks=colbr, name=quants[2])+
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'),
          legend.key.width = unit(0.6, 'cm'),
          legend.text = element_text(size=7),
          axis.title=element_text(size=10),
          legend.title=element_text(size=9)
    )+
    geom_point(aes(x=Fobs,y=S50obs),size=2.5)+
    geom_segment(aes(x = Fobs, xend = Fobs, y = min(S50), yend = S50obs), colour = "black",size=0.3,linetype="dotted")+
    geom_segment(aes(x = 0, xend = Fobs, y = S50obs, yend = S50obs), colour = "black",size=0.3,linetype="dotted")+
    scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(-0.03, 0))+
    ylab(ylab)+xlab("Fishing Mortality")
  if(Ftrg[1]!="none"){
    P3 = P3+geom_point(data=ftrg,aes(x=ftrg,y=s50),shape = 21, colour = "black",size=1.1, fill = "white")
    P4= P4+geom_point(data=ftrg,aes(x=ftrg,y=s50),shape = 21, colour = "black",size=1.1, fill = "white")
  }
  plots <- list(P1=P1,P2=P2,P3=P3,P4=P4)
  
  if(length(panels)>1) return(gridExtra::grid.arrange(grobs =  plots[panels], ncol = ncol))  
  if(length(panels)==1) return(plots[[panels]])  
} #}}}




### Plotting grid for checking:

plotGridSelInd <- function(stk){
  df <- as.data.frame(fbar(stk))
  df$unique = 'F'
  p_F <- ggplot(data = df, aes(x = year, y = data)) +
    # ggplotFL::geom_flpar(data = rps, x = xpos, colour = color_thres)+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(limits = c(0,NA))+
    # scale_y_continuous(labels = scaleFUN)+
    geom_line() + facet_wrap(~unique)+theme(axis.title.y=element_blank(),
                                            axis.title.x=element_blank())
  df <- as.data.frame(ssb(stk))
  df$unique = 'SSB'
  p_ssb <- ggplot(data = df, aes(x = year, y = data)) +
    # ggplotFL::geom_flpar(data = rps, x = xpos, colour = color_thres)+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(limits = c(0,NA))+
    # scale_y_continuous(labels = scaleFUN)+
    geom_line() + facet_wrap(~unique)+theme(axis.title.y=element_blank(),
                                            axis.title.x=element_blank())
  
  df <- as.data.frame(rec(stk))
  df$unique = 'Recruitment'
  p_rec <- ggplot(data = df, aes(x = year, y = data)) +
    # ggplotFL::geom_flpar(data = rps, x = xpos, colour = color_thres)+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(limits = c(0,NA))+
    # scale_y_continuous(labels = scaleFUN)+
    geom_line() + facet_wrap(~unique)+theme(axis.title.y=element_blank(),
                                            axis.title.x=element_blank())
  
  
  p_sel <- plotSels(stk) +theme(legend.position = 'left')+ggtitle("")
  
  df <- as.data.frame(FjFap[[name(stk)]])
  df$unique = 'Fjuv/Fapical'
  p_FjFap <- ggplot(data = df, aes(x = year, y = data)) +
    # ggplotFL::geom_flpar(data = rps, x = xpos, colour = color_thres)+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(limits = c(0,NA))+
    # scale_y_continuous(labels = scaleFUN)+
    geom_line() + facet_wrap(~unique)+theme(axis.title.y=element_blank(),
                                            axis.title.x=element_blank())
  
  
  df <- as.data.frame(FrFb[[name(stk)]])
  df$unique = 'Frec/Fbar'
  p_FrFb <- ggplot(data = df, aes(x = year, y = data)) +
    # ggplotFL::geom_flpar(data = rps, x = xpos, colour = color_thres)+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(limits = c(0,NA))+
    # scale_y_continuous(labels = scaleFUN)+
    geom_line() + facet_wrap(~unique)+theme(axis.title.y=element_blank(),
                                            axis.title.x=element_blank())
  
  
  
  df <- as.data.frame(FjFb[[name(stk)]])
  df$unique = 'Fjuv/Fbar'
  p_FjFb <- ggplot(data = df, aes(x = year, y = data)) +
    # ggplotFL::geom_flpar(data = rps, x = xpos, colour = color_thres)+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(limits = c(0,NA))+
    # scale_y_continuous(labels = scaleFUN)+
    geom_line() + facet_wrap(~unique)+theme(axis.title.y=element_blank(),
                                            axis.title.x=element_blank())
  
  
  df <- as.data.frame(FjFad[[name(stk)]])
  df$unique = 'Fjuv/Fadult'
  p_FjFad <- ggplot(data = df, aes(x = year, y = data)) +
    # ggplotFL::geom_flpar(data = rps, x = xpos, colour = color_thres)+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(limits = c(0,NA))+
    # scale_y_continuous(labels = scaleFUN)+
    geom_line() + facet_wrap(~unique)+theme(axis.title.y=element_blank(),
                                            axis.title.x=element_blank())
  
  
  p1 <- ggarrange(p_F,p_ssb,p_rec,p_sel,nrow = 4)
  p2 <- ggarrange(p_FjFap,p_FrFb,p_FjFb,p_FjFad,nrow = 4)
  
  p <- ggarrange(p1,p2)
  return(annotate_figure(p,top = name(stk)))
}

# plotting overlapping ages in Fbar and Fjuv
plotFbIsFj <- function(stk){
  dff <- as.data.frame(mat(stk))
  dff$mat  <- ifelse(dff$data < 0.5, "juv", "adult")
  dff$fbar <- ifelse(dff$age %in% an(range(stk)['minfbar']):an(range(stk)['maxfbar']), TRUE, FALSE)
  
  
  dfjuv <- dff %>%
    filter(mat == 'juv') %>%
    group_by(year) %>%
    summarise(
      maxjuvage = max(age, na.rm = TRUE),
      minjuvage = min(age, na.rm = TRUE)
    )
  
  dfbar <- dff %>%
    filter(fbar == TRUE) %>%
    group_by(year) %>%
    summarise(
      maxfbarage = max(age, na.rm = TRUE),
      minfbarage = min(age, na.rm = TRUE)
    )
  
  ggplot(dfjuv, aes(x = year)) +
    geom_ribbon(aes(ymin = minjuvage, ymax = maxjuvage+0.1),
                fill = "skyblue", alpha = 0.5) +
    geom_ribbon(data = dfbar, aes(ymin = minfbarage-0.1, ymax = maxfbarage),
                fill = "salmon", alpha = 0.5) +
    scale_y_continuous(limits = c(range(stk)['min']-0.1,range(stk)['max']+0.1), 
                       breaks = seq(range(stk)['min'],range(stk)['max'],1))+
    geom_line(data = dfbar,aes(y = minfbarage), color = "salmon", linewidth = 0.5) +
    geom_line(aes(y = maxjuvage), color = "skyblue", linetype = "dashed", linewidth = 0.5) +
    labs(
      title = paste0('Overlap F juvenile and Fbar - ', name(stk)),
      x = "Year",
      y = "age"
    )
}


# plotting overlapping ages in Fapical and Fjuv
plotFapIsFj <- function(stk){
  dff <- as.data.frame(mat(stk))
  sel <- as.data.frame(harvest(stk)%/%apply(harvest(stk), c(2:6),max))[c(1,2,7)]
  names(sel)[3] <- 'sel'
  dff <- merge(dff,sel)
  dff$mat  <- ifelse(dff$data < 0.5, "juv", "adult")
  dff$fap <- ifelse(dff$sel == 1, TRUE, FALSE)
  
  
  dfjuv <- dff %>%
    filter(mat == 'juv') %>%
    group_by(year) %>%
    summarise(
      maxjuvage = max(age, na.rm = TRUE),
      minjuvage = min(age, na.rm = TRUE)
    )
  
  dfbar <- dff %>%
    filter(fap == TRUE) %>%
    group_by(year) %>%
    summarise(
      maxfapage = max(age, na.rm = TRUE),
      minfapage = min(age, na.rm = TRUE)
    )
  
  ggplot(dfjuv, aes(x = year)) +
    geom_ribbon(aes(ymin = minjuvage, ymax = maxjuvage+0.1),
                fill = "skyblue", alpha = 0.5) +
    geom_ribbon(data = dfbar, aes(ymin = minfapage-0.1, ymax = maxfapage),
                fill = "salmon", alpha = 0.5) +
    scale_y_continuous(limits = c(range(stk)['min']-0.1,range(stk)['max']+0.1), 
                       breaks = seq(range(stk)['min'],range(stk)['max'],1))+
    geom_line(data = dfbar,aes(y = maxfapage), color = "salmon", linewidth = 0.5) +
    geom_line(aes(y = maxjuvage), color = "skyblue", linetype = "dashed", linewidth = 0.5) +
    labs(
      title = paste0('Overlap F juvenile and Fapical - ', name(stk)),
      x = "Year",
      y = "age"
    )
}

plotIndicators <- function(stk){
  fjfa <- Fjuv.over.Fadult(stk)
  fjfa_df <- as.data.frame(fjfa)
  fjfa_df<- fjfa_df[,c(1,2,7)]
  names(fjfa_df)[3] <-'Fjuv/Fadult'
  
  fjfb <- Fjuv.over.Fbar(stk)
  fjfb_df <- as.data.frame(fjfb)
  fjfb_df<- fjfb_df[,c(1,2,7)]
  names(fjfb_df)[3] <-'Fjuv/Fbar'
  
  fjfm <- Fjuv.over.Fmax(stk)
  fjfm_df <- as.data.frame(fjfm)
  fjfm_df<- fjfm_df[,c(1,2,7)]
  names(fjfm_df)[3] <-'Fjuv/Fapical'
  
  df <- merge(fjfa_df,fjfb_df)
  df <- merge(df, fjfm_df)
  
  df_long <- melt(as.data.table(df),
                  id.vars = "year",
                  measure.vars = c("Fjuv/Fadult", "Fjuv/Fbar", "Fjuv/Fapical"),
                  variable.name = "Indicator",
                  value.name = "value")
  dff <- as.data.frame(df_long)
  
  
  ggplot(data = dff) +
    geom_line(aes(x = year, y = value, group = Indicator, color = Indicator), linewidth = 1)+
    theme(legend.position = 'bottom', axis.title.y=element_blank())+
    scale_y_continuous(limits = c(-0.01,NA))
}