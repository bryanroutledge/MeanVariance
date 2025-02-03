# meanvariance_plot_lib.R

Suggest.Plot.Range<-function(
    E,
    scale.factor=0.15,
    N.points=15,
    mu.range=NULL
){
  # Pick a range to plot (awlays a bit tricky)
  #mu <- seq(from=0.00,to=0.15,by=0.01)
  # Add a few more dots around the GMV
  #RbarGMV = E$portfolio.moments %>% Get.GMV.Rbar
  #mu<- c(mu,RbarGMV-10^(-5),RbarGMV+10^(-5))  # left right of GMV so plot "touches" at point
  
  
  x<-Report.Economy.Returns(E)
  RbarGMV = E$portfolio.moments %>% Get.GMV.Rbar
  
  mu <- seq(
    from= ifelse(min(x$Rbar)<0,min(x$Rbar)*(1+scale.factor),min(x$Rbar)*(1-scale.factor)),
    to=max(x$Rbar) *  (1+scale.factor),
    length.out=N.points)
  
  if(!is.null(mu.range)){
    mu <- seq(
      from= min(mu.range),
      to=max(mu.range),
      length.out=N.points)
  }
  
  
    mu<- c(mu,RbarGMV-10^(-8),RbarGMV+10^(-8))  # left right of GMV so plot "touches" at point
  

  return(mu)
}


Pretty.Mean.Variance.Plot <-function(
    p,
    guides.off=TRUE,
    caption=NA
){
  # Not needed but make things pretty
  p<- p+ theme(axis.ticks.x = element_blank(),
               axis.line.x = element_blank(),
               axis.ticks.y = element_blank(),
               axis.line.y = element_blank(),
  )
  p<- p+ scale_y_continuous(labels = scales::percent)   
  p<- p+ scale_x_continuous(labels = scales::percent)   
  
  p<-p + labs(
      x = expression("Volatility (standard deviaition " * sigma * ")"),  # Use expression to get the sigma symbol
      y = "Expected Return"   # Use expression to get the sigma symbol
  )
  if(!is.na(caption)){
    p<-p+labs(caption=caption)
  }
  
  if(guides.off){
    p<- p+guides(color="none",alpha="none",size="none")
  }
  return(p)
}

Plot.Mean.Variance<-function(
    E,
    show.base.portfolios=TRUE,
    show.assets = TRUE,
    label.base.portfolios=TRUE,
    label.assets=TRUE,
    caption=NA
){
  p <- ggplot(data = E$portfolio.moments %>% group_by(Efficient))
  p<- p + geom_line(aes(y=Rbar,x=RSigma,color=Efficient))  # Color is needed to connect dots in right order
  
  # Add wR and w1
  if(show.base.portfolios){
    p <- p + geom_point(data = E$portfolio.moments %>% filter(Name%in%c("wR","w1")),
                        aes(y=Rbar,x=RSigma),
                        size=4,color="black")
    if(label.base.portfolios){
      p<- p + geom_label_repel(
        data = E$portfolio.moments %>% filter(Name%in%c("wR","w1")),
        aes(y=Rbar,x=RSigma,label=Name),
        nudge_x = 0,nudge_y=0,na.rm = TRUE,segment.color="grey")
    }
  } 
  # Add Assets
  if(show.assets){
    p <- p + geom_point(data = E$Asset.Moments,
                        aes(y=Rbar,x=RSigma),
                        size=2,color="red")
    
    if(label.assets){ 
      p<- p + geom_label_repel(
        data = E$Asset.Moments,
        aes(y=Rbar,x=RSigma,label=Clean.Asset.Names(Name)),
        nudge_x = 0,nudge_y=0,na.rm = TRUE,segment.color="grey")
    }
  }  
  p<- Pretty.Mean.Variance.Plot(p,
                                caption=caption)
  return(p)
}