# meanvariance_lib.R

Clean.Asset.Names<-function(
  x
){
  x<- gsub("eR_I_","",x)
  return(x)
}


Estimate.Mean.Variance<-function(
    data,
    assets=NA,   # use all if NA
    window=NA,    # use all if NA
    periods=12   # convert monthly to annual --  easier to interpret things
){
  if(!any(is.na(assets))){
    data<-data %>% select(t,all_of(assets))
  }
  if(!any(is.na(window))){
    data<- data %>% filter(t>=min(window)&t<=max(window))
  }
  Rbar = data %>% select(-t) %>% summarise_all(mean_na) %>% t()
  RSigma = data %>% select(-t) %>% summarise_all(std_na) %>% t()  # FYI 
  
  V=cov(data %>% select(-t))
  Rho=cor(data %>% select(-t))
  
  if(!is.na(periods)){
    Rbar = periods * Rbar
    V    = periods * V  #  i.i.d. assumption per month
    RSigma=sqrt(periods)*RSigma
    Rho  = Rho             # not depend on period
  }
  
  asset.names <- rownames(Rbar)

  
  Asset.Moments <- tibble(
    Type="Asset",
    Name= asset.names %>% as.vector(),
    Rbar = Rbar %>% as.vector(),
    RSigma = RSigma %>% as.vector()
  )
  
  Economy = list(
    Rbar=Rbar,
    V=V,
    Rho=Rho,
    RSigma=RSigma,
    Assets=asset.names,
    Window=window,
    Asset.Moments=Asset.Moments
  )
  
  return(Economy)
}

Calc.Rbar <- function(w,Rbar){
  return(t(w) %*% Rbar %>% as.numeric())
}

Calc.Sigma <- function(w,V){
  return(t(w) %*% V %*% w %>% as.numeric() %>% sqrt())
}


Get.GMV.Rbar<- function(
    portfolio.moments
){
  # this is a handy thing to know for plots
  return(portfolio.moments %>% filter(Name=="w1") %>% pull(Rbar) %>% as.numeric())
}

Report.Economy.Returns<-function(
    E
){
  
  Table <- bind_rows(
    E$Asset.Moments,
    E$portfolio.moments %>% 
      mutate(
        Type="Portfolio",
        Name = Clean.Asset.Names(Name)
      )
  )
  
  return(Table)
}

Report.Economy.Portfolio<-function(
    E,
    portfolio.name = c("wR","w1"),
    Show.Correlation = TRUE,
    Pretty=TRUE
){
  x <- Report.Economy.Returns(E) %>%       # Asset Returns
    filter(Type=="Asset") %>%
    select(Name,Rbar,RSigma)
  
  if(Show.Correlation){
    x <- bind_cols(x, E$Rho %>% 
                     as_tibble %>%
                     rename_with(~ Clean.Asset.Names(paste0("Rho_", .)), everything())
    )
  }
  
  Table<- full_join(
    x,
    #  
    E$portfolio.weights %>%            # Portfolio weights
      select(Name,all_of(portfolio.name)),
    by = join_by(Name)
  ) %>%
    relocate(Name,all_of(portfolio.name))
  
  for (p in portfolio.name){
    P<- E$portfolio.moments %>% filter(Name == p) 
    Table<- Table %>%
      add_row(
        Name =p , Rbar=P$Rbar, RSigma=P$RSigma,
        !!sym(p) := 1
      )
  }
  
  if(Pretty){
    Table <- Table %>%
    mutate(Name=Clean.Asset.Names(Name)) %>%
      gt %>%
      fmt_percent(
        columns = c(Rbar,RSigma),
        decimals = 2
      ) %>%
      fmt_number(
        columns = starts_with(("w")),
        decimals =2
      )
    
    if(Show.Correlation){
     Table <- Table %>%
       fmt_number(
         columns = starts_with(("Rho")),
         decimals =2
       )
    }
  }
  
  return(Table)
}





