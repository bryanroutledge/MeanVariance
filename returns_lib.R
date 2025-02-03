# libs

# NEED TO DO THE log and load in same chunk so you do not re-re-re-relog
#SEE: https://dplyr.tidyverse.org/reference/mutate_all.html scale2 exaple
continously.compounded.R <- function(x){ log(1 + x/100.0)}

remove.missing.kenfrench <- function(x){ifelse(x<=-99,NA,x)} # -99.99 means na in Ken French data

Clean.Log.Excess<-function(
  Data,
  this.period=NA
){
  if(!(this.period %in%c("Daily","Monthly"))){
    print.error(stop="this.period needs to be Daily or Monthly")
  }
  # Log Them!
  if("Processed" %in% names(Data)){
    print.error(stop="DATA HAS ALREADY BEEN CLEANED AND (1+LOG) ALREADY!!")
  }
  # Clean step:
  Data  <- Data  %>% mutate(
    R_Market = R_MktRF + R_RF,
    t = ymd(date),
    TIME=t,
    Period=this.period
  ) %>%
    rename(
      FF_RmminusRf  = R_MktRF,
      FF_HML_Factor = R_HML,
      FF_SMB_Factor = R_SMB, 
      R_Rf =  R_RF,
    ) %>% select(t,date,R_Rf,R_Market,everything())

  # remove -99.99's
  Data <- Data  %>% 
    mutate_at(vars(starts_with("R_")), remove.missing.kenfrench)  
  
  # Log it
  Data <- Data  %>% 
      mutate_at(vars(starts_with("R_")), continously.compounded.R)
  
  
  
  # to do excess returns, create a new matrix since I am overwriting things
  # This leaves an eR_Rf == 0 ... ok.
  Data <- bind_cols( 
    Data ,
    Data  %>% select(t,starts_with("R_"))%>% 
      mutate(xR_Rf = R_Rf) %>%
      mutate_at(vars(starts_with("R_")), ~. -xR_Rf ) %>%
      setNames(paste0('e', names(.))) 
  ) %>% 
    select(t,date,R_Rf,R_Market,eR_Market,starts_with("FF"),everything()) %>%
    select(-eR_Rf,-exR_Rf )
  
  
  Data <- Data %>% mutate(Processed=TRUE)
  return(Data)
}



Fix.Leap.Year <- function(
	x
){
	# Make sure FEB is 29th
	x = ifelse((leap_year(x)&month(x)==2&day(x)==28),
			x+1,
			x
		)
	return(as_date(x))
}


Load.Daily<-function(
  dir="./"  # local is default    
)
{
  file="FF.Data.Daily.Rdata"
  file=file.path(dir,file)
  print.error(warn=paste0("Loading: ",file))
  load(file)
	return(FF.Data.Daily)
}

Load.Monthly<-function(
    dir="./"  # local is default    
)
{
  file="FF.Data.Monthly.Rdata"
  file=file.path(dir,file)
  print.error(warn=paste0("Loading: ",file))
  load(file)
  return(FF.Data.Monthly)
}

Load.PE<-function(
	f="./ShillerPE.csv",
	Download=FALSE
){
	if(Download){
		system("~/Desktop/Dropbox/FamaFrenchToStata/Get.ShillerPE.pl")
	}
	Data <- read_csv(file=f, col_types = cols(.default = "d", date="c"))
	## FU*** 2008.1  is October!! Fixed in pl script
	Data <- Data %>% mutate(
				TIME =
	 				as_date(
						parse_date_time(date, orders = c("Y.m"))
					) + months(1) - days(1),
				TIME = Fix.Leap.Year(TIME),
				year = year(TIME),
				month= month(TIME),
				day   = day(TIME)
	)  
	# replace Date
	Data <- Data %>% mutate(Period=as.factor("Monthly"))
	Data <- Data %>% select(Period,TIME,year,month,day, everything())	
	return(Data)
}


Make.Meta <- function(
	data
){
	x <- list(
		T.start = min(data$TIME),
		T.max   = max(data$TIME),
		N.obs  =  data %>% select(TIME) %>% pull %>% length,
		Period = data %>% select(Period) %>% unique %>% pull %>% as.character
	)
	x<-data.frame(x)
	return(x)	
}


Real.cols <- function(
	X
){
	# https://stackoverflow.com/questions/5863097/selecting-only-numeric-columns-from-a-data-frame
	x<-X %>% select_if(is.numeric) %>% names 
	x<- x[!x %in% c("n.obs")]
	return(x)
}

Calc.R.stats <- function(
	start.d = NULL,               # if you prefer to send in a date
	start = NULL,           	       # date "20010101
	what  = c(),                  # list of columns in quotes
	what.pattern = NULL,          #  start bits "R_I_So"    
	end.d = NULL,	              
	end   = NULL,                 # usuall all the data to end
	sharpe.calc=TRUE,
	X = Data
){
	if(!is.null(what.pattern)){
		what.pattern <- X %>% select(starts_with(what.pattern)) %>% names
	}
	what <- c(what.pattern,what)
	
	if(is.null(start.d)){
		if(!is.null(start)){start.d=ymd(start)}
		else(start.d=min(X$TIME))
	}
	if(is.null(end.d)){
		if(!is.null(end)){end.d=ymd(end)}
		else(end.d=max(X$TIME))
	}
	X<- X %>% filter(TIME >= start.d & TIME <= end.d )

	# add later the begin/end date by chosen thing	
	
	x<- X %>% select(all_of(what)) %>% gather(key="portfolio",value=r)
	S <- x %>% group_by(portfolio) %>% summarise(
    	mean=mean(r,na.rm=TRUE),
      	sd=sd(r,na.rm=TRUE),
      	min=min(r,na.rm=TRUE),
      	max=max(r,na.rm=TRUE),
      	n.obs = sum(!is.na(r)	),
      	t.0 = min(X$TIME[!is.na(r)]),
      	t.T = max(X$TIME[!is.na(r)]),
    )
    if ((X %>% select(Period) %>% unique %>% pull ) == "Monthly"){
    	f=12
    }
    if ((X %>% select(Period) %>% unique %>% pull ) == "Daily"){
    	f=250
    }
    
	S <- S %>% mutate(
  		mean.er.annual = f * mean,
  		sig.er.annual = sqrt(f)* sd,
  		Sharpe.annual = mean.er.annual / sig.er.annual
	)

	# change order??
	S<- S %>% select(portfolio,
			mean.er.annual, 
			sig.er.annual, 
			Sharpe.annual, 
			t.0,t.T,
			everything()
			)
	if(!sharpe.calc){
		S <- S %>% select(-Sharpe.annual)
	}	
	return(S)
		
}




Best.Worst <- function(
	Best = TRUE,
	Worst = TRUE,
	Flag.Year = NULL,
	## what  = c("R_Rf","R_Market"),
	start = "19260101",           # default date
	N = 20,	
	Print = FALSE,
	X = Data
){
	# best/worst
	R <- One.Dollar(start=start)
	bad<-c()
	good<-c()
	if(Worst){
		bad<- R %>% filter(Portfolio == "R_Market") %>% arrange(R) %>% 
			select(-logW) %>% head(n=N) %>% 
		  mutate(
		    h="Worst",
		    Worst=TRUE
		  )
	}
	if(Best){
		good<- R %>% filter(Portfolio == "R_Market") %>% arrange(-R) %>% 
			select(-logW) %>% head(n=N) %>% 
		  mutate(
		    h="Best",
		    Best=TRUE
		  ) %>% 
		 arrange(R)
	}
	out<-bind_rows(bad,good)
	if(!is.null(Flag.Year)){
		out <- out %>% mutate(flag=ifelse(year(TIME)==Flag.Year,"****"," "))
	}
	if (Print){
		out %>% as.data.frame %>%print
	}
	return(out)
	### NEEDS SOME WORK ...
}


One.Dollar <- function(
	start.d = NULL,               # if you prefer to send in a date
	start = NULL,                # 19790101
	what  = c("R_Rf","R_Market"), # list of columns in quotes
	what.pattern = NULL,          #  start bits "R_I_So"    
	end.d = NULL,	              
	end   = NULL,                 # usuall all the data to end
	end.only = FALSE,             # if you want the terminal only
	make.na.zero = FALSE,         #   To make plots work 
	X = Data
){
	if(!is.null(what.pattern)){
		what.pattern <- X %>% select(starts_with(what.pattern)) %>% names
	}
	what <- c(what.pattern,what)
	
	if(is.null(start.d)){
		if(!is.null(start)){start.d=ymd(start)}
		else(start.d=min(X$TIME))
	}
	if(is.null(end.d)){
		if(!is.null(end)){end.d=ymd(end)}
		else(end.d=max(X$TIME))
	}
	X<- X %>% filter(TIME >= start.d & TIME <= end.d )
	
	if(make.na.zero){
		X[is.na(X)]<-0
	}
	
	X<- X %>% select(TIME, R_Rf, !!(what))
	# add some zeros to start
	x<-X[1,]
	x[1]<-x[1]-1
	x[-1]<-x[-1]*0
	X<-bind_rows(x,X)
	# make long
	
	R <- X %>% gather(key=Portfolio,value=R, !!(what))

	R <- R %>% arrange(Portfolio, TIME ) %>% 
			group_by(Portfolio)  %>%
				mutate (logW = cumsum(R))  %>% ungroup
	
	R <- R %>% filter(TIME <= end.d) 	# should be inoc if end is null		
	if (end.only){			
		R <- R %>% filter(TIME == end.d) %>% 
		select(TIME,Portfolio,logW)	%>%
			arrange(logW) %>%
			mutate (
				W = exp(logW),
				t.0=start.d,
				t.T=end.d
			) %>%
			select(Portfolio,t.0,t.T,W,logW)
	}		
	return(R)
}		


# Do I use this?
Plot.One.Dollar <- function(
	start.d = NULL,               # if you prefer to send in a date
	start = NULL,                # 19790101
	what  = c("R_Rf","R_Market"), # list of columns in quotes
	what.pattern = NULL,          #  start bits "R_I_So"    
	end.d = NULL,	              
	end   = NULL,                 # usuall all the data to end
	size = NULL,
	new.plot=FALSE,               # Make T to get ggplot()
	X = Data)
{
	p <- c()
	if(new.plot){p <- ggplot()}
	if(is.null(size)){size=1}
	
	p<- p+ geom_line(data= One.Dollar(
				start.d = start.d,
				start =  start,
				what  = what,
				what.pattern=what.pattern,
				end.d = end.d,
				end   = end,
				end.only = FALSE,           
				X = Data
			),
			aes(x=TIME,y=logW,color=Portfolio),size=size)		
	return(p)		
}


Plot.TimeSeries <- function(
	start.d = NULL,               # if you prefer to send in a date
	start = NULL,                # 19790101
	what  = NULL, # list of columns in quotes c("R_Rf","R_Market")
	what.pattern = NULL,          #  start bits "R_I_So"    
	end.d = NULL,	              
	end   = NULL,                 # usuall all the data to end
	size = NULL,
	new.plot=FALSE,               # Make T to get ggplot()
	X = Data
){
	if(!is.null(what.pattern)){
		what.pattern <- X %>% select(starts_with(what.pattern)) %>% names
	}
	what <- c(what.pattern,what)
	
	if(is.null(start.d)){
		if(!is.null(start)){start.d=ymd(start)}
		else(start.d=min(X$TIME))
	}
	if(is.null(end.d)){
		if(!is.null(end)){end.d=ymd(end)}
		else(end.d=max(X$TIME))
	}
	X<- X %>% filter(TIME >= start.d & TIME <= end.d )
	
	# do not gather TIME tht is why -TIME
	x<- X %>% select(TIME,all_of(what)) %>% gather(-TIME,key="portfolio",value=r)	
	
	p <- ggplot(data=x)+geom_line(aes(x=TIME,y=r,color=portfolio))
	return(p)
}



