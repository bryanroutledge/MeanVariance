# Handy lib that I use all the time
#
#
library(stringr)
library(tools)    # file paths etc.
library(crayon)   # For colored outpu in terminal https://www.r-bloggers.com/2018/07/coloured-output-in-the-r-console/

# you can do this...??
# https://stackoverflow.com/questions/38351820/negation-of-in-in-r
`%notin%` <- Negate(`%in%`)

get.input <- function(
	x,
	text="Variable= [enter to keep]:",
	type="int"  # trim to (int,log,rea)
){
	text<-sub("=",paste(c("=",x),collapse=""),text)
	type<-substr(tolower(type),0,3)
	s<-readline(prompt=text)
	s<-str_trim(s)
	if(!(s=="")){
        x=s
		if(type=="int"){x=as.integer(x)}
		if(type=="log"){x=as.logical(as.integer(x))}
		if(type=="rea"){x=as.double(x)}
	}
	return(x)
}


# how to pause.  Copy or use this 
pawse <- function(
	t=NULL,
	message=NULL
){
	if(!is.null(t)){
		if(!is.null(message)){print(message)}
		Sys.sleep(t)
	}else{
		if(is.null(message)){message="Press [enter] to continue"}
		readline(prompt=message)
	}
	return()
}

clean <- function(
	x)
{
	x<-str_trim(x) # left and right
	x<-str_to_lower(x)
	x<-gsub("[^a-z0-9]","",x,perl=TRUE)
	x<-gsub("\\s+","",x,perl=TRUE)	
	return(x)
}

clean.dash <- function(
	x)
{
	x<-str_trim(x) # left and right
	x<-gsub("[^A-Za-z0-9]","-",x,perl=TRUE)
	x<-gsub("\\s+","",x,perl=TRUE)	
	x<-gsub("-+","-",x,perl=TRUE)	
	x<-gsub("^-|-$","",x,perl=TRUE)			
	return(x)
}

clean.col.names <- function(
  x)
{
  x<-gsub("%","percent",x,perl=TRUE)
  x<-str_to_lower(x)
  x<-clean.dash(x)
  x<-gsub("-",".",x,perl=TRUE)	
  x<-make.unique(x)
  return(x)
}



f.pdf<-function(
        file,
        dir="Graphics"
        )
        #  Gets you back FRED.PDF even if you gave it FRED.PDF or FRED
{
	# Graphics can be NULL or "" to get you no leading / just local
   f<-paste(c(file,".pdf"),collapse="")	
   if (!is.null(dir) ){
   	    if (dir==""){
   			# print("dir-->NULL")
   			dir<-NULL
   		}
   	}
   	if (!is.null(dir)){	
   		f<-paste(c(dir,"/",file,".pdf"),collapse="")
   } else{
   		f<-paste(c(file,".pdf"),collapse="")
   }
   f<-sub("(pdf.)+pdf", "pdf", f, ignore.case = TRUE, perl = TRUE)
   print(paste(c("TO FILE:",f),collapse=""))
   return(f)
}

f.name<-function(
  file,
  dir="./",
  extension=NULL,
  echo=TRUE
){
  #  Gets you back FRED.ex even if you gave it FRED.ex or FRED
  if (!is.null(dir)){	
    file<-paste(c(dir,"/",file),collapse="")
  }
  file<- sub("//+","/",file,ignore.case=TRUE,perl=TRUE)
  
  if(!is.null(extension)){
    extension<-paste(c(".",extension),collapse="")
    ex<- sub("\\.+","\\.",extension,ignore.case=TRUE,perl=TRUE)
    if(!grepl(paste0(extension,"$"),file)){
      file<-paste0(file,extension)
    }
  }

  if(echo){
    print(paste(c("TO FILE:",file),collapse=""))
  }
  return(file)
}

f.adddate<-function(x){
  # add the date to x
  d=dirname(x)
  f=basename(x)
  e=file_ext(x)
  x=paste0(d,"/",f,".")
  ## NOT WORKING YET... 
}

print.plot<-function(
	p,
	file,
    dir="Graphics",
    height=NA,
    width=NA,
    aspect.ratio=NA, # instead of height width
    eps=FALSE
    )
{
      f<-f.pdf(file,dir=dir)
      #  https://stackoverflow.com/questions/20103212/how-to-set-the-whole-chart-to-become-wider-in-ggplot
      

	if(!is.na(aspect.ratio)){
		height <- 6
		width  <- height * aspect.ratio
	}

	if(!is.na(height)&!is.na(width)){
		pdf(file=f, height=height, width=width) 
	}
	else {
      pdf(f) 
    }  
      print(p)
      dev.off()

      
      if (eps){
      	  f<-sub("(pdf.)*pdf", "eps", f, ignore.case = TRUE, perl = TRUE)
	      postscript(f, horizontal = FALSE)
	      print(p)
	      dev.off()
	      print("EPS...")
	      print(f)
	   }

      return(f)      
}


# CONSIDER FOR PNG and other??
print.plot.png<-function(
  p,
  f,
  f.stub=NA, #"/Users/rout/Desktop/Dropbox/CryptoPeg/Graphics/UPDATE-Vol/BTC.Volatility",
  f.path=NA,
  aspect.ratio = 1.4,
  size = 6,
  units = "in",
  display.plot=FALSE,
  echo.file=TRUE
){
  # 
  # f.path / f.stub . f .png
  if(!is.na(f.stub)){
    f<- paste(c(f.stub,".",f),collapse="")
  }
  if(!is.na(f.path)){
    f<- file.path(f.path,f)
  }
  f<- paste(c(f,".png"),collapse="")
  
  f<-sub("(png.)*png", "png", f, ignore.case = TRUE, perl = TRUE)
  f<-sub("//+", "/", f, ignore.case = TRUE, perl = TRUE)

  if(echo.file){print(f)}
  if(display.plot){print(p)}
  ggsave(filename=f,
         device="png",
         plot=p,
         width = aspect.ratio*size,
         height = size,
         units = units
  )
}

        
log.seq <- function(
	from,
	to,
	length.out=10,
	squish.factor=0.0 # 0.0=no squish
)
{        
	# squish moves left end close to zero
	s <- from*(squish.factor)
	if(squish.factor<0 | squish.factor >= 1){
			error("NOT   0<= squish.factor<1 ")
	}
	return(exp(seq(log(from-s), log(to-s), length.out = length.out))+s)
}
	
	
	
headtail <- function(
	data,
	n = 5,
	n.head = NULL,
	n.tail = NULL
){
	if(is.null(n.head)){n.head = n}
	if(is.null(n.tail)){n.tail = n}
	bind_rows( head(data,n=n.head),tail(data,n=n.tail))
}



# Just to do the matlab example
close.all<-function(){graphics.off()}



# 
positive <- function(x){ifelse(x>0,x,0)}
negative <- function(x){ifelse(x<0,x,0)}

clamp<-function(x,lower=-Inf,upper=+Inf){pmax(pmin(x, upper), lower)}

is.equal<-function(
  x,
  y,
  tol=2.8e-10
){
  abs(x-y)<tol
}

# See https://www.r-bloggers.com/2018/07/coloured-output-in-the-r-console/
# use cat and cryaon to make this better
# https://www.r-project.org/nosvn/pandoc/crayon.html
print.error<-function(
  stop=NULL,   # stop =c("reason","reason more") --> will stop
  warn=NULL,   # warn =c("reason","reason more") --> will NOT stop
  note=NULL    
){
  
  stop.style = red $ bold
  warn.style = make_style("maroon", bg = FALSE)
  note.style = blue

  if(!is.null(note)){
    note <- gsub("^","NOTE: ",note)
    note <- paste(note,collapse="\n")
    cat(note.style(note))
  }  
    
  if(!is.null(warn)){
      warn <- gsub("^","WARNING: ",warn)
      warn <- paste(warn,collapse="\n")
      cat(warn.style(warn))
  }
  if(!is.null(stop)){
    stop <- gsub("^","ERROR: ",stop)
    stop <- paste(c(stop,""),collapse="\n")
    cat(stop.style(stop))
    stop("-- stop requested in print.error (in bryan_lib.R) --")
  }
}

stop.here<-function(){print.error(stop="stop here")}

print.all<-function(data){print(data,n=Inf)}
	

print.sample<-function(
	data,
	p=0.10,
	n = 10
){
	# print 10% of a data frame
	N=clamp(round(1/p),lower=1)
	data %>%
	     mutate(pop = sample(0:N, n(), replace = TRUE))%>%
	     filter(pop == 0) %>%
	     select(-pop) %>%
	     print(n=n)
}



make.eps.name<-function(x){
  n=length(x)
  sub("^","eps.",c(1:n))
}
make.epsilon<-function(
  Rows,
  Cols=1,
  Seed=0,
  Type="uniform")
{
  if(Seed!=0){
    set.seed(Seed)
  }else{
    set.seed(NULL)
  }
  #as_tibble(c(rnorm(parm$N))) %>% rename(eps=value)
  # as_tibble(c(runif(N))) %>% rename(eps=value)
  x<-NULL
  if(Type=="uniform"){
    x<-matrix(runif(Rows*Cols),ncol=Cols)%>%as_tibble( .name_repair = make.eps.name)
  }
  if(Type=="normal"){
    x<-matrix(rnorm(Rows*Cols),ncol=Cols)%>%as_tibble( .name_repair = make.eps.name)
  }
  return(x)
}


# Shorten Names to make cols workable
# You often need this with google form sheet results
# Note Names is global 
# To do make "Names" a choice in function
Map.Col.Names.to.Letters<-function(
  X,
  Name.Global="Names",
  echo.names=TRUE
){
  old.names=names(X)
  new.names=LETTERS[1:length(old.names)]

  Names<-list(detail=old.names,short=new.names) %>% as_tibble() %>% select(short,detail)
  X <- as_tibble(X, .name_repair = ~new.names) 
  
  if(echo.names){
    print.error(note="Names is GLOBAL\n")
    Names %>% mutate(x=paste0(short,"=",detail)) %>% select(x)%>% print()
  }
  # <<- makes it global
  assign(Name.Global,Names,envir = .GlobalEnv) # MAKE GLOBAL
  return(X)
}  

# for printing col names in google sheet
print.letters.to.names<-function(n){
  n%>%mutate(Index=paste0(short,"=",detail))%>%select(Index)
}

mean_na<-function(x){mean(x,na.rm=TRUE)}
std_na<-function(x){sd(x,na.rm=TRUE)}
max_na<-function(x){max(x,na.rm=TRUE)}
safe.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=TRUE), NA)  # if all NA do not say INF
safe.min <- function(x) ifelse( !all(is.na(x)), min(x, na.rm=TRUE), NA)


mod <-function(
    numerator, 
    denominator
){
  numerator %/% denominator
}

rem <-function(
    numerator, 
    denominator
){
  numerator %% denominator
}
millions <-function(x){
  return(x / 1e+06)
}

billions <-function(x){
  return(x / 1e+09)
}
