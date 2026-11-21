# Record the current process time to measure total execution duration later
start_time <- proc.time()


# loading the packages required
packagename<-c("quantmod","ggplot2")
if (length(setdiff(packagename, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packagename, rownames(installed.packages())),repos = "https://cran.uni-muenster.de/", Ncpus = 16)  
}

lapply(packagename, require, character.only = T)
rm(packagename)


# define the start and the end date for the analysis in the format "year-month-day" (it has to be at least three days before the present date) 
stdat<-"2026-03-01"
endat<-"2026-11-03"


# define the factor by which you want the stock to change at minimum (1 stands for 100% which effectively means double as much as the average low)
fac<-0.8

# set the average low to zero at the beginning 
lowme<-0

# minimum number of changes according to the defined factor
minvol<-3

# minimum number of days when the stock price is above the stock price defined by the factor without going below that value
minday<-3
  
# minimum number of days between two periods of time in which the stock price is above the stock price defined by the factor 
minbet<-3


# retrieve all stock acronyms
sym<-stockSymbols()
sym<-sym[,1]


# progress bar to see the % of completion
n <- length(sym)
pb <- txtProgressBar(min = 0, max = n, style=3)

dfl<-list()

# Actual loop: 
for(i in 1:length(sym)) {
  tryCatch({sym[i]-> symbol
  # specify the "from" date to desired start date
  tryit <- try(getSymbols(symbol,from=stdat, to=endat, src='yahoo'))
  if(inherits(tryit, "try-error")){
    i <- i+1
  } else {
    # specify the "from" date to desired start date
    data <- getSymbols(symbol, from=stdat, to=endat, src='yahoo')
    dfl[[i]] <- data.frame(get(sym[i]), date = as.Date(rownames(data.frame(get(sym[i])))), stock = sym[i])
    rm(symbol)
    rm(list = sym[i])
  }
  setTxtProgressBar(pb, i)},
  error = function(msg){
    i <- i +1
  })
}


# save original download
dfl_orig<-dfl


# delete empty data frames
dfl<-dfl[sapply(dfl, function(x) !is.null(nrow(x)))]


# create a variable denoting the volatility intervals
for (i in 1:length(dfl)) {
  dfl[[i]]["interv"]<-0
  dfl[[i]]<-dfl[[i]][is.na(dfl[[i]][colnames(dfl[[i]])[grepl("High",colnames(dfl[[i]]))]])==F&is.na(dfl[[i]][colnames(dfl[[i]])[grepl("Low",colnames(dfl[[i]]))]])==F,]
}


# identify all volatility intervals 
for (i in 1:length(dfl)) {
  if(is.na((max(dfl[[i]][1:nrow(dfl[[i]]),colnames(dfl[[i]])[grepl("High",colnames(dfl[[i]]))]], na.rm = T) -
            mean(dfl[[i]][1:nrow(dfl[[i]]),colnames(dfl[[i]])[grepl("Low",colnames(dfl[[i]]))]], na.rm = T)) /
           mean(dfl[[i]][1:nrow(dfl[[i]]),colnames(dfl[[i]])[grepl("Low",colnames(dfl[[i]]))]], na.rm = T))==T) {
    next
  }
  if((max(dfl[[i]][1:nrow(dfl[[i]]),colnames(dfl[[i]])[grepl("High",colnames(dfl[[i]]))]], na.rm = T) -
      mean(dfl[[i]][1:nrow(dfl[[i]]),colnames(dfl[[i]])[grepl("Low",colnames(dfl[[i]]))]], na.rm = T)) /
      mean(dfl[[i]][1:nrow(dfl[[i]]),colnames(dfl[[i]])[grepl("Low",colnames(dfl[[i]]))]], na.rm = T) >= fac) {
      lowme<-mean(dfl[[i]][1:nrow(dfl[[i]]),colnames(dfl[[i]])[grepl("Low",colnames(dfl[[i]]))]], na.rm = T)
      
        for (j in 1:nrow(dfl[[i]])) {
          if(j==1){
            dfl[[i]][j,"interv"]<-ifelse((dfl[[i]][j,colnames(dfl[[i]])[grepl("High",colnames(dfl[[i]]))]] - lowme) / lowme >= fac,-1,dfl[[i]][j,"interv"])
          }
          else {
            if(dfl[[i]][j-1,"interv"]!=0) {
              dfl[[i]][j,"interv"]<-ifelse((dfl[[i]][j,colnames(dfl[[i]])[grepl("High",colnames(dfl[[i]]))]] - lowme <=0) | 
                                             (dfl[[i]][j,colnames(dfl[[i]])[grepl("Low",colnames(dfl[[i]]))]] - lowme <=0),0,dfl[[i]][j-1,"interv"])
            }
            else {
              dfl[[i]][j,"interv"]<-ifelse((dfl[[i]][j,colnames(dfl[[i]])[grepl("High",colnames(dfl[[i]]))]] - lowme) / lowme >= fac,
                                           max(dfl[[i]][1:nrow(dfl[[i]]),"interv"])+1,dfl[[i]][j,"interv"])
            }
          }
    }
  }
}


# identify the stocks that fulfill the defined criteria for the minimum number of volatility intervals and their length and the number of days between them
dflsat<-list()
n<-1
for (i in 1:length(dfl)){
  if(max(dfl[[i]][1:nrow(dfl[[i]]),"interv"]) >= minvol){
    uniint<-unique(dfl[[i]][1:nrow(dfl[[i]]),"interv"])[unique(dfl[[i]][1:nrow(dfl[[i]]),"interv"])!=0]
    mindayvec<-rep(0,length(unique(dfl[[i]][1:nrow(dfl[[i]]),"interv"])[unique(dfl[[i]][1:nrow(dfl[[i]]),"interv"])!=0]))
    for (j in 1:length(mindayvec)){
      mindayvec[j]<-ifelse(max(dfl[[i]][dfl[[i]][1:nrow(dfl[[i]]),"interv"]==uniint[j],"date"], na.rm = T)-
                           min(dfl[[i]][dfl[[i]][1:nrow(dfl[[i]]),"interv"]==uniint[j],"date"], na.rm = T)>=minday,1,0)
    }
    minbetvec<-rep(0,length(unique(dfl[[i]][1:nrow(dfl[[i]]),"interv"])[unique(dfl[[i]][1:nrow(dfl[[i]]),"interv"])!=0]))
    for (j in 1:length(minbetvec)){
      minbetvec[j]<-ifelse(j==1&min(dfl[[i]][dfl[[i]][1:nrow(dfl[[i]]),"interv"]==uniint[j],"date"], na.rm = T)-
                             min(dfl[[i]][,"date"], na.rm = T)>=minbet,1,
                           ifelse(j>1&min(dfl[[i]][dfl[[i]][1:nrow(dfl[[i]]),"interv"]==uniint[j],"date"], na.rm = T)-
                                    max(dfl[[i]][dfl[[i]][1:nrow(dfl[[i]]),"interv"]==uniint[j-1],"date"], na.rm = T)>=minbet,1,0))
    }    
    if(sum(mindayvec)>=minvol&sum(minbetvec)>=minvol){
      dflsat[[n]]<-dfl[[i]]
      n<-n+1
    }
  }
}

rm(i,i,j,uniint,mindayvec,minbetvec)

# visual inspection
#ggplot(data= dflsat[[1]], mapping = aes(x = date, y=CPHI.High))+geom_point()
#ggplot(data= dflsat[[1]], mapping = aes(x = date, y=CPHI.High))+geom_line()

# select the stocks that on the maximum date are below the defined threshold
dflfin<-list()
n<-1
for (i in 1:length(dflsat)){
  if(dflsat[[i]][which.max(dflsat[[i]][,"date"]), colnames(dflsat[[i]])[grepl("Low",colnames(dflsat[[i]]))]]<
     mean(dflsat[[i]][i:nrow(dflsat[[i]]),colnames(dflsat[[i]])[grepl("Low",colnames(dflsat[[i]]))]], na.rm = T)){
    dflfin[[n]]<-dflsat[[i]]
    n<-n+1
  }
}

# print the identified volatile stocks
for (i in 1:length(dflfin)){
  print(strsplit(colnames(dflfin[[i]])[grepl("Low",colnames(dflfin[[i]]))],"[.]")[[1]][1])
}


# ── Laufzeit ausgeben ────────────────────────────────────────

# Calculate the total elapsed time by subtracting the start time from the current time
elapsed <- proc.time() - start_time
# Print the total runtime formatted as hours, minutes and seconds
cat(sprintf("\n Gesamtlaufzeit: %02d:%02d:%02d (hh:mm:ss)\n",
            # Calculate the number of full hours from the total elapsed seconds
            as.integer(elapsed["elapsed"]) %/% 3600,
            # Calculate the remaining minutes after subtracting full hours
            (as.integer(elapsed["elapsed"]) %% 3600) %/% 60,
            # Calculate the remaining seconds after subtracting full hours and minutes
            as.integer(elapsed["elapsed"]) %% 60
))


