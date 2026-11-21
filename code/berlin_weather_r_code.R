# loading the packages required
packagename<-c("rdwd","riem","meteospain","readxl","reshape2","ggplot2","leaflet","data.table")
if (length(setdiff(packagename, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packagename, rownames(installed.packages())),repos = "https://cran.uni-muenster.de/", Ncpus = 16)  
}

lapply(packagename, require, character.only = T)
rm(packagename)


#### load  and prepare the weather data for Malaga ----
# namespace of the package
ns <- asNamespace("meteospain")

# unlock the internal function that caused a cache error
if (exists(".get_cached_result", envir = ns)) {
  unlockBinding(".get_cached_result", ns)
}

# overwrite the internal function 
assign(".get_cached_result", function(cache_ref, expr) {
  return(expr)
}, envir = ns)

# Red de Información Agroclimática de Andalucía
riadf<-get_stations_info_from(service = "ria") # Malaga station id 29-1

riadat<-get_meteo_from(service = 'ria', ria_options(resolution="daily",
                                                    start_date = as.Date('2000-01-01'),
                                                    end_date = as.Date('2025-12-31'),
                                                    stations='29-1'))

# Malaga only has data from October 2000 onwards so we delete the year 2000 from the analysis
riadat<-riadat[riadat$timestamp>="2001-01-01",]


riadat$ye<-year(riadat$timestamp)
aggregate(timestamp~ye,riadat,NROW)
# Malaga has many years with missing dates but only in 2001 more than 5 days are missing

# exclusing the year 2001 from the malage data due to missing values
riadat<-riadat[riadat$ye!="2001",]


#### load  and prepare the weather data for Berlin ----
bt<-dataDWD(selectDWD(name="Berlin-Tempelhof", res = "daily", var = "kl", per = "historical"))
max(bt$MESS_DATUM) # 2025-12-31
bt<-bt[bt$MESS_DATUM>="2000-01-01",]

bt$ye<-as.numeric(format(bt$MESS_DATUM, "%Y"))


#### load  and prepare the weather data for Gothenburg ----
# load the temperature data
pathtodata<-paste0(getwd(),"/data")
gt<-read.csv(paste0(pathtodata,"/goeteborg_a_temperature.csv"), sep=";",skip = 14)

# create a date variable
gt$date<-as.Date(gt$Datum)

# select the desired time period
gt<-gt[gt$date>="2000-01-01"&gt$date<="2025-12-31",]

# aggregate the hourly temperature data to the maximum temperature of each day
gtd<-aggregate(Lufttemperatur~Datum,gt,max)
gtd$ye<-substr(gtd$Datum,1,4)
gtd$date<-as.Date(gtd$Datum)

aggregate(Datum~ye,gtd,NROW) # 20 days missing in 2013

# excluding the year 2013 from the analysis due to missing data
gtd<-gtd[gtd$ye!="2013",]


# load the precipitation data
gp<-read.csv(paste0(pathtodata,"/goeteborg_a_precipitation.csv"), sep=";", skip=14)

# create a date variable
gp$date<-as.Date(gp$Från.Datum.Tid..UTC.)

# select the desired time period
gp<-gp[gp$date>="2000-01-01"&gp$date<="2025-12-31",]

gp$ye<-substr(gp$Från.Datum.Tid..UTC.,1,4)


#### load  and prepare the weather data for Zagreb ----
# load the precipitation data for Zagreb
zgp<-read_excel(path=paste0(pathtodata,"/zagreb.xlsx"), sheet = "precipitation", skip = 3)

#deleting not needed rows and columns
zgp<-zgp[is.na(zgp$'2000')==FALSE,c(1:13)]

# creating a year variable
zgp$ye<-cumsum(zgp$'2000'==1) + 1999

# deleting not needed rows
zgp<-zgp[zgp$'2000'<2000,]

# melting the months columns into one column
zgp<-reshape2::melt(zgp, id.vars = c("ye","2000"), measure.vars = c("1","2","3","4","5","6","7","8","9","10","11","12"))

# creating a date variable
zgp$date<-as.Date(paste(zgp$ye, zgp$variable, zgp$'2000', sep="-"))

# transform the NA values to 0 as NA means that no precipitation was measured
zgp$precipitation<-ifelse(is.na(zgp$value)==TRUE,0,zgp$value)

# load the temperature data for Zagreb
zgt<-read_excel(path=paste0(pathtodata,"/zagreb.xlsx"), sheet = "maximum temp", skip = 2)

#deleting not needed rows and columns
zgt<-zgt[is.na(zgt$'2000')==FALSE,c(1:13)]

# creating a year variable
zgt$ye<-cumsum(zgt$'2000'==1) + 1999

# deleting not needed rows
zgt<-zgt[zgt$'2000'<2000,]

# melting the months columns into one column
zgt<-reshape2::melt(zgt, id.vars = c("ye","2000"), measure.vars = colnames(zgt)[2:13])

# transforming the roman into arabic numbers
zgt$mon<-as.numeric(zgt$variable)

# creating a date variable
zgt$date<-as.Date(paste(zgt$ye, zgt$mon, zgt$'2000', sep="-"))

# transform the NA values to 0 as NA means that no precipitation was measured
zgt$maxtemp<-zgt$value


#### load  and prepare the weather data for Dublin ----
# load the data
dub<-read.csv(paste0(pathtodata,"/dublin.csv"), skip = 13)

# creating a date variable
table(is.na(dub$date)) # no missing values in the date variable
dub$mon<-substr(dub$date,4,6)
mo2Num <- function(x) match(tolower(x), tolower(month.abb))
dub$mon<-mo2Num(dub$mon)
table(is.na(dub$mon)) # no missing values in the month variable

dub$ye<-substr(dub$date,8,11)

dub$da<-substr(dub$date,1,2)

# adjust the format of the date variable
dub$date<-as.Date(paste(dub$ye, dub$mon, dub$da, sep="-"))
table(is.na(dub$date)) # no missing values in the date variable

# select the desired time period
dub<-dub[dub$date>="2000-01-01"&dub$date<="2025-12-31",]
aggregate(da~ye,dub,NROW) # 61 missing days in 2020
# excluding the year 2020 from the data due to missing values
dub<-dub[dub$ye!="2020",]


#### merge all data into one data set ----
# prepare the individual data sets
bt$station<-1 # 1 for Berlin
bt$date<-bt$MESS_DATUM
bt$maxtemp<-bt$TXK
bt$precipitation<-bt$RSK

dub$station<-2 # 2 for Dublin
dub$maxtemp<-dub$maxt
dub$precipitation<-dub$rain

gp$precipitation<-gp$Nederbördsmängd 
gtd$maxtemp<-gtd$Lufttemperatur
gpt<-merge(gp[,c("date","ye","precipitation")],gtd[,c("date","maxtemp")], by= "date", all.x=TRUE)
gpt$station<-3 # 3 for Gothenburg

riadat$station<-4 # 4 for Malaga
riadat$date<-as.Date(riadat$timestamp)
riadat$maxtemp<-riadat$max_temperature
rdat<-as.data.frame(riadat)

zgt<-zgt[is.na(zgt$date)==F,]
zgp<-zgp[is.na(zgp$date)==F,]
zg<-merge(zgp[,c("date","precipitation","ye")], zgt[,c("date","maxtemp")], by="date", all.x = T)
zg$station<-5 # 5 for Zagreb
zg$maxtemp<-as.numeric(zg$maxtemp)

# bind all data sets into one
alldat<-do.call("rbind",list(bt[,c("date","station","maxtemp","precipitation","ye")],
              dub[,c("date","station","maxtemp","precipitation","ye")],
              gpt[,c("date","station","maxtemp","precipitation","ye")],
              rdat[,c("date","station","maxtemp","precipitation","ye")],
              zg[,c("date","station","maxtemp","precipitation","ye")]))

# create a factor variable for the locations
alldat$station_fac<-factor(alldat$station, levels = c(1:5), labels = c("Berlin","Dublin","Gothenburg","Malaga","Zagreb"))

# transform the year variable into numeric
alldat$ye<-as.numeric(alldat$ye)


#### defining the colours for the different cities ----
my_colors <- c("red2", "forestgreen", "dodgerblue2", "darkmagenta", "blue4")
# lightskyblue1 for gothenburg
# dodgerblue2 for zagreb
# red2 for berlin
# forestgreen for dublin
# darkmagenta for malaga



#### visualization of the sum of precipitation per year----
dfprecsum<-aggregate(precipitation~ye+station,alldat,sum)

# create a factor variable for the locations
dfprecsum$station_fac<-factor(dfprecsum$station, levels = c(1:5), labels = c("Berlin","Dublin","Gothenburg","Malaga","Zagreb"))

# create a line chart
ggplot(dfprecsum, aes(x = ye, y = precipitation, colour = station_fac,
                   group = station_fac)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(x = "\nYear", y = "Sum of precipitation (in mm)\n",
       colour = "City") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+
  scale_colour_manual(values = my_colors)



#### visualization of the average sum of precipitation per year----
dfprecmean<-aggregate(precipitation~station,dfprecsum,mean)

# create a factor variable for the locations
dfprecmean$station_fac<-factor(dfprecmean$station, levels = c(1:5), labels = c("Berlin","Dublin","Gothenburg","Malaga","Zagreb"))

# create a bar chart
ggplot(dfprecmean, aes(x = reorder(station_fac, -precipitation), y = precipitation, fill = station_fac)) +
  geom_col() +
  labs(x = "\nCity", y = "Average sum of precipitation per year (in mm)\n") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+ guides(fill="none")+
  scale_fill_manual(values = my_colors)


#### visualization of days with precipitation ----
dfprecday<-aggregate(precipitation~ye+station,alldat,FUN = function(x) sum(x >= 1, na.rm = TRUE))

# create a factor variable for the locations
dfprecday$station_fac<-factor(dfprecday$station, levels = c(1:5), labels = c("Berlin","Dublin","Gothenburg","Malaga","Zagreb"))

# create a line chart
ggplot(dfprecday, aes(x = ye, y = precipitation, colour = station_fac,
                   group = station_fac)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(x = "\nYear", y = "Number of days with rain\n",
       colour = "City") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+
  scale_fill_manual(values = my_colors)

#### visualization of the average number of days with rain per year----
dfprecdaymean<-aggregate(precipitation~station,dfprecday,mean)

# create a factor variable for the locations
dfprecdaymean$station_fac<-factor(dfprecdaymean$station, levels = c(1:5), labels = c("Berlin","Dublin","Gothenburg","Malaga","Zagreb"))

# create a bar chart
ggplot(dfprecdaymean, aes(x = reorder(station_fac, -precipitation), y = precipitation, fill = station_fac)) +
  geom_col() +
  labs(x = "\nCity", y = "Average number of days with rain per year\n") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+ guides(fill="none")+
  scale_fill_manual(values = my_colors)


#### visualization of days with heavy rain ----
dfprecdayheav<-aggregate(precipitation~ye+station,alldat,FUN = function(x) sum(x >= 20, na.rm = TRUE))

# create a factor variable for the locations
dfprecdayheav$station_fac<-factor(dfprecdayheav$station, levels = c(1:5), labels = c("Berlin","Dublin","Gothenburg","Malaga","Zagreb"))

# create a line chart
ggplot(dfprecdayheav, aes(x = ye, y = precipitation, colour = station_fac,
                      group = station_fac)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(x = "\nYear", y = "Number of days with heavy rain\n",
       colour = "City") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+
  scale_fill_manual(values = my_colors)

#### visualization of the average number of days with heavy rain per year----
dfprecdayheavmean<-aggregate(precipitation~station,dfprecdayheav,mean)

# create a factor variable for the locations
dfprecdayheavmean$station_fac<-factor(dfprecdayheavmean$station, levels = c(1:5), labels = c("Berlin","Dublin","Gothenburg","Malaga","Zagreb"))

# create a bar chart
ggplot(dfprecdayheavmean, aes(x = reorder(station_fac, -precipitation), y = round(precipitation), fill = station_fac)) +
  geom_col() +
  labs(x = "\nCity", y = "Average number of days with heav rain per year\n") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)), breaks = seq(0, 12, by = 2))+ 
  guides(fill="none")+
  scale_fill_manual(values = my_colors)



#### visualization of the average maximum daily temperature ----
# create a data set without the 29 February
alldatwtnf<-alldat[format(alldat$date,format = "%d %b")!="29 Feb",]

# create the number of the day of the year from the date variable
alldatwtnf$dny<-as.numeric(format(alldatwtnf$date,format = "%j"))

# calculate the mean daily maximum temperature
dfmeanmaxtemp<-aggregate(maxtemp~dny+station,alldatwtnf,mean)

# create a factor variable for the locations
dfmeanmaxtemp$station_fac<-factor(dfmeanmaxtemp$station, levels = c(1:5), labels = c("Berlin","Dublin","Gothenburg","Malaga","Zagreb"))

# create a line chart
ggplot(dfmeanmaxtemp, aes(x = dny, y = maxtemp, colour = station_fac,
                          group = station_fac)) +
  geom_line(linewidth = 1) +
  #geom_point(size = 3) +
  labs(x = "\nDay of the year", y = "Average maximum daily temperature (in °C)\n",
       colour = "City") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+
  scale_colour_manual(values = my_colors) +
  geom_vline(xintercept = c(1, 92, 183, 275, 365), linetype = "dashed", color = "ivory4", size = 1) +
  scale_x_continuous(
    breaks = c(1, 92, 183, 275, 365),
    labels = c("1st of\nJanuary", "1st of\nApril", "1st of\nJuly", "1st of\nOctober", "31st of\nDecember")
  )


#### visualization of the average maximum daily temperature per month ----
# create a month variable
alldat$mon<-month(alldat$date)

# calculate the mean daily maximum temperature
dfmeanmaxtempmon<-aggregate(maxtemp~mon+station,aggregate(maxtemp~mon+station+ye,alldat,mean),mean)

# create a factor variable for the locations
dfmeanmaxtempmon$station_fac<-factor(dfmeanmaxtempmon$station, levels = c(1:5), labels = c("Berlin","Dublin","Gothenburg","Malaga","Zagreb"))

# create a line chart
ggplot(dfmeanmaxtempmon, aes(x = mon, y = maxtemp, colour = station_fac,
                          group = station_fac)) +
  geom_line(linewidth = 1) +
  #geom_point(size = 3) +
  labs(x = "\nMonth of the year", y = "Average maximum daily temperature\nper month (in °C)\n",
       colour = "City") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+
  scale_colour_manual(values = my_colors) +
  geom_vline(xintercept = c(1, 4, 7, 10, 12), linetype = "dashed", color = "ivory4", size = 1) +
  scale_x_continuous(
    breaks = c(1, 4, 7, 10, 12),
    labels = c("January", "April", "July", "October", "December")
  )

dfmeanmaxtempmon

# difference between smallest and largest average daily maximum temperature
max(dfmeanmaxtemp$maxtemp[dfmeanmaxtemp$station==1])-min(dfmeanmaxtemp$maxtemp[dfmeanmaxtemp$station==1])
max(dfmeanmaxtemp$maxtemp[dfmeanmaxtemp$station==2])-min(dfmeanmaxtemp$maxtemp[dfmeanmaxtemp$station==2])
max(dfmeanmaxtemp$maxtemp[dfmeanmaxtemp$station==3])-min(dfmeanmaxtemp$maxtemp[dfmeanmaxtemp$station==3])
max(dfmeanmaxtemp$maxtemp[dfmeanmaxtemp$station==4])-min(dfmeanmaxtemp$maxtemp[dfmeanmaxtemp$station==4])
max(dfmeanmaxtemp$maxtemp[dfmeanmaxtemp$station==5])-min(dfmeanmaxtemp$maxtemp[dfmeanmaxtemp$station==5])


#### visualization of summer days ----
dfsumd<-aggregate(maxtemp~ye+station,alldat,FUN = function(x) sum(x >= 25, na.rm = TRUE))

# create a factor variable for the locations
dfsumd$station_fac<-factor(dfsumd$station, levels = c(1:5), labels = c("Berlin","Dublin","Gothenburg","Malaga","Zagreb"))

# create a line chart
ggplot(dfsumd, aes(x = ye, y = maxtemp, colour = station_fac,
                      group = station_fac)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(x = "\nYear", y = "Number of summer days\n",
       colour = "City") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+
  scale_fill_manual(values = my_colors)


#### visualization of the average number of summer days per year----
dfsumdmean<-aggregate(maxtemp~station,dfsumd,mean)

# create a factor variable for the locations
dfsumdmean$station_fac<-factor(dfsumdmean$station, levels = c(1:5), labels = c("Berlin","Dublin","Gothenburg","Malaga","Zagreb"))

# create a bar chart
ggplot(dfsumdmean, aes(x = reorder(station_fac, -maxtemp), y = round(maxtemp), fill = station_fac)) +
  geom_col() +
  labs(x = "\nCity", y = "Average number of summer days per year\n") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+ guides(fill="none")+
  scale_fill_manual(values = my_colors)


#### visualization of heat days ----
dfhd<-aggregate(maxtemp~ye+station,alldat,FUN = function(x) sum(x >= 30, na.rm = TRUE))

# create a factor variable for the locations
dfhd$station_fac<-factor(dfhd$station, levels = c(1:5), labels = c("Berlin","Dublin","Gothenburg","Malaga","Zagreb"))

# create a line chart
ggplot(dfhd, aes(x = ye, y = maxtemp, colour = station_fac,
                   group = station_fac)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(x = "\nYear", y = "Number of heat days\n",
       colour = "City") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+
  scale_fill_manual(values = my_colors)


#### visualization of the average number of heat days per year----
dfhdmean<-aggregate(maxtemp~station,dfhd,mean)

# create a factor variable for the locations
dfhdmean$station_fac<-factor(dfhdmean$station, levels = c(1:5), labels = c("Berlin","Dublin","Gothenburg","Malaga","Zagreb"))

# create a bar chart
ggplot(dfhdmean, aes(x = reorder(station_fac, -maxtemp), y = round(maxtemp), fill = station_fac)) +
  geom_col() +
  labs(x = "\nCity", y = "Average number of heat days per year\n") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))+ guides(fill="none")+
  scale_fill_manual(values = my_colors)


#### visualization of the weather stations' locations
# get the coordinates for each station
malaga_coor<-riadf$geometry[which(riadf$station_id=="29-1")]
dublin_coor<-read.csv(paste0(pathtodata,"/dublin.csv"), skip = 1, nrows = 2)
dublin_coor<-c(gsub("Latitude.","",row.names(dublin_coor)[1]), gsub("Longitude.","",dublin_coor$'Station.Height..18.M'[1]))
dublin_coor<-as.numeric(dublin_coor)
berlin_coor <- metaInfo(findID("Berlin-Tempelhof", exactmatch = FALSE))
berlin_coor<-c(berlin_coor$geoBreite[1], berlin_coor$geoLaenge[1])
gothenburg_coor<-read.csv(paste0(pathtodata,"/goeteborg_a_precipitation.csv"),skip=12,nrows=1)
gothenburg_coor<-c(substr(colnames(gothenburg_coor),nchar(colnames(gothenburg_coor))-14,nchar(colnames(gothenburg_coor))-8),
                   substr(colnames(gothenburg_coor),nchar(colnames(gothenburg_coor))-6,nchar(colnames(gothenburg_coor))))
gothenburg_coor<-as.numeric(gothenburg_coor)
zagreb_coor<-c(45.82212656307635, 16.033440853711223)

# function to transfrom colour names into a RGB-Matrix
r_colors <- function(names) {
  rgb_mat <- col2rgb(names)
  rgb(rgb_mat[1, ], rgb_mat[2, ], rgb_mat[3, ], maxColorValue = 255)
}

r_colors(my_colors)

map <- leaflet() %>%
  addTiles() %>%  # add the standard openstreetmap
  setView(lng = 4, lat = 45, zoom = 3) %>% # sets the focus on europe
  addCircleMarkers(
    lng = c(berlin_coor[2],dublin_coor[2],gothenburg_coor[2],malaga_coor[[1]][1],zagreb_coor[2]), 
    lat = c(berlin_coor[1],dublin_coor[1],gothenburg_coor[1],malaga_coor[[1]][2],zagreb_coor[1]), 
    radius = 2,          # set the size of the dot marker
    color = r_colors(my_colors),       # colour of the marker
    fillColor = r_colors(my_colors),   # colour of the markers filling
    fillOpacity = 2,   # opacity of the filling
    popup = c("Berlin","Dublin","Gothenburg","Malaga","Zagreb")
  )
  
map

