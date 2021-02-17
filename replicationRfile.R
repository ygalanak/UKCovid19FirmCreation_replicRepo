#----
# UK COVID-19 Firm Creation
# Replication R file
# Yannis Galanakis; i.galanakis@kent.ac.uk
# February 2021; version 15022021
#----

# load libraries 
packages <- c('tidyverse', 'naniar', 'haven', 'survey',
              'data.table', 'lubridate', 'ggalt', 'cowplot','animation',
              'patchwork', 'sp', 'scales', 'raster', 'rgeos', 'mapproj',
              'rgdal', 'maptools', 'emojifont', 'nord', 'paletteer', 'plotly')
pkg_notinstall <-  packages[!(packages %in% installed.packages()[,"Package"])]

lapply(pkg_notinstall, install.packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)
if (!require(gpclib)) install.packages("gpclib", type="source");library(gpclib)
gpclibPermit()  # Gives maptool permisssion to use gpclib
load.fontawesome()

rm(list=ls())
#### DATA ####
#---
# Read Companies House company data and save 1% sample.
# Result is small, easy to view, csv.
#---

# Create a temp. file
temp <- tempfile()
# Use `download.file()` to fetch the file into the temp. file
download.file("http://download.companieshouse.gov.uk/BasicCompanyDataAsOneFile-2021-02-01.zip",temp)
# Use unz() to extract the target file from temp. file
repD<- read.csv(unz(temp, "BasicCompanyDataAsOneFile-2021-02-01.csv"))
# Remove the temp file via 'unlink()'
unlink(temp)
# Make incorporation date as date format.
repD$IncorporationDate <- as.Date(repD$IncorporationDate, "%d/%m/%Y")
# select firms registered in January 2021
repD2021 <- repD[which(repD$IncorporationDate >= "2021-01-01" &
                       repD$IncorporationDate <= "2021-01-31"), ]
# randomly select the 1% of the resulting Jan2021 file
repD2021 <- repD2021 %>% slice_sample(prop = 0.01)
# keep only relevant columns to Report
repD2021 <- repD2021[c(2,10:15,27:30)]
# save resulting file as .csv (to be uploaded on Website)
write_csv(repD2021,'output/replicData2021.csv')

# use 2019 as base year
# create data for Jan2019
repD2019 <- repD[which(repD$IncorporationDate >= "2019-01-01" &
                       repD$IncorporationDate <= "2019-01-31"), ]
repD2019 <- repD2019 %>% slice_sample(prop = 0.01)
repD2019 <- repD2019[c(2,10:15,27:30)]
# save resulting file as .csv (to be uploaded on Website)
write_csv(repD2019,'output/replicData2019.csv')
# remove initial big data frame
remove(repD)


### ANALYSIS ####
# Aggregate Analysis----


# Daily
# How many new firms by day? 
n_incorp <- repD2021 %>%
  group_by(IncorporationDate) %>%
  count()

# Rename IncorporationDate to day
n_incorp <- n_incorp %>% rename(day = IncorporationDate)
# what's the week day?
n_incorp$wkday <- weekdays(n_incorp$day, abbr = TRUE)
# change the max date here in new release!
total_days <- seq(ymd("2021-01-01"), ymd("2021-01-31"), by = "days") 
# Instead of dropping NAs we replace them with 0.
total_days <- tibble(total_days) %>%
  mutate(n = 0) %>%
  rename(day = total_days)

fp <- full_join(total_days, n_incorp, by = "day")
fp <- fp %>% mutate(ni = replace_na(n.y, 0))
fp <- tibble(date = fp$day, ni = fp$ni, day = fp$wkday)
write.csv(fp, "output/repByDay.csv")

# Basic dates
start_ldIII <- "2021-01-05"

# statistics
median_total <- median(fp$ni)
mean_total <- mean(fp$ni)
ldIII <- subset(fp, date >= start_ldIII)
median_during_ldIII <- median(ldIII$ni)
mean_during_ldIII <- mean(ldIII$ni)

#make table
avg <- matrix(c(median_total, median_during_ldIII,  
                mean_total, mean_during_ldIII),
              ncol=2,byrow=TRUE)
colnames(avg) <- c("Total","During Lockdown-III")
rownames(avg) <- c("Median", "Mean")

# Plot 1: Histogram with medians

dailyagg <- plot_ly(alpha = 0.5, y=fp$ni, x=fp$date, type = "bar", showlegend= F) 
dailyagg <- dailyagg %>% layout(xaxis=list(type='auto')) %>%
  add_segments(x = min(fp$date), xend= max(fp$date), y=median_total, yend=median_total, name = "Median (total)", showlegend=T) %>%
  add_segments(x = min(fp$date), xend= max(fp$date), y=median_during_ldIII, yend=median_during_ldIII, name = "Median (during lockdown III)", showlegend=T) %>%
  layout(title = 'Daily Company Registrations',
         yaxis = list(title = "Number of registrations", showgrid = F))

# Relative
#---

# week number in 2021
fp$week <- week(fp$date) 

# calculate the number of fimrs registered in January 2019
#2019
n_incorp2019 <- repD2019 %>%
  group_by(IncorporationDate) %>%
  count()
# replicate the data frame analysis of 2021
n_incorp2019 <- n_incorp2019 %>% rename(day = IncorporationDate)
n_incorp2019$wkday <- weekdays(n_incorp2019$day, abbr = TRUE)
n_incorp2019$week <- week(n_incorp2019$day)
total_days2019 <- seq(ymd("2019-01-01"), ymd("2019-01-31"), by = "days") 
total_days2019 <- tibble(total_days2019) %>%
  mutate(n = 0) %>%
  rename(day = total_days2019)

fp2019 <- full_join(total_days2019, n_incorp2019, by = "day")
fp2019 <- fp2019 %>% mutate(ni = replace_na(n.y, 0))
fp2019 <- tibble(date = fp2019$day, ni = fp2019$ni, day = fp2019$wkday, week = fp2019$week)

# Aggregate the firms per week
Week2019 <- aggregate(fp2019$ni, by=list(week=fp2019$week), FUN=sum)
Week2021 <- aggregate(fp$ni, by=list(week=fp$week), FUN=sum)

# Calculate the ratio: registrations2021/registrations2019
Week2021$ratioTo2019 <- Week2021$x/Week2019$x

# Plot 2: Ratio of registrations
RegRatio <- plot_ly(alpha = 0.5, y=Week2021$ratio, x = Week2021$week, type = 'scatter', mode = 'lines', name = "2021/2019") %>%
  add_segments(x=min(Week2021$week), xend = max(Week2021$week), y=1, yend=1, line = list(dash = "dash"), showlegend=F)
RegRatio <- RegRatio %>% layout(title = list(text = paste0('Company registrations Ratio',
                                                           '<br>',
                                                           '<sup>',
                                                           '(Same week of reference in 2019)',
                                                           '</sup>')),
                                xaxis = list(title= "Weeks", showgrid = F),
                                yaxis = list(title = "Ratio", showgrid = F))

# Differences in the same month over different years
mean_Jan2019 <- mean(fp2019$ni)
med_Jan2019  <- median(fp2019$ni)
mean_Jan2021 <- mean(fp$ni)
med_Jan2021 <- median(fp$ni)

# Plot 3: Plot the medians in the same month
JanChange <- plot_ly(x=c("2019", "2021"), y=c(med_Jan2019, med_Jan2021), alpha=0.5, name = "Median January Registrations", type='bar', showlegend=F)
JanChange <- JanChange %>%
  layout(title = "Daily median company registrations in January",
         xaxis = list(title = "", showgrid=F),
         yaxis = list(title = "", showgrid=F))

# Regioanl Analysis ----

# Postcode Area (FIRST 1 or 2 LETTERS)
## How many new firms by date and areas postcodes?

# keep only the first 1 or 2 letters before the numbers in the Postcode
repD2021$PostCodeArea <- sub("^([[:alpha:]]*).*", "\\1", repD2021$RegAddress.PostCode)
# calculate the number of firms per PC and date of registration
n_incorp4 <-repD2021 %>%
  group_by(IncorporationDate,PostCodeArea) %>%
  count()
n_incorp4 <- n_incorp4 %>% rename(day = IncorporationDate)
n_incorp4$wkday <- weekdays(n_incorp4$day, abbr = TRUE)
n_incorp4$week <- week(n_incorp4$day) 

# change the max date here in new release!
total_days4 <- seq(ymd("2021-01-01"), ymd("2021-01-31"), by = "days") 

total_days4 <- tibble(total_days4) %>%
  mutate(n = 0) %>%
  rename(day = total_days4)

fp2 <- full_join(total_days4, n_incorp4, by = "day")
fp2 <- fp2 %>% mutate(ni = replace_na(n.y, 0))
fp2 <- tibble(date = fp2$day, ni = fp2$ni, PostcodeArea = fp2$PostCodeArea,
                       wkday = fp2$wkday, week = fp2$week)
# file with longitude and latitude of postcodes
areaPC <- read.csv("input/Postcodes summaryCLEAN.csv")
fp2Total <- merge(fp2,areaPC,by="PostcodeArea")
write.csv(fp2Total, "output/ByDayPC.csv")

# perform the analysis for 2019
# keep only the first 1 or 2 letters before the numbers in the Postcode
repD2019$PostCodeArea <- sub("^([[:alpha:]]*).*", "\\1", repD2019$RegAddress.PostCode)

n_incorp4_2019 <-repD2019 %>%
  group_by(IncorporationDate,PostCodeArea) %>%
  count()
n_incorp4_2019 <- n_incorp4_2019 %>% rename(day = IncorporationDate)
n_incorp4_2019$wkday <- weekdays(n_incorp4_2019$day, abbr = TRUE)
n_incorp4_2019$week <- week(n_incorp4_2019$day) 

total_days4_2019 <- seq(ymd("2019-01-01"), ymd("2019-01-31"), by = "days") 

total_days4_2019 <- tibble(total_days4_2019) %>%
  mutate(n = 0) %>%
  rename(day = total_days4_2019)

full_period4_2019 <- full_join(total_days4_2019, n_incorp4_2019, by = "day")

full_period4_2019 <- full_period4_2019 %>% mutate(ni = replace_na(n.y, 0))

full_period4_2019 <- tibble(date = full_period4_2019$day, ni = full_period4_2019$ni, PostcodeArea = full_period4_2019$PostCodeArea,
                            wkday = full_period4_2019$wkday, week = full_period4_2019$week)

# Aggregate per week and postcode
mapWeek2021 <- aggregate(fp2Total$ni, 
                         by=list(week=fp2Total$week, PostcodeArea=fp2Total$PostcodeArea), 
                         FUN=sum)
mapWeek2019 <- aggregate(full_period4_2019$ni, 
                         by=list(week=full_period4_2019$week, PostcodeArea=full_period4_2019$PostcodeArea), 
                         FUN=sum)

mapWeek2021 <- mapWeek2021 %>% rename(x2021 = x)
mapWeek2019 <- mapWeek2019 %>% rename(x2019 = x)
# merge the 2 previous data frames with week and PC
mapWeek2021T <- merge(mapWeek2021, mapWeek2019, by=c("week","PostcodeArea"))
# calculate the percentage change between 2021 and 2019
mapWeek2021T$change <- (mapWeek2021T$x2021/mapWeek2021T$x2019 - 1) *100
# merge for longitude and latitude
mapWeek2021T <- merge(mapWeek2021T, areaPC,by="PostcodeArea")
# merge with countries
postcod2country <- read_csv("input/convertedPC2country.csv")
postcod2country <- postcod2country %>% rename (PostcodeArea = Postcode.area)
mapWeek2021T <- merge(mapWeek2021T, postcod2country, by="PostcodeArea")
# What to plot as MAP: Average change of January 2021 relative to January 2019 by Postcode Area
mapJan2021Ave <- mapWeek2021T %>%
  group_by(PostcodeArea) %>%
  summarise(avJan = mean(change))
## merge with countries
mapJan2021Ave <- merge(mapJan2021Ave, postcod2country, by="PostcodeArea")
mapJan2021Ave <- merge(mapJan2021Ave, areaPC, by="PostcodeArea")

# Plot 4a: Map with bubbles. Size of bubble denotes the perc. change 
# Download the Outline of the UK
UKareas <-  raster::getData('GADM', country='GBR', level = 3) %>%
  fortify()

aveMap <-
  ggplot() +
  geom_polygon(data = UKareas, 
               aes(long, lat, group = group), 
               fill="grey", alpha=0.4) +
  geom_point(data = mapJan2021Ave, 
             aes(x = Longitude, y = Latitude, 
                 size= avJan, label = Area.covered),  
             shape = 21, 
             fill = "steelblue", 
             alpha = .6) +
  ylim(50,59) + coord_map() +
  labs(title = "Average change of New Incorporations",
       subtitle = "Change measured between January 20201 and January 2019") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# country

# Define London areas
london <- c("Central London", "East London", 
            "North London", "South East London",
            "South West London", "West London")

# not include function
'%!in%' <- function(x,y)!('%in%'(x,y))

# only LDN
mapWeek2021TonlyLDN <- subset(mapWeek2021T, Area.covered %in% london)
# exclude LDN
mapWeek2021TnoLDN   <- subset(mapWeek2021T, Area.covered %!in% london)
# aggregate firm registration LDN by country
AggregateonlyLDN2021 <- aggregate(list(mapWeek2021TonlyLDN$x2021,mapWeek2021TonlyLDN$x2019),
                                     by=list(week=mapWeek2021TonlyLDN$week, 
                                             Country=mapWeek2021TonlyLDN$Country), 
                                     FUN=sum)
colnames(AggregateonlyLDN2021)[3] <- "Jan2021"
colnames(AggregateonlyLDN2021)[4] <- "Jan2019"
# calculate the percentage change
AggregateonlyLDN2021$change <- ((AggregateonlyLDN2021$Jan2021/AggregateonlyLDN2021$Jan2019)-1)*100
# replace country name to LDN
AggregateonlyLDN2021$Country <- sub("England", "London", AggregateonlyLDN2021$Country)

AggregateexclLDN2021 <- aggregate(list(mapWeek2021TnoLDN$x2021,mapWeek2021TnoLDN$x2019),
                                     by=list(week=mapWeek2021TnoLDN$week, 
                                             Country=mapWeek2021TnoLDN$Country), 
                                     FUN=sum)
colnames(AggregateexclLDN2021)[3] <- "Jan2021"
colnames(AggregateexclLDN2021)[4] <- "Jan2019"
# calculate the percentage change
AggregateexclLDN2021$change  <- ((AggregateexclLDN2021$Jan2021/AggregateexclLDN2021$Jan2019-1)*100)
# replace the name of the country to England (excl. LDN)
AggregateexclLDN2021$Country <- sub("England", "England (excl. Ldn)", AggregateexclLDN2021$Country)

# Plot 5: by country and LDN
Country <- plot_ly (alpha=0.5, x=AggregateexclLDN2021$week, 
                    y=AggregateexclLDN2021$change, 
                    type = 'scatter', mode = 'line', 
                    color = AggregateexclLDN2021$Country)
Country <- Country %>% 
  add_trace(y = AggregateonlyLDN2021$change, 
            x = AggregateonlyLDN2021$week, 
            name = 'London', type='scatter', mode = 'lines', 
            color = AggregateonlyLDN2021$Country)
Country <- Country %>% 
  layout(title = list(text = paste0('Change of company registrations by Country',
                                    '<br>',
                                    '<sup>',
                                    '(Same week of reference in 2019)',
                                    '</sup>')),
         xaxis = list(title= "Weeks", showgrid = F),
         yaxis = list(title = "% change", showgrid = F))


# Zoom in London
LondonByAreas2021 <- aggregate(list(mapWeek2021TonlyLDN$x2021,
                                    mapWeek2021TonlyLDN$x2019),
                                  by=list(week=mapWeek2021TonlyLDN$week, 
                                          AreaCovered=mapWeek2021TonlyLDN$Area.covered), 
                                  FUN=sum)
colnames(LondonByAreas2021)[3] <- "Jan2021"
colnames(LondonByAreas2021)[4] <- "Jan2019"
LondonByAreas2021$change <- ((LondonByAreas2021$Jan2021/LondonByAreas2021$Jan2019)-1)*100

# Plot 6: by London areas
LDN <- plot_ly (alpha=0.5, x=LondonByAreas2021$week, y=LondonByAreas2021$change, 
                type = 'scatter', mode = 'line', 
                color = LondonByAreas2021$AreaCovered)
LDN %>%
  layout(title = list(text = paste0('Change of company registrations in January by London Areas',
                                    '<br>',
                                    '<sup>',
                                    '(Same week of reference in 2019)',
                                    '</sup>')),
         xaxis = list(title= "Weeks", showgrid = F),
         yaxis = list(title = "% change", showgrid = F))

# Sectoral Analysis ----

# Keep only the code from SIC code text 1
repD2021$SIC5dg1<-  as.numeric(gsub("([0-9]+).*$", "\\1", repD2021$SICCode.SicText_1)) # pattern is by finding a set of numbers in the start and capturing them
# keep only the first 2 digits if more than 2
repD2021$SIC2dg1 <- repD2021$SIC5dg1/1000
# Drop decimals in SIC2digit 
repD2021$SIC2dg1 <- as.integer(repD2021$SIC2dg1)

# Convert SIC2dig to subsectors and sectors
# This table is retrieved, following this code: https://github.com/ygalanak/UKsicConverter
convert <- read.csv('input/sic2007conversion.csv')
convert <- convert %>% rename (Sector = Section)
repD2021Total <- merge(repD2021, convert, by="SIC2dg1")

# calculate how many new firms by day and sector? 
n_incorp3 <- repD2021Total %>%
  group_by(IncorporationDate, Sector) %>%
  count()
n_incorp3 <- n_incorp3 %>% rename(day = IncorporationDate)
n_incorp3$wkday <- weekdays(n_incorp3$day, abbr = TRUE)
n_incorp3$week <- week(n_incorp3$day) 

# change the max date here in new release!
total_days3 <- seq(ymd("2021-01-01"), ymd("2021-01-31"), by = "days") 

total_days3 <- tibble(total_days3) %>%
  mutate(n = 0) %>%
  rename(day = total_days3)

full_period3 <- full_join(total_days3, n_incorp3, by = "day")

full_period3 <- full_period3 %>% mutate(ni = replace_na(n.y, 0))

full_period3 <- tibble(date = full_period3$day, ni = full_period3$ni, Sector = full_period3$Sector,
                       wkday = full_period3$wkday, week = full_period3$week)
# merge with conversion file for the name of each section
full_period3Total <- merge(full_period3,convert[,c("Sector", "Section.name")],by="Sector") #merge for sector names
# Remove duplicates based on week columns
full_period3 <- full_period3 %>%
  distinct(Sector, date, week, .keep_all = TRUE)
write.csv(full_period3, "output/byDaySector.csv")

# replicate steps above for 2019
# Keep only the code from SIC
repD2019$SIC5dg1<-  as.numeric(gsub("([0-9]+).*$", "\\1", repD2019$SICCode.SicText_1)) # pattern is by finding a set of numbers in the start and capturing them
repD2019$SIC2dg1 <- repD2019$SIC5dg1/1000
# Drop decimals in SIC2digit 
repD2019$SIC2dg1 <- as.integer(repD2019$SIC2dg1)
repD2019Total <- merge(repD2019, convert, by="SIC2dg1")

# calculate how many new firms in Jan2019
n_incorp3_2019 <- repD2019Total %>%
  group_by(IncorporationDate, Sector) %>%
  count()
n_incorp3_2019 <- n_incorp3_2019 %>% rename(day = IncorporationDate)
n_incorp3_2019$wkday <- weekdays(n_incorp3_2019$day, abbr = TRUE)
n_incorp3_2019$week <- week(n_incorp3_2019$day) 

# change the max date here in new release!
total_days3_2019 <- seq(ymd("2019-01-01"), ymd("2019-01-31"), by = "days") 

total_days3_2019 <- tibble(total_days3_2019) %>%
  mutate(n = 0) %>%
  rename(day = total_days3_2019)

full_period3_2019 <- full_join(total_days3_2019, n_incorp3_2019, by = "day")

full_period3_2019 <- full_period3_2019 %>% mutate(ni = replace_na(n.y, 0))

full_period3_2019 <- tibble(date = full_period3_2019$day, ni = full_period3_2019$ni, 
                            wkday = full_period3_2019$wkday, Sector = full_period3_2019$Sector, week = full_period3_2019$week)
dffp3Total2019 <- merge(full_period3_2019,convert[,c("Sector", "Section.name")],by="Sector") #merge for sic names
# Remove duplicates based on week columns
dffp3Total2019 <- dffp3Total2019 %>%
  distinct(Sector, date, week, .keep_all = TRUE)

# aggregate by week and sic
regWeek2021 <- aggregate(full_period3$ni, 
                         by=list(week=full_period3$week, Sector=full_period3$Sector), 
                         FUN=sum)
regWeek2019 <- aggregate(dffp3Total2019$ni, 
                         by=list(week=dffp3Total2019$week, Sector=dffp3Total2019$Sector), 
                         FUN=sum)
regWeek2019<- regWeek2019 %>%
  group_by(Sector) %>% 
  arrange(week, .by_group = TRUE) 
# calculate percentage change
regWeek2021$change <- (regWeek2021$x-regWeek2019$x)/regWeek2019$x *100

regWeek2021 <- merge(regWeek2021, convert[,c("Sector", "Section.name")],by="Sector")
# Remove duplicates based on week columns
regWeek2021 <- regWeek2021 %>%
  distinct(Sector, week, .keep_all = TRUE)

# Plot 7: All sectors in January 2021/2019 
Sec <- plot_ly(alpha=0.5, x=regWeek2021$week, y=regWeek2021$change, 
                  color = regWeek2021$Section.name, 
               type = 'scatter', mode = 'line', showlegend =F)
Sec %>%
  layout(autosize = T, xaxis = list(showgrid = F),
         yaxis = list(title = "% change to 2019", showgrid = F),
         title = list(text = paste0('Change in company registrations',
                                    '<br>',
                                    '<sup>',
                                    '(Same week of reference in 2019)',
                                    '</sup>')))


# Daily registrations in a month by sector
DsecJan2021 <- aggregate(full_period3$ni, by=list(Sector=full_period3$Sector), FUN=sum)
colnames(DsecJan2021)[2] <- "Jan2021"
dffp3Jan2019 <- subset(dffp3Total2019, date >="2019-01-01" & date<="2019-01-31")
DsecJan2019 <- aggregate(dffp3Jan2019$ni, by=list(Sector=dffp3Jan2019$Sector), FUN=sum)
colnames(DsecJan2019)[2] <- "Jan2019"
DsecJan <- merge(DsecJan2021, DsecJan2019, by="Sector") 
DsecJan <- merge(DsecJan, convert[,c("Sector", "Section.name")],by="Sector")
# Remove duplicates based on week columns
DsecJan <- DsecJan %>%
  distinct(Sector, .keep_all = TRUE)
write.csv(DsecJan, "output/bySectorCompareALL.csv")

# Plot 8: Daily registrations by sector, January total
DSJan <- plot_ly(alpha = 0.5, x=DsecJan$Section.name, y=DsecJan$Jan2019, name = "2019", type = 'bar')
DSJan <- DSJan %>%
  add_trace(alpha = 0.5, x=DsecJan$Section.name, y=DsecJan$Jan2021, name = "2021", type = 'bar') %>%
  layout(autosize = T, xaxis = list(tickfont = list(size = 7), tickwidth=3,tickangle = 90),
         yaxis = list(title = "# of registations in January", showgrid = F),
         title = list(text = paste0('Daily company registrations in January by sector')))


# save workspace
save.image(file = "ReplicWorkspace.RData")

