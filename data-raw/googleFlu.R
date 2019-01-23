library("dplyr")
library("tidyr")
library("ggplot2")
library("gtrendsR")
library("MMWRweek")

# Grab the individual state codes
stateCodes <- as.character(unique(subset(gtrendsR::countries, country_code=="US")$sub_code)[2:52])

# Create overall dataframe
res <- NULL
for(st in stateCodes){
  print(st)
  tmp <- gtrends(c("flu", "influenza"), geo = st, low_search_volume = TRUE)$interest_over_time
  tmp$stateName <- sapply(strsplit(st,"-"), `[`, 2)
  res <- rbind(res, tmp)
}

# Get MMWR Week of Date
res <- cbind(res, MMWRweek(res$date))
res <- res %>% 
  mutate(week = MMWRweek,
         year = MMWRyear) %>%
  select(-MMWRweek, -MMWRyear, -MMWRday)

# Change hits data to numeric
res$hits[res$hits == "<1"] <- "0"
res$hits <- as.numeric(res$hits)

# Remove weeks we don't need
res <- res[-which(res$week > 20 & res$week < 40), ]

# Create a season variable
res$season <- rep("14-15", nrow(res))

#2014-2015
res[which(res$week >= 40 & res$week <= 53 & res$year == 2014), "season"] <- "14-15"
res[which(res$week <= 39 & res$week > 0   & res$year == 2015), "season"] <- "14-15"
#2015-2016
res[which(res$week >= 40 & res$week <= 53 & res$year == 2015), "season"] <- "15-16"
res[which(res$week <= 39 & res$week >= 0  & res$year == 2016), "season"] <- "15-16"
#2016-2017
res[which(res$week >= 40 & res$week <= 53 & res$year == 2016), "season"] <- "16-17"
res[which(res$week <= 39 & res$week >= 0  & res$year == 2017), "season"] <- "16-17"
#2017-2018
res[which(res$week >= 40 & res$week <= 53 & res$year == 2017), "season"] <- "17-18"
res[which(res$week <= 39 & res$week >= 0  & res$year == 2018), "season"] <- "17-18"
#2018-2019
res[which(res$week >= 40 & res$week <= 53 & res$year == 2018), "season"] <- "18-19"
res[which(res$week <= 39 & res$week >= 0  & res$year == 2019), "season"] <- "18-19"
#2019-2020
res[which(res$week >= 40 & res$week <= 53 & res$year == 2019), "season"] <- "19-20"
res[which(res$week <= 39 & res$week >= 0  & res$year == 2020), "season"] <- "19-20"

res$season <- as.factor(res$season)

# Reorder the weeks
for(i in 1:nlevels(res$season)){
  if(sum(res[res$season==levels(res$season)[i],"week"]==53)!=0){
    # Assign week 40:52 to 1:13
    # Assign week 1:20 to 14:33
    res[which(res$season==levels(res$season)[i] & res$week==14),"week"] <- 28
    res[which(res$season==levels(res$season)[i] & res$week==15),"week"] <- 29
    res[which(res$season==levels(res$season)[i] & res$week==16),"week"] <- 30
    res[which(res$season==levels(res$season)[i] & res$week==17),"week"] <- 31
    res[which(res$season==levels(res$season)[i] & res$week==18),"week"] <- 32
    res[which(res$season==levels(res$season)[i] & res$week==19),"week"] <- 33
    res[which(res$season==levels(res$season)[i] & res$week==20),"week"] <- 34
    
    res[which(res$season==levels(res$season)[i] & res$week==1),"week"]  <- 15
    res[which(res$season==levels(res$season)[i] & res$week==2),"week"]  <- 16
    res[which(res$season==levels(res$season)[i] & res$week==3),"week"]  <- 17
    res[which(res$season==levels(res$season)[i] & res$week==4),"week"]  <- 18
    res[which(res$season==levels(res$season)[i] & res$week==5),"week"]  <- 19
    res[which(res$season==levels(res$season)[i] & res$week==6),"week"]  <- 20
    res[which(res$season==levels(res$season)[i] & res$week==7),"week"]  <- 21
    res[which(res$season==levels(res$season)[i] & res$week==8),"week"]  <- 22
    res[which(res$season==levels(res$season)[i] & res$week==9),"week"]  <- 23
    res[which(res$season==levels(res$season)[i] & res$week==10),"week"] <- 24
    res[which(res$season==levels(res$season)[i] & res$week==11),"week"] <- 25
    res[which(res$season==levels(res$season)[i] & res$week==12),"week"] <- 26
    res[which(res$season==levels(res$season)[i] & res$week==13),"week"] <- 27
    
    res[which(res$season==levels(res$season)[i] & res$week==40),"week"] <- 1
    res[which(res$season==levels(res$season)[i] & res$week==41),"week"] <- 2
    res[which(res$season==levels(res$season)[i] & res$week==42),"week"] <- 3
    res[which(res$season==levels(res$season)[i] & res$week==43),"week"] <- 4
    res[which(res$season==levels(res$season)[i] & res$week==44),"week"] <- 5
    res[which(res$season==levels(res$season)[i] & res$week==45),"week"] <- 6
    res[which(res$season==levels(res$season)[i] & res$week==46),"week"] <- 7
    res[which(res$season==levels(res$season)[i] & res$week==47),"week"] <- 8
    res[which(res$season==levels(res$season)[i] & res$week==48),"week"] <- 9
    res[which(res$season==levels(res$season)[i] & res$week==49),"week"] <- 10
    res[which(res$season==levels(res$season)[i] & res$week==50),"week"] <- 11
    res[which(res$season==levels(res$season)[i] & res$week==51),"week"] <- 12
    res[which(res$season==levels(res$season)[i] & res$week==52),"week"] <- 13
    res[which(res$season==levels(res$season)[i] & res$week==53),"week"] <- 14
  }else{
    # Assign week 40:52 to 1:13
    # Assign week 1:20 to 14:33
    res[which(res$season==levels(res$season)[i] & res$week==14),"week"] <- 27
    res[which(res$season==levels(res$season)[i] & res$week==15),"week"] <- 28
    res[which(res$season==levels(res$season)[i] & res$week==16),"week"] <- 29
    res[which(res$season==levels(res$season)[i] & res$week==17),"week"] <- 30
    res[which(res$season==levels(res$season)[i] & res$week==18),"week"] <- 31
    res[which(res$season==levels(res$season)[i] & res$week==19),"week"] <- 32
    res[which(res$season==levels(res$season)[i] & res$week==20),"week"] <- 33
    
    res[which(res$season==levels(res$season)[i] & res$week==1),"week"]  <- 14
    res[which(res$season==levels(res$season)[i] & res$week==2),"week"]  <- 15
    res[which(res$season==levels(res$season)[i] & res$week==3),"week"]  <- 16
    res[which(res$season==levels(res$season)[i] & res$week==4),"week"]  <- 17
    res[which(res$season==levels(res$season)[i] & res$week==5),"week"]  <- 18
    res[which(res$season==levels(res$season)[i] & res$week==6),"week"]  <- 19
    res[which(res$season==levels(res$season)[i] & res$week==7),"week"]  <- 20
    res[which(res$season==levels(res$season)[i] & res$week==8),"week"]  <- 21
    res[which(res$season==levels(res$season)[i] & res$week==9),"week"]  <- 22
    res[which(res$season==levels(res$season)[i] & res$week==10),"week"] <- 23
    res[which(res$season==levels(res$season)[i] & res$week==11),"week"] <- 24
    res[which(res$season==levels(res$season)[i] & res$week==12),"week"] <- 25
    res[which(res$season==levels(res$season)[i] & res$week==13),"week"] <- 26
    
    res[which(res$season==levels(res$season)[i] & res$week==40),"week"] <- 1
    res[which(res$season==levels(res$season)[i] & res$week==41),"week"] <- 2
    res[which(res$season==levels(res$season)[i] & res$week==42),"week"] <- 3
    res[which(res$season==levels(res$season)[i] & res$week==43),"week"] <- 4
    res[which(res$season==levels(res$season)[i] & res$week==44),"week"] <- 5
    res[which(res$season==levels(res$season)[i] & res$week==45),"week"] <- 6
    res[which(res$season==levels(res$season)[i] & res$week==46),"week"] <- 7
    res[which(res$season==levels(res$season)[i] & res$week==47),"week"] <- 8
    res[which(res$season==levels(res$season)[i] & res$week==48),"week"] <- 9
    res[which(res$season==levels(res$season)[i] & res$week==49),"week"] <- 10
    res[which(res$season==levels(res$season)[i] & res$week==50),"week"] <- 11
    res[which(res$season==levels(res$season)[i] & res$week==51),"week"] <- 12
    res[which(res$season==levels(res$season)[i] & res$week==52),"week"] <- 13
  }
}

# Create region variable
regions <- cdcfluview::hhs_regions
regions$stateName <- state.abb[match(regions$state_or_territory,state.name)]
regions[is.na(regions$stateName),"stateName"][3] <- c("DC")

res <- left_join(res, regions) %>%
  select(-gprop, -geo, -stateName, -year, -category, -regional_office, -state_or_territory, -date)

# Aggregate across keywords
googleFlu <- res %>%
  group_by(week, season, region) %>%
  summarize(
    hit = mean(hits),
    region_num = unique(region_number)
  )

# Save the data
devtools::use_data(googleFlu, overwrite = TRUE)

ggplot(res) + 
  geom_point(aes(x=week, y=hit)) + 
  theme_bw() + labs(y="Search Hits", x="Date") +
  facet_grid(season~region)
