# Compare the google flu data to cdcfluview data

library("gFluData")
library("fludata")
library("ggplot2")


cdcdat = data.frame(subset_data(season=c(2014:2016), forecast_week=0)$df, source="CDC")
googledat = subset(data.frame(googleFlu, source="Google"), season %in% c("14-15","15-16"))

p1 = ggplot() + geom_point(data=cdcdat, aes(x=week, y=ILIPer*6)) + facet_grid(Season~Region)
p2 = ggplot() + geom_point(data=googledat, aes(x=week, y=hit)) + facet_grid(season~region_num)

gridExtra::grid.arrange(p1, p2, ncol=2)

combd = rbind(
  data.frame(week=cdcdat$week, y=cdcdat$ILIPer*6, season=cdcdat$Season, region=cdcdat$Region, source="CDC"),
  data.frame(week=googledat$week, y=googledat$hit, season=googledat$season, region=googledat$region_num, source="Google")
)

ggplot(data=combd) + geom_point(aes(x=week, y=y, color=source)) + facet_grid(season~region) 

