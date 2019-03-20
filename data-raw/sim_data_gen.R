library(functform)
library(boot)
library(ggplot2)

dualsim <- NULL

# List of parameters
mean_param <- matrix(data=c(-4, -4, 12, -1.75, 3, 9,
                            -4, -4, 15, -1.75, 9, 3,
                            -3, -4, 12, -2, 3, 9,
                            -4, -3, 15, -2, 9, 3,
                            -4, -4, 15, -1.5, 4, 3), 
                     ncol=6, byrow=TRUE)


for(reg in 1:10){
  for(seas in 1:5){
    # Create mean function
    mu <- inv.logit(functform:::asg(x=1:30, 
                                    beta1=mean_param[seas, 1], 
                                    beta2=mean_param[seas, 2], 
                                    mu=mean_param[seas, 3],
                                    h=mean_param[seas, 4],
                                    sigma1=mean_param[seas, 5],
                                    sigma2=mean_param[seas, 6]))
    
    # Set the standard devs
    sd1 <- 0.01
    sd2 <- sd1*5
    
    # Arrange into a dataset
    tmp <- rbind(
      data.frame(week = 1:30, y = rnorm(n=30, mean=mu, sd=sd1), mu = mu, source = "1", region = reg, season = seas),
      data.frame(week = 1:30, y = rnorm(n=30, mean=mu, sd=sd2), mu = mu, source = "2", region = reg, season = seas)
    )
    
    dualsim <- rbind(tmp, dualsim)
  }
}


# Change values below zero to be above zero
dualsim$y[dualsim$y < 0] <- abs(dualsim$y[dualsim$y < 0])

# Add count data
dualsim$num <- sample(x=c(9900:10100), size=nrow(dualsim), replace=TRUE)
dualsim$count <- round(dualsim$y * dualsim$num)
dualsim$y <- dualsim$count/dualsim$num





# Plot the data
ggplot(data=dualsim) + 
  geom_point(aes(x=week, y=y, color=source)) + 
  geom_line(aes(x=week, y=mu)) + 
  facet_grid(season~region) +
  theme_bw()

ggplot(data=dualsim) + 
  geom_point(aes(x=week, y=count, color=source)) + 
  geom_line(aes(x=week, y=mu*size)) + 
  facet_grid(season~region) +
  theme_bw()

# Save the date
usethis::use_data(dualsim, overwrite = TRUE)
