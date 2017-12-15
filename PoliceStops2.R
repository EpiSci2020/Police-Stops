#stop and frisk data (with noise added to protect confidentiality)
#precincts are numbered 1-75
#ethnicity 1=black, 2=hispanic, 3=white
#crime type 1=violent, 2=weapons, 3=property, 4=drug

devtools::install_github("rmcelreath/rethinking", force = TRUE)
library(rethinking)
library(ggplot2)
setwd("D:/Regression Gelman Book/PoliceStops")
cop <- read.csv("D:/Regression Gelman Book/PoliceStops/PoliceStops.txt",
                sep="", stringsAsFactors=FALSE)
str(cop)
colnames(cop) <- c("stops", "pop", "arrests", "precinct", "eth", "crime")
cop$pop <- scale(cop$pop)
cop$arrests <- scale(cop$arrests)

ggplot() +
  
  geom_boxplot(aes(x = as.factor(eth), y = stops), data = cop)
m1 <-glm(stops ~ 1, data = cop, family = "poisson")
summary(m1)
precis(m1)

m2 <-glm(stops ~ factor(eth), data = cop, family = "poisson")
summary(m2)

m3 <-glm(stops ~ factor(eth) + arrests, data = cop, family = "poisson")
summary(m3) 

m4 <-glm(stops ~ factor(eth) + arrests + crime, data = cop, family = "poisson")
summary(m4)

m5 <-glm(stops ~ factor(eth) + arrests + crime + pop, data = cop, family = "poisson")
summary(m5)

m6 <-glm(stops ~ factor(eth) + arrests + crime + pop + factor(precinct),
         data = cop, family = "poisson")
summary(m6)

#same as m44 in stan
m7 <-glm(stops ~ factor(eth) + crime + pop + arrests,
         data = cop, family = "poisson")
summary(m7)
AIC(m1, m2, m3, m4, m5, m6)

#*******************************************************
m11 <- map2stan(
  alist(
    stops ~ dpois(lambda),
    log(lambda) <- a,
    a ~ dnorm(0, 10)
  ),
  data = cop, start = list (a = 6.0), iter = 4000, warmup = 1000,
  chains = 4, cores = 8, control = list(adapt_delta = 0.95)) 
precis(m11)
plot(precis(m11))

m22 <- map2stan(
  alist(
    stops ~ dpois(lambda),
    log(lambda) <- a + beth*eth,
    a ~ dnorm(0, 10),
    beth ~ dnorm(0, 1)
  ),
  data = cop, start = list (a = 6.0, beth = -0.7), iter = 4000, warmup = 1000,
  chains = 4, cores = 8, control = list(adapt_delta = 0.95)) 
plot(m22)
precis(m22)
plot(precis(m22))

m33 <- map2stan(
  alist(
    stops ~ dpois(lambda),
    log(lambda) <- a + beth*eth + bcrime*crime,
    a ~ dnorm(0, 10),
    beth ~ dnorm(0, 1),
    bcrime ~ dnorm(0, 1)
  ),
  data = cop, start = list (a = 6.0, beth = -0.7, bcrime = -0.3), iter = 4000, warmup = 1000,
  chains = 4, cores = 8, control = list(adapt_delta = 0.95)) 
plot(m33)
precis(m33)
plot(precis(m33))

#This is the best model by MAIC*********
m44 <- map2stan(
  alist(
    stops ~ dpois(lambda),
    log(lambda) <- a + beth*eth + bcrime*crime +bpop*pop,
    a ~ dnorm(0, 10),
    beth ~ dnorm(0, 1),
    bcrime ~ dnorm(0, 1),
    bpop ~ dnorm(0, 1)
  ),
  data = cop, start = list (a = 6.0, beth = -0.7, bcrime = -0.3, bpop = 0), iter = 4000, warmup = 1000,
  chains = 4, cores = 8, control = list(adapt_delta = 0.95)) 
plot(m44)
precis(m44)
plot(precis(m44))
#*****

m55 <- map2stan(
  alist(
    stops ~ dpois(lambda),
    log(lambda) <- a + beth*eth + bcrime*crime +bpop*pop + barrests*arrests,
    a ~ dnorm(0, 10),
    beth ~ dnorm(0, 1),
    bcrime ~ dnorm(0, 1),
    bpop ~ dnorm(0, 1),
    barrests ~ dnorm(0,1)
  ),
  data = cop, start = list (a = 6.0, beth = -0.7, bcrime = -0.3, bpop = 0.5), iter = 4000, warmup = 1000,
  chains = 4, cores = 8, control = list(adapt_delta = 0.95)) 
precis(m55)
plot(precis(m55))
plot(m55)
compare(m11, m22, m33, m44, m55)

post <- extract.samples(m55)
str(post)
mean(post$beth)
