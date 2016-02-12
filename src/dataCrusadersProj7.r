
t = read.csv("WINE142.csv")

summary(t$COUNTRY)

arg = (subset(t, COUNTRY == "Argentina"))
aus = (subset(t, COUNTRY == "Australia"))
cal = (subset(t, COUNTRY == "California"))
fra = (subset(t, COUNTRY == "France"))
ita = (subset(t, COUNTRY == "Italy"))
kaz = (subset(t, COUNTRY == "Kazahstan"))
mon = (subset(t, COUNTRY == "Montenegro"))

c(mean(arg$PRICE), mean(aus$PRICE), mean(cal$PRICE), mean(fra$PRICE), mean(ita$PRICE), mean(kaz$PRICE), mean(mon$PRICE))

plot(t$COUNTRY, t$PRICE)


#t test
montenegro = t[t$COUNTRY == "Montenegro", 5]
france = t[t$COUNTRY == "France", 5]

#fit the longer one to be the size of the smaller one
montenegro = montenegro[1:240]

n = 1000
N = length(montenegro)

montenegro_200 <- montenegro_400 <- montenegro_600 <- montenegro_800 <- vector("numeric",n)
france_200 <- france_400 <- france_600 <- france_800 <- vector("numeric",n)


for(i in 1:n){
  s_200 <- sample(N,200,replace=T)
  s_400 <- sample(N,400,replace=T)
  s_600 <- sample(N,600,replace=T)
  s_800 <- sample(N,800,replace=T)
  
  montenegro_200[i] <- mean(montenegro[s_200])
  montenegro_400[i] <- mean(montenegro[s_400])
  montenegro_600[i] <- mean(montenegro[s_600])
  montenegro_800[i] <- mean(montenegro[s_800])
  
  france_200[i] <- mean(france[s_200])
  france_400[i] <- mean(france[s_400])
  france_600[i] <- mean(france[s_600])
  france_800[i] <- mean(france[s_800])
}


country_sd <- sd(montenegro - france)

d_200 <- sqrt(200)*(montenegro_200 - france_200)/country_sd
d_400 <- sqrt(400)*(montenegro_400 - france_400)/country_sd
d_600 <- sqrt(600)*(montenegro_600 - france_600)/country_sd
d_800 <- sqrt(800)*(montenegro_800 - france_800)/country_sd

par(mfrow=c(4,1))
hist(d_200,main="200 bottles",freq=F,xlim=c(-5,5),ylim=c(0,0.4))
curve(dnorm,add=T,col=2)
hist(d_400,main="400 bottles",freq=F,xlim=c(-5,5),ylim=c(0,0.4))
curve(dnorm,add=T,col=2)
hist(d_600,main="600 bottles",freq=F,xlim=c(-5,5),ylim=c(0,0.4))
curve(dnorm,add=T,col=2)
hist(d_800,main="800 bottles",freq=F,xlim=c(-5,5),ylim=c(0,0.4))
curve(dnorm,add=T,col=2)


dev.off()
plot(dnorm, col = 2, xlim = c(-5, 5))
abline(v = d_200[1], col = "red")
abline(v = d_800[1], col = "blue")




1 - pnorm(d_200[1])

1 - pnorm(d_800[1])
