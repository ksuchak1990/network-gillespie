setwd("~/Documents/thesis/network-gillespie/")

# Parameters
N <- 250
beta <- 0.5
I_init <- 5.0

# Import data
tempdata <- as.data.frame(read.table("./output.txt"))
tempvar <- N - tempdata$V2
tempdata2 <- data.frame(tempdata$V1, tempvar)
colnames(tempdata) <- c("t","I")
colnames(tempdata2) <- c("t","S")

k <- 125
interval <- max(tempdata$I)/k
lower <- 0
upper <- lower + interval
Is <- data.frame( "t" = numeric(0) , "I" = numeric(0))

for (i in 1:k)
{
  lower <- upper
  upper <- lower + interval
  tempvar <- tempdata[which(tempdata$I > lower  & tempdata$I < upper),]
  Is[i,1] <- mean(tempvar$t)
  Is[i,2] <- (upper + lower)/2
}

Ss <- data.frame( Is$t , N - Is$I)
colnames(Ss) <- c("t","S")


# Theoretical solution
myfrac <- (N-I_init)/I_init
t <- seq(from = 0.0, to = max(tempdata[which(tempdata$I==max(tempdata$I) | tempdata$I==0),1])
         , length.out =  10000)
myI <- N*((1+myfrac*exp(-beta*N*t))^-1)
mydata <- data.frame(t, myI)
mydata2 <- data.frame(t, N - myI)
colnames(mydata2) <- c("t","myS")

# plot(Is$t,Is$I,type = 'l', col = "red")
# points(Ss[,1],Ss[,2],type = 'l',col = "green")
# points(mydata$t,mydata$myI,type = 'l', col = "blue")
# points(mydata2$t,mydata2$myS,type = 'l', col = "black")

#theory curve black dash
#sim blue

#Check for required packages
list.of.packages <- c("ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(ggplot2)

png(filename="./plot.png")
ggplot(data = Is, aes(x=t, y=I)) +
  geom_line(aes(colour="Simulation I")) +
  geom_line(data = Ss, aes(x=t, y=S , colour="Simulation S")) +
  geom_line(data = mydata, aes(x=t, y=myI, colour="Theoretical I"),linetype="dotted") +
  geom_line(data = mydata2, aes(x=t, y=myS, colour="Theoretical S"),linetype="dotted") +
  xlab("Time") +
  ylab("Susceptible/Infected") +
  theme_bw()
dev.off()



# Produce graphical output
# plot(tempdata$t,tempdata$I,col="red")
# points(tempdata2$t,tempdata2$S,col="blue")
# points(mydata$t,mydata$myI,type = "l")
# points(mydata2$t,mydata2$myS,type = "l")
# ggplot(data = tempdata, aes(x=t, y=I)) +
# 	geom_smooth(aes(colour="Simulation I")) +
#   geom_smooth(data = tempdata2, aes(x=t, y=S , colour="Simulation S")) +
#   geom_line(data = mydata, aes(x=t, y=myI, colour="Theoretical I")) +
#   geom_line(data = mydata2, aes(x=t, y=myS, colour="Theoretical S")) +
#   xlab("Time") +
# 	ylab("Susceptible/Infected") +
# 	theme_bw()



# Average time to absorbing state
# cat("Average time to absorbing state")
# mean(tempdata[which(tempdata$I==max(tempdata$I) | tempdata$I==0),1])


