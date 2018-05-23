args <- commandArgs(TRUE)

# Parameters
N <- as.integer(args[1])
beta <- as.double(args[2])
gamma <- as.double(args[3])
I_init <- as.integer(args[4])
rat <- gamma / beta

# Import simulation data
tempdata <- as.data.frame(read.table("./output/output.txt"))
tempvar <- N - tempdata$V2
tempdata2 <- data.frame(tempdata$V1, tempvar)
colnames(tempdata) <- c("t","I")
colnames(tempdata2) <- c("t","S")

k <- N
interval <- max(tempdata$I)/k
lower <- 0
upper <- lower + interval
Is <- data.frame( "t" = numeric(0) , "I" = numeric(0))

# Process simulation data
for (i in 1:k)
{
  tempvar <- tempdata[which(tempdata$I >= lower  & tempdata$I < upper),]
  Is[i,1] <- mean(tempvar$t)
  Is[i,2] <- (upper + lower)/2
  lower <- upper
  upper <- lower + interval
}

Ss <- data.frame( Is$t , N - Is$I)
colnames(Ss) <- c("t","S")

# Theoretical solution
myfrac <- (N - rat - I_init)/I_init
t <- seq(from = 0.0, to = max(tempdata[which(tempdata$I==max(tempdata$I) | tempdata$I==0),1])
         , length.out =  10000)
myI <- (N - rat) * ((1 + myfrac * exp((gamma - beta * N) * t))^-1)
mydata <- data.frame(t, myI, N - myI)
colnames(mydata) <- c("t","myI","myS")

#Check for required packages
list.of.packages <- c("ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(ggplot2)

tv <- Is[complete.cases(Is$t),]
m <- max(tv$t)/2

# Plot comparison of theoretical and simulation
png(filename="./graphs/sis_plot.png")
ggplot(data = Is, aes(x=t, y=I)) +
  geom_line(aes(colour="Simulation I")) +
  geom_line(data = Ss, aes(x=t, y=S , colour="Simulation S")) +
  geom_line(data = mydata, aes(x=t, y=myI, colour="Theoretical I"),linetype="dotted") +
  geom_line(data = mydata, aes(x=t, y=myS, colour="Theoretical S"),linetype="dotted") +
  xlab("Time") +
  xlim(0, m)
  ylab("Susceptible/Infected") +
  theme_bw()
dev.off()


