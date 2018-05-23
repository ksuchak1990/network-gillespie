N <- 3
beta <- 0.1
gamma <- 0.1
I_init <- 1
rat <- gamma / beta

R <- 2000
# Import data, make workable dataframe
mydata <- as.data.frame(read.table("../output/output.txt"))
colnames(mydata) <- c("t", "state")
mydata$state <- strtoi(mydata$state,base = 2)

# Sort data by time
data_sorted <- mydata[order(mydata$t),]

# Find unique times
times_unique <- unique(data_sorted$t)

# Make data frame into whcih we can output our data
df <- data.frame(matrix(nrow = length(times_unique),ncol = 9))
colnames(df) <- c("t", "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7")
df$t <- times_unique

# Sorted data for which t == 0 
myrows <- which(mydata$t == df[1, 1])
states_cur <- mydata[myrows, 2]

# Populate first row
for (i in 0:7) {
  df[1, i+2] <- length(which(states_cur == i))
}

# Populate subsequent rows, accounting for changes
for (i in 2:length(times_unique)) {
  myrows <- which(mydata$t == df[i, 1])
  states_cur <- mydata[myrows, 2]
  states_prev <- mydata[myrows - 1, 2]
  df[i, 2:9] <- df[i-1, 2:9]
  for (j in 1:length(states_cur)) {
    df[i, states_cur[j]+2] <- df[i, states_cur[j]+2] + 1
    df[i, states_prev[j]+2] <- df[i, states_prev[j]+2] - 1
  }
}

# Scale for number of realisations
df[,2:9] <- df[2:9]/R

df2 <- data.frame(matrix(nrow = length(times_unique),ncol = 9))
colnames(df2) <- c("t", "s0", "s1", "s2", "s3")
df2$t <- df$t
df2$s0 <- df$s0
df2$s1 <- df$s1 + df$s2 + df$s4
df2$s2 <- df$s3 + df$s5 + df$s6
df2$s3 <- df$s7

df2$s0 <- df2$s0 * 0
df2$s1 <- df2$s1 * 1
df2$s2 <- df2$s2 * 2
df2$s3 <- df2$s3 * 3

df2$sum <- df2$s0 + df2$s1 + df2$s2 + df2$s3

# Theoretical solution
myfrac <- (N - rat - I_init)/I_init
#t <- seq(from = 0.0, to = max(tempdata[which(tempdata$I==max(tempdata$I) | tempdata$I==0),1])
#         , length.out =  10000)
t <- seq(from = 0.0, to = 250
         , length.out =  10000)
myI <- (N - rat) * ((1 + myfrac * exp((gamma - beta * N) * t))^-1)
mydata <- data.frame(t, myI, N - myI)
colnames(mydata) <- c("t","myI","myS")

#Check for required packages
list.of.packages <- c("ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(ggplot2)


# Plot comparison of theoretical and simulation
png(filename="./sis_plot.png")
ggplot(data = df2, aes(x=t, y=sum)) +
  geom_line(aes(colour="Simulation I")) +
  geom_line(data = mydata, aes(x=t, y=myI, colour="Theoretical I"),linetype="dotted") +
  xlab("Time") +
  ylab("Susceptible/Infected") +
  theme_bw()
dev.off()


N<-3
#### SIS complete N network ####
Knet <- function(t, y, parms) {
  with(as.list(c(y,parms)), {
    dy1 <- g * y[2]
    for (i in 2:N-1){
      dyi <- (i+1) * g * y[i+1] + (i-1) * (N - i - 1) * b * y[i-1] - (k * (N - i) * b + i * g) * y[i]
    }
    dyN <- (N-1) * b * y[N-1] - N * g * y[N]
    list(c(dy1, dy2, dy3, dy4, dy5, dy6, dy7, dy8))
  })
}
yini <- c(y1 = 0, y2 = th, y3 = th, y4 = th, y5 = 0, y6 = 0, y7 = 0, y8 = 0)
parms <- c(b=0.1, g=0.1)
times <- seq(from = 0, to = max(df$t), by = 0.01)
out <- ode (times = times, y = yini, func = sistrinet, parms = parms)
out <- as.data.frame(out)

th <- 1/3
library(deSolve)
#### SIS triangle network ####
sistrinet <- function(t, y, parms) {
  with(as.list(c(y,parms)), {
    dk1 <- g * (y[2] + y[3] + y[4])
    dk2 <- g * (y[5] + y[6]) - (2 * b + g) * y[2]
    dk3 <- g * (y[5] + y[7]) - (2 * b + g) * y[3]
    dk4 <- g * (y[6] + y[7]) - (2 * b + g) * y[4]
    dk5 <- g * y[8] + b * (y[2] + y[3]) - 2 * (b + g) * y[5]
    dk6 <- g * y[8] + b * (y[2] + y[4]) - 2 * (b + g) * y[6]
    dk7 <- g * y[8] + b * (y[3] + y[4]) - 2 * (b + g) * y[7]
    dk8 <- 2 * b * (y[5] + y[6] + y[7]) - 3 * g * y[8]
    list(c(dk1, dk2, dk3, dk4, dk5, dk6, dk7, dk8))
  })
}
yini <- c(y1 = 0, y2 = th, y3 = th, y4 = th, y5 = 0, y6 = 0, y7 = 0, y8 = 0)
parms <- c(b=0.1, g=0.1)
times <- seq(from = 0, to = 300, by = 0.1)
out <- ode (times = times, y = yini, func = sistrinet, parms = parms)
out <- as.data.frame(out)
plot(out$time,out$y1)



###### Networks ######

k3_mat <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow=3, ncol=3, byrow = TRUE)
s4_mat <- matrix(c(0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0), nrow=4, ncol=4, byrow = TRUE)

kn_mat <- matrix(rep(1, len=100), nrow = 10, ncol = 10)
diag(kn_mat) <- 0

sn_mat <- matrix(rep(0, len=100), nrow = 10, ncol = 10)
sn_mat[1,] <- 1
sn_mat[,1] <- 1
diag(sn_mat) <- 0

ex_mat <- matrix(c(0, 0, 1, 0, 0, 
                   0, 0, 1, 0, 0, 
                   1, 1, 0, 1, 1, 
                   0, 0, 1, 0, 1, 
                   0, 0, 1, 1, 0), nrow=5, ncol=5, byrow = TRUE)

library(network)
library(sna)
library(ggplot2)

k3_net <- network(k3_mat, directed = FALSE)
s4_net <- network(s4_mat, directed = FALSE)

kn_net <- network(kn_mat, directed = FALSE)
sn_net <- network(sn_mat, directed = FALSE)
ex_net <- network(ex_mat, directed = FALSE)

library(GGally)
pdf(file = "k3_net.pdf")
ggnet2(k3_net, label = TRUE, color = "black", label.color = "white", mode = "circle")
dev.off()

pdf(file = "s4_net.pdf")
ggnet2(s4_net, label = TRUE, color = "black", label.color = "white", mode = "hall")
dev.off()

pdf(file = "kn_net.pdf")
ggnet2(kn_net, color = "black", mode = "circle")
dev.off()

pdf(file = "sn_net.pdf")
ggnet2(sn_net, color = "black", mode = "kamadakawai")
dev.off()

pdf(file = "ex_net.pdf")
ggnet2(ex_net, label = TRUE, color = "black", label.color = "white", mode = "kamadakawai")
dev.off()


