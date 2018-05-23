### Simulation data

R <- 1000
# Import data, make workable dataframe
mydata <- as.data.frame(read.table("../output/output.txt"))
colnames(mydata) <- c("t", "state")
mydata$state <- strtoi(mydata$state,base = 2)

# Sort data by time
data_sorted <- mydata[order(mydata$t),]

# Find unique times
times_unique <- unique(data_sorted$t)

# Make data frame into whcih we can output our data
df <- data.frame(matrix(nrow = length(times_unique),ncol = 8))
colnames(df) <- c("t", "s1", "s2", "s3", "s4", "s5", "s6", "s7")
df$t <- times_unique

# Sorted data for which t == 0 
myrows <- which(mydata$t == df[1, 1])
states_cur <- mydata[myrows, 2]

for (i in 1:7) {
  df[1, i+1] <- length(which(states_cur == i))
}

for (i in 2:length(times_unique)) {
  myrows <- which(mydata$t == df[i, 1])
  states_cur <- mydata[myrows, 2]
  states_prev <- mydata[myrows - 1, 2]
  df[i, 2:8] <- df[i-1, 2:8]
  for (j in 1:length(states_cur)) {
    df[i, states_cur[j]+1] <- df[i, states_cur[j]+1] + 1
    df[i, states_prev[j]+1] <- df[i, states_prev[j]+1] - 1
  }
}

# Scale for number of realisations
df[,2:8] <- df[2:8]/R

### Numerical solution of master equation

th <- 1/3
library(deSolve)

#### SI triangle network ####
trinet <- function(t, y, parms) {
  with(as.list(c(y,parms)), {
    dy1 <- 0
    dy2 <- -2  * b * y[2]
    dy3 <- -2  * b * y[3]
    dy4 <- -2  * b * y[4]
    dy5 <- b * (y[2] + y[3] - 2 * y[5])
    dy6 <- b * (y[2] + y[4] - 2 * y[6])
    dy7 <- b * (y[3] + y[4] - 2 * y[7])
    dy8 <- 2 * b * (y[5] + y[6] + y[7])
    list(c(dy1, dy2, dy3, dy4, dy5, dy6, dy7, dy8))
  })
}
yini <- c(y1 = 0, y2 = th, y3 = th, y4 = th, y5 = 0, y6 = 0, y7 = 0, y8 = 0)
parms <- c(b=0.5)
times <- seq(from = 0, to = 20, by = 0.01)
out <- ode (times = times, y = yini, func = trinet, parms = parms)
out <- as.data.frame(out)

png(filename = "./si_trinet_001.png")
library(ggplot2)
ggplot(df, aes(t, s1)) +
  geom_point(aes(colour = "Simulation")) +
  geom_line(data = out, aes(x=time, y=y2 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  theme_bw()
dev.off()

png(filename = "./si_trinet_010.png")
library(ggplot2)
ggplot(df, aes(t, s2)) +
  geom_point(aes(colour = "Simulation")) +
  geom_line(data = out, aes(x=time, y=y3 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  theme_bw()
dev.off()

png(filename = "./si_trinet_100.png")
library(ggplot2)
ggplot(df, aes(t, s4)) +
  geom_point(aes(colour = "Simulation")) +
  geom_line(data = out, aes(x=time, y=y4 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  theme_bw()
dev.off()

png(filename = "./si_trinet_011.png")
library(ggplot2)
ggplot(df, aes(t, s3)) +
  geom_point(aes(colour = "Simulation")) +
  geom_line(data = out, aes(x=time, y=y5 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  theme_bw()
dev.off()

png(filename = "./si_trinet_101.png")
library(ggplot2)
ggplot(df, aes(t, s5)) +
  geom_point(aes(colour = "Simulation")) +
  geom_line(data = out, aes(x=time, y=y6 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  theme_bw()
dev.off()

png(filename = "./si_trinet_110.png")
library(ggplot2)
ggplot(df, aes(t, s6)) +
  geom_point(aes(colour = "Simulation")) +
  geom_line(data = out, aes(x=time, y=y7 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  theme_bw()
dev.off()

png(filename = "./si_trinet_111.png")
library(ggplot2)
ggplot(df, aes(t, s7)) +
  geom_point(aes(colour = "Simulation")) +
  geom_line(data = out, aes(x=time, y=y8 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  theme_bw()
dev.off()




# png(filename = "./trinet.png")
# plot(out)
# dev.off()





# # Plot for each state
# for (i in 1:7) {
#   plot(df$t, df[,i+1])
# }


