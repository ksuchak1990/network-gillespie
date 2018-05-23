### Simulation data

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

df2 <- data.frame(matrix(nrow = length(times_unique),ncol = 6))
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

df2$tot <- df2$s0 + df2$s1 + df2$s2 + df2$s3

### Numerical solution of master equation

th <- 1/3
library(deSolve)

#### SIS triangle network ####
sistrinet <- function(t, y, parms) {
  with(as.list(c(y,parms)), {
    dy1 <- g * (y[2] + y[3] + y[4])
    dy2 <- g * (y[5] + y[6]) - (2 * b + g) * y[2]
    dy3 <- g * (y[5] + y[7]) - (2 * b + g) * y[3]
    dy4 <- g * (y[6] + y[7]) - (2 * b + g) * y[4]
    dy5 <- g * y[8] + b * (y[2] + y[3]) - 2 * (b + g) * y[5]
    dy6 <- g * y[8] + b * (y[2] + y[4]) - 2 * (b + g) * y[6]
    dy7 <- g * y[8] + b * (y[3] + y[4]) - 2 * (b + g) * y[7]
    dy8 <- 2 * b * (y[5] + y[6] + y[7]) - 3 * g * y[8]
    list(c(dy1, dy2, dy3, dy4, dy5, dy6, dy7, dy8))
  })
}
yini <- c(y1 = 0, y2 = th, y3 = th, y4 = th, y5 = 0, y6 = 0, y7 = 0, y8 = 0)
parms <- c(b=0.1, g=0.1)
times <- seq(from = 0, to = max(df$t), by = 0.01)
out <- ode (times = times, y = yini, func = sistrinet, parms = parms)
out <- as.data.frame(out)

out2 <- data.frame(matrix(nrow = length(out$time),ncol = 6))
colnames(out2) <- c("t", "s0", "s1", "s2", "s3", "tot")
out2$t <- out$time
out2$s0 <- out$y1
out2$s1 <- out$y2 + out$y3 + out$y4
out2$s2 <- out$y5 + out$y6 + out$y7
out2$s3 <- out$y8

out2$s0 <- out2$s0 * 0
out2$s1 <- out2$s1 * 1
out2$s2 <- out2$s2 * 2
out2$s3 <- out2$s3 * 3

out2$tot <- out2$s0 + out2$s1 + out2$s2 + out2$s3


png(filename = "./sis_trinet_000.png")
library(ggplot2)
ggplot(df, aes(t, s0)) +
  geom_point(aes(colour = "Simulation")) +
  geom_line(data = out, aes(x=time, y=y1 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  theme_bw()
dev.off()

png(filename = "./sis_trinet_001.png")
library(ggplot2)
ggplot(df, aes(t, s1)) +
  geom_point(aes(colour = "Simulation")) +
  geom_line(data = out, aes(x=time, y=y2 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  theme_bw()
dev.off()

png(filename = "./sis_trinet_010.png")
library(ggplot2)
ggplot(df, aes(t, s2)) +
  geom_point(aes(colour = "Simulation")) +
  geom_line(data = out, aes(x=time, y=y3 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  theme_bw()
dev.off()

png(filename = "./sis_trinet_100.png")
library(ggplot2)
ggplot(df, aes(t, s4)) +
  geom_point(aes(colour = "Simulation")) +
  geom_line(data = out, aes(x=time, y=y4 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  theme_bw()
dev.off()

png(filename = "./sis_trinet_011.png")
library(ggplot2)
ggplot(df, aes(t, s3)) +
  geom_point(aes(colour = "Simulation")) +
  geom_line(data = out, aes(x=time, y=y5 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  theme_bw()
dev.off()

png(filename = "./sis_trinet_101.png")
library(ggplot2)
ggplot(df, aes(t, s5)) +
  geom_point(aes(colour = "Simulation")) +
  geom_line(data = out, aes(x=time, y=y6 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  theme_bw()
dev.off()

png(filename = "./sis_trinet_110.png")
library(ggplot2)
ggplot(df, aes(t, s6)) +
  geom_point(aes(colour = "Simulation")) +
  geom_line(data = out, aes(x=time, y=y7 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  theme_bw()
dev.off()

png(filename = "./sis_trinet_111.png")
library(ggplot2)
ggplot(df, aes(t, s7)) +
  geom_point(aes(colour = "Simulation")) +
  geom_line(data = out, aes(x=time, y=y8 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  theme_bw()
dev.off()

png(filename = "./sis_trinet_average.png")
library(ggplot2)
ggplot(df2, aes(x=t, y=tot)) +
  geom_point(aes(colour = "Simulation")) +
  geom_line(data = out2, aes(x=t, y=tot , colour="Numerical")) +
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


