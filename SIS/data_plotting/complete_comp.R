t_max <- 20
th <- 1/3
library(deSolve)
# 
# #### K_3 ME ####
# k3me <- function(t, y, parms) {
#   with(as.list(c(y,parms)), {
#     dy1 <- g * (y[2] + y[3] + y[4])
#     dy2 <- g * (y[5] + y[6]) - (2 * b + g) * y[2]
#     dy3 <- g * (y[5] + y[7]) - (2 * b + g) * y[3]
#     dy4 <- g * (y[6] + y[7]) - (2 * b + g) * y[4]
#     dy5 <- g * y[8] + b * (y[2] + y[3]) - 2 * (b + g) * y[5]
#     dy6 <- g * y[8] + b * (y[2] + y[4]) - 2 * (b + g) * y[6]
#     dy7 <- g * y[8] + b * (y[3] + y[4]) - 2 * (b + g) * y[7]
#     dy8 <- 2 * b * (y[5] + y[6] + y[7]) - 3 * g * y[8]
#     list(c(dy1, dy2, dy3, dy4, dy5, dy6, dy7, dy8))
#   })
# }
# yini <- c(y1 = 0, y2 = th, y3 = th, y4 = th, y5 = 0, y6 = 0, y7 = 0, y8 = 0)
# parms <- c(b=1, g=1)
# times <- seq(from = 0, to = t_max, by = 0.01)
# k3 <- ode (times = times, y = yini, func = k3me, parms = parms)
# k3 <- as.data.frame(k3)
# 

#### K_3 LME ####
k3lme <- function(t, y, parms) {
  with(as.list(c(y,parms)), {
    dy1 <- g * y[2]
    dy2 <- 2 * g * y[3] - (2 * b + g) * y[2]
    dy3 <- 3 * g * y[4] + 2 * b * y[2] - 2 * (b + g) * y[3]
    dy4 <- 2 * b * y[3] - 3 * g * y[4]
    list(c(dy1, dy2, dy3, dy4))
  })
}
yini <- c(y1 = 0, y2 = 1, y3 = 0, y4 = 0)
parms <- c(b=1, g=1)
times <- seq(from = 0, to = t_max, by = 0.01)
k3l <- ode (times = times, y = yini, func = k3lme, parms = parms)
k3l <- as.data.frame(k3l)

### Simulation data

R <- 2500
# Import data, make workable dataframe
mydata <- as.data.frame(read.table("../output/k3_output.txt"))
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

pdf("./sis_k3_0.pdf")
library(ggplot2)
ggplot(df2, aes(t, s0)) +
  geom_point(aes(colour = "Simulation"), size = 0.5, alpha = 0.05) +
  geom_line(data = k3l, aes(x=time, y=y1 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  scale_color_manual(values=c("#990000", "#000000")) +
  theme_bw() +
  theme(legend.position="none")
dev.off()

pdf("./sis_k3_1.pdf")
library(ggplot2)
ggplot(df2, aes(t, s1)) +
  geom_point(aes(colour = "Simulation"), size = 0.5, alpha = 0.05) +
  geom_line(data = k3l, aes(x=time, y=y2 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  scale_color_manual(values=c("#990000", "#000000")) +
  theme_bw() +
  theme(legend.position="none")
dev.off()

pdf("./sis_k3_2.pdf")
library(ggplot2)
ggplot(df2, aes(t, s2)) +
  geom_point(aes(colour = "Simulation"), size = 0.5, alpha = 0.05) +
  geom_line(data = k3l, aes(x=time, y=y3 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  scale_color_manual(values=c("#990000", "#000000")) +
  theme_bw() +
  theme(legend.position="none")
dev.off()

pdf("./sis_k3_3.pdf")
library(ggplot2)
ggplot(df2, aes(t, s3)) +
  geom_point(aes(colour = "Simulation"), size = 0.5, alpha = 0.05) +
  geom_line(data = k3l, aes(x=time, y=y4 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  scale_color_manual(values=c("#990000", "#000000")) +
  theme_bw() +
  theme(legend.position="none")
dev.off()


