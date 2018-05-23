t_max <- 20
library(deSolve)

# #### S_4 ME ####
# s4me <- function(t, y, parms) {
#   with(as.list(c(y,parms)), {
#     dy1 <- g * (y[2] + y[3] + y[4] + y[5])
#     dy2 <- g * (y[6] + y[7] + y[8]) - (3 * b + g) * y[2]
#     dy3 <- g * (y[6] + y[9] + y[10]) - (b + g) * y[3]
#     dy4 <- g * (y[7] + y[9] + y[11]) - (b + g) * y[4]
#     dy5 <- g * (y[8] + y[10] + y[11]) - (b + g) * y[5]
#     dy6 <- b * (y[2] + y[3]) + g * (y[12] + y[13]) - 2 * (b + g) * y[6]
#     dy7 <- b * (y[2] + y[4]) + g * (y[12] + y[14]) - 2 * (b + g) * y[7]
#     dy8 <- b * (y[2] + y[5]) + g * (y[13] + y[14]) - 2 * (b + g) * y[8]
#     dy9 <- g * (y[12] + y[17]) - 2 * (b + g) * y[9]
#     dy10 <- g * (y[13] + y[17]) - 2 * (b + g) * y[10]
#     dy11 <- g * (y[14] + y[17]) - 2 * (b + g) * y[11]
#     dy12 <- b * (y[6] + y[7] + 2 * y[9]) + g * y[16] - (b + 3 * g) * y[12]
#     dy13 <- b * (y[6] + y[8] + 2 * y[10]) + g * y[16] - (b + 3 * g) * y[13]
#     dy14 <- b * (y[7] + y[8] + 2 * y[11]) + g * y[16] - (b + 3 * g) * y[14]
#     dy15 <- g * y[16] - (b + 3 * g) * y[15]
#     dy16 <- b * (y[12] + y[13] + y[14] + 3 * y[15]) - 4 * g * y[16]
#     list(c(dy1, dy2, dy3, dy4, dy5, dy6, dy7, dy8, dy9, dy10, dy11, dy12, dy13, dy14, dy15, dy16))
#   })
# }
# yini <- c(y1 = 0, y2 = 0.25, y3 = 0.25, y4 = 0.25, y5 = 0.25, y6 = 0, y7 = 0, y8 = 0, y9 = 0, y10 = 0, y11 = 0, y12 = 0, y13 = 0, y14 = 0, y15 = 0, y16 = 0)
# parms <- c(b=1, g=1)
# times <- seq(from = 0, to = t_max, by = 0.01)
# s4 <- ode (times = times, y = yini, func = s4me, parms = parms)
# s4 <- as.data.frame(s4)

#### S_4 LME ####
s4lme <- function(t, y, parms) {
  with(as.list(c(y,parms)), {
    dy1 <- g * (y[2] + y[3])
    dy2 <- g * y[4] - (3 * b + g) * y[2]
    dy3 <- g * (y[4] + 2 * y[5]) - (g + b) * y[3]
    dy4 <- b * (3 * y[2] + y[3]) + 2 * g * y[6] - 2 * (b + g) * y[4]
    dy5 <- g * (y[6] + y[7]) - 2 * (b + g) * y[5]
    dy6 <- 2 * b * (y[4] + y[5]) + 3 * g * y[8] - (b + 3 * g) * y[6]
    dy7 <- g * y[8] - (b + 3 * g) * y[7]
    dy8 <- b * (y[6] + 3 * y[7]) - 4 * g * y[8]
    list(c(dy1, dy2, dy3, dy4, dy5, dy6, dy7, dy8))
  })
}
yini <- c(y1 = 0, y2 = 0.25, y3 = 0.75, y4 = 0, y5 = 0, y6 = 0, y7 = 0, y8 = 0)
parms <- c(b=1, g=1)
times <- seq(from = 0, to = t_max, by = 0.01)
s4l <- ode (times = times, y = yini, func = s4lme, parms = parms)
s4l <- as.data.frame(s4l)

### Simulation data

R <- 2500
# Import data, make workable dataframe
mydata <- as.data.frame(read.table("../output/s4_output.txt"))
colnames(mydata) <- c("t", "state")
mydata$state <- strtoi(mydata$state,base = 2)

# Sort data by time
data_sorted <- mydata[order(mydata$t),]

# Find unique times
times_unique <- unique(data_sorted$t)

# Make data frame into whcih we can output our data
df <- data.frame(matrix(nrow = length(times_unique),ncol = 17))
colnames(df) <- c("t", "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "s12", "s13", "s14", "s15")
df$t <- times_unique

# Sorted data for which t == 0 
myrows <- which(mydata$t == df[1, 1])
states_cur <- mydata[myrows, 2]

# Populate first row
for (i in 0:15) {
  df[1, i+2] <- length(which(states_cur == i))
}

# Populate subsequent rows, accounting for changes
for (i in 2:length(times_unique)) {
  myrows <- which(mydata$t == df[i, 1])
  states_cur <- mydata[myrows, 2]
  states_prev <- mydata[myrows - 1, 2]
  df[i, 2:17] <- df[i-1, 2:17]
  for (j in 1:length(states_cur)) {
    df[i, states_cur[j]+2] <- df[i, states_cur[j]+2] + 1
    df[i, states_prev[j]+2] <- df[i, states_prev[j]+2] - 1
  }
}

# Scale for number of realisations
df[,2:17] <- df[2:17]/R

df2 <- data.frame(matrix(nrow = length(times_unique),ncol = 6))
colnames(df2) <- c("t", "s0", "s1", "s2", "s3", "s4")
df2$t <- df$t
df2$s0 <- df$s0
df2$s1 <- df$s1 + df$s2 + df$s4 + df$s8
df2$s2 <- df$s3 + df$s5 + df$s9 + df$s6 + df$s10 + df$s12
df2$s3 <- df$s7 + df$s11 + df$s13 + df$s14
df2$s4 <- df$s15

s4l2 <- data.frame(matrix(nrow = length(s4l$time),ncol = 6))
colnames(s4l2) <- c("t", "s0", "s1", "s2", "s3", "s4")
s4l2$t <- s4l$time
s4l2$s0 <- s4l$y1
s4l2$s1 <- s4l$y2 + s4l$y3
s4l2$s2 <- s4l$y4 + s4l$y5
s4l2$s3 <- s4l$y6 + s4l$y7
s4l2$s4 <- s4l$y8

pdf("./sis_s4_0.pdf")
library(ggplot2)
ggplot(df2, aes(t, s0)) +
  geom_point(aes(colour = "Simulation"), size = 0.5, alpha = 0.05) +
  geom_line(data = s4l2, aes(x=t, y=s0 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  scale_color_manual(values=c("#990000", "#000000")) +
  theme_bw() +
  theme(legend.position="none")
dev.off()

pdf("./sis_s4_1.pdf")
library(ggplot2)
ggplot(df2, aes(t, s1)) +
  geom_point(aes(colour = "Simulation"), size = 0.5, alpha = 0.05) +
  geom_line(data = s4l2, aes(x=t, y=s1 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  scale_color_manual(values=c("#990000", "#000000")) +
  theme_bw() +
  theme(legend.position="none")
dev.off()

pdf("./sis_s4_2.pdf")
library(ggplot2)
ggplot(df2, aes(t, s2)) +
  geom_point(aes(colour = "Simulation"), size = 0.5, alpha = 0.05) +
  geom_line(data = s4l2, aes(x=t, y=s2 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  scale_color_manual(values=c("#990000", "#000000")) +
  theme_bw() +
  theme(legend.position="none")
dev.off()

pdf("./sis_s4_3.pdf")
library(ggplot2)
ggplot(df2, aes(t, s3)) +
  geom_point(aes(colour = "Simulation"), size = 0.5, alpha = 0.05) +
  geom_line(data = s4l2, aes(x=t, y=s3 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  scale_color_manual(values=c("#990000", "#000000")) +
  theme_bw() +
  theme(legend.position="none")
dev.off()

pdf("./sis_s4_4.pdf")
library(ggplot2)
ggplot(df2, aes(t, s4)) +
  geom_point(aes(colour = "Simulation"), size = 0.5, alpha = 0.05) +
  geom_line(data = s4l2, aes(x=t, y=s4 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  scale_color_manual(values=c("#990000", "#000000")) +
  theme_bw() +
  theme(legend.position="none")
dev.off()

