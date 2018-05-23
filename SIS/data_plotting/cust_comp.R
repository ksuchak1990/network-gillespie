t_max <- 20
library(deSolve)

#### CUST LME ####
custlme <- function(t, y, parms) {
  with(as.list(c(y,parms)), {
    dy1 <- g * (y[2] + y[3] + y[4])
    
    dy2 <- g * (2 * y[5] + y[6] + y[7]) - (b + g) * y[2]
    dy3 <- g * (y[6] + y[8]) - (4 * b + g) * y[3]
    dy4 <- g * (y[7] + y[8] + 2 * y[9]) - (2 * b + g) * y[4]
    
    dy5 <- g * (y[10] + y[11]) - 2 * (b + g) * y[5]
    dy6 <- b * (y[2] + 2 * y[3]) + g * (2 * y[10] + y[12]) - (3 * b + 2 * g) * y[6]
    dy7 <- g * (2 * y[11] + y[12] + 2 * y[13]) - (3 * b + 2 * g) * y[7]
    dy8 <- b * (2 * y[3] + y[4]) + g * (y[12] + 2 * y[14]) - 2 * (2 * b + g) * y[8]
    dy9 <- b * y[4] + g * (y[13] + y[14]) - 2 * (b + g) * y[9]
    
    dy10 <- b * (2 * y[5] + y[6]) + g * y[15] - (2 * b + 3 * g) * y[10]
    dy11 <- g * (y[15] + 2 * y[16]) - (4 * b + 3 * g) * y[11]
    dy12 <- 2 * b * (y[6] + y[7] + y[8]) + 2 * g * (y[15] + y[17]) - 3 * (b + g) * y[12]
    dy13 <- b * y[7] + g * (2 * y[16] + y[17]) - 3 * (b + g) * y[13]
    dy14 <- 2 * b * (y[8] + y[9]) + g * y[17] - (2 * b + 3 * g) * y[14]
    
    dy15 <- b * (2 * y[10] + 3 * y[11] + y[12]) + 2 * g * y[18] - 2 * (b + 2 * g) * y[15]
    dy16 <- b * y[11] + g * y[18] - 4 * (b + g) * y[16]
    dy17 <- b * (2 * y[12] + 3 * y[13] + 2 * y[14]) + 2 * g * y[18] - (b + 4 * g) * y[17]
    
    dy18 <- b * (2 * y[15] + 4 * y[16] + y[17]) - 5 * g * y[18]
    list(c(dy1, dy2, dy3, dy4, dy5, dy6, dy7, dy8, dy9, dy10, dy11, dy12, dy13, dy14, dy15, dy16, dy17, dy18))
  })
}
yini <- c(y1 = 0, y2 = 0.4, y3 = 0.2, y4 = 0.4, y5 = 0, y6 = 0, y7 = 0, y8 = 0, y9 = 0, y10 = 0, y11 = 0, y12 = 0, y13 = 0, y14 = 0, y15 = 0, y16 = 0, y17 = 0, y18 = 0)
parms <- c(b=1, g=1)
times <- seq(from = 0, to = 20, by = 0.01)
custl <- ode (times = times, y = yini, func = custlme, parms = parms)
custl <- as.data.frame(custl)



### Simulation data

R <- 2500
# Import data, make workable dataframe
mydata <- as.data.frame(read.table("../output/custom_output.txt"))
colnames(mydata) <- c("t", "state")
mydata$state <- strtoi(mydata$state,base = 2)

# Sort data by time
data_sorted <- mydata[order(mydata$t),]

# Find unique times
times_unique <- unique(data_sorted$t)

# Make data frame into whcih we can output our data
df <- data.frame(matrix(nrow = length(times_unique),ncol = 33))
colnames(df) <- c("t", "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "s12", "s13", "s14", "s15", "s16", "s17", "s18", "s19", "s20", "s21", "s22", "s23", "s24", "s25", "s26", "s27", "s28", "s29", "s30", "s31")
df$t <- times_unique

# Sorted data for which t == 0 
myrows <- which(mydata$t == df[1, 1])
states_cur <- mydata[myrows, 2]

# Populate first row
for (i in 0:31) {
  df[1, i+2] <- length(which(states_cur == i))
}

# Populate subsequent rows, accounting for changes
for (i in 2:length(times_unique)) {
  myrows <- which(mydata$t == df[i, 1])
  states_cur <- mydata[myrows, 2]
  states_prev <- mydata[myrows - 1, 2]
  df[i, 2:33] <- df[i-1, 2:33]
  for (j in 1:length(states_cur)) {
    df[i, states_cur[j]+2] <- df[i, states_cur[j]+2] + 1
    df[i, states_prev[j]+2] <- df[i, states_prev[j]+2] - 1
  }
}

# Scale for number of realisations
df[,2:33] <- df[2:33]/R

df2 <- data.frame(matrix(nrow = length(times_unique),ncol = 7))
colnames(df2) <- c("t", "s0", "s1", "s2", "s3", "s4", "s5")
df2$t <- df$t
df2$s0 <- df$s0
df2$s1 <- df$s1 + df$s2 + df$s4 + df$s8 + df$s16
df2$s2 <- df$s3 + df$s5 + df$s9 + df$s6 + df$s10 + df$s12 + df$s20 + df$s24 + df$s18 + df$s17
df2$s3 <- df$s28 + df$s26 + df$s25 + df$s19 + df$s11 + df$s7 + df$s22 + df$s13 + df$s14 + df$s21
df2$s4 <- df$s15 + df$s23 + df$s27 + df$s29 + df$s30
df2$s5 <- df$s31

custl2 <- data.frame(matrix(nrow = length(custl$time),ncol = 7))
colnames(custl2) <- c("t", "s0", "s1", "s2", "s3", "s4", "s5")
custl2$t <- custl$time
custl2$s0 <- custl$y1
custl2$s1 <- custl$y2 + custl$y3 + custl$y4
custl2$s2 <- custl$y5 + custl$y6 + custl$y7 + custl$y8 + custl$y9
custl2$s3 <- custl$y10 + custl$y11 + custl$y12 + custl$y13 + custl$y14
custl2$s4 <- custl$y15 + custl$y16 + custl$y17
custl2$s5 <- custl$y18

pdf("./sis_cust_0.pdf")
library(ggplot2)
ggplot(df2, aes(t, s0)) +
  geom_point(aes(colour = "Simulation"), size = 0.5, alpha = 0.05) +
  geom_line(data = custl2, aes(x=t, y=s0 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  scale_color_manual(values=c("#990000", "#000000")) +
  theme_bw() +
  theme(legend.position="none")
dev.off()

pdf("./sis_cust_1.pdf")
library(ggplot2)
ggplot(df2, aes(t, s1)) +
  geom_point(aes(colour = "Simulation"), size = 0.5, alpha = 0.05) +
  geom_line(data = custl2, aes(x=t, y=s1 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  scale_color_manual(values=c("#990000", "#000000")) +
  theme_bw() +
  theme(legend.position="none")
dev.off()

pdf("./sis_cust_2.pdf")
library(ggplot2)
ggplot(df2, aes(t, s2)) +
  geom_point(aes(colour = "Simulation"), size = 0.5, alpha = 0.05) +
  geom_line(data = custl2, aes(x=t, y=s2 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  scale_color_manual(values=c("#990000", "#000000")) +
  theme_bw() +
  theme(legend.position="none")
dev.off()

pdf("./sis_cust_3.pdf")
library(ggplot2)
ggplot(df2, aes(t, s3)) +
  geom_point(aes(colour = "Simulation"), size = 0.5, alpha = 0.05) +
  geom_line(data = custl2, aes(x=t, y=s3 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  scale_color_manual(values=c("#990000", "#000000")) +
  theme_bw() +
  theme(legend.position="none")
dev.off()

pdf("./sis_cust_4.pdf")
library(ggplot2)
ggplot(df2, aes(t, s4)) +
  geom_point(aes(colour = "Simulation"), size = 0.5, alpha = 0.05) +
  geom_line(data = custl2, aes(x=t, y=s4 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  scale_color_manual(values=c("#990000", "#000000")) +
  theme_bw() +
  theme(legend.position="none")
dev.off()

pdf("./sis_cust_5.pdf")
library(ggplot2)
ggplot(df2, aes(t, s5)) +
  geom_point(aes(colour = "Simulation"), size = 0.5, alpha = 0.05) +
  geom_line(data = custl2, aes(x=t, y=s5 , colour="Numerical")) +
  xlab("Time") +
  ylab("P(s)") +
  scale_color_manual(values=c("#990000", "#000000")) +
  theme_bw() +
  theme(legend.position="none")
dev.off()

