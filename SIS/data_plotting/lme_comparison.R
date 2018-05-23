t_max <- 20
th <- 1/3
library(deSolve)

#### K_3 ME ####
k3me <- function(t, y, parms) {
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
parms <- c(b=1, g=1)
times <- seq(from = 0, to = t_max, by = 0.01)
k3 <- ode (times = times, y = yini, func = k3me, parms = parms)
k3 <- as.data.frame(k3)

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

#### S_4 ME ####
s4me <- function(t, y, parms) {
  with(as.list(c(y,parms)), {
    dy1 <- g * (y[2] + y[3] + y[4] + y[5])
    dy2 <- g * (y[6] + y[7] + y[8]) - (3 * b + g) * y[2]
    dy3 <- g * (y[6] + y[9] + y[10]) - (b + g) * y[3]
    dy4 <- g * (y[7] + y[9] + y[11]) - (b + g) * y[4]
    dy5 <- g * (y[8] + y[10] + y[11]) - (b + g) * y[5]
    dy6 <- b * (y[2] + y[3]) + g * (y[12] + y[13]) - 2 * (b + g) * y[6]
    dy7 <- b * (y[2] + y[4]) + g * (y[12] + y[14]) - 2 * (b + g) * y[7]
    dy8 <- b * (y[2] + y[5]) + g * (y[13] + y[14]) - 2 * (b + g) * y[8]
    dy9 <- g * (y[12] + y[17]) - 2 * (b + g) * y[9]
    dy10 <- g * (y[13] + y[17]) - 2 * (b + g) * y[10]
    dy11 <- g * (y[14] + y[17]) - 2 * (b + g) * y[11]
    dy12 <- b * (y[6] + y[7] + 2 * y[9]) + g * y[16] - (b + 3 * g) * y[12]
    dy13 <- b * (y[6] + y[8] + 2 * y[10]) + g * y[16] - (b + 3 * g) * y[13]
    dy14 <- b * (y[7] + y[8] + 2 * y[11]) + g * y[16] - (b + 3 * g) * y[14]
    dy15 <- g * y[16] - (b + 3 * g) * y[15]
    dy16 <- b * (y[12] + y[13] + y[14] + 3 * y[15]) - 4 * g * y[16]
    list(c(dy1, dy2, dy3, dy4, dy5, dy6, dy7, dy8, dy9, dy10, dy11, dy12, dy13, dy14, dy15, dy16))
  })
}
yini <- c(y1 = 0, y2 = 0.25, y3 = 0.25, y4 = 0.25, y5 = 0.25, y6 = 0, y7 = 0, y8 = 0, y9 = 0, y10 = 0, y11 = 0, y12 = 0, y13 = 0, y14 = 0, y15 = 0, y16 = 0)
parms <- c(b=1, g=1)
times <- seq(from = 0, to = t_max, by = 0.01)
s4 <- ode (times = times, y = yini, func = s4me, parms = parms)
s4 <- as.data.frame(s4)

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




