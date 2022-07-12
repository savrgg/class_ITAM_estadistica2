x <- c(10, 12, 15, 5, 7, 5, 5, 15, 8, 1, 13, 15)
y <- c(30, 60, 50, 12, 10, 25, 10, 60, 25, 10, 75, 95)

ex <- 111/12
mean(x)
vx <- (1277-12*ex*ex)/11
var(x)

ey <- 462/12
mean(y)
vy <- (26944-12*ey*ey)/11
var(y)

cxy <- (5585-12*ex*ey)/11
cov(x,y)

cxy/(sqrt(vy)*sqrt(vx))
r1 <- cor(x,y)



y <- c(10, 12, 15, 5, 7, 5, 5, 15, 8, 1)/100
x <- c(30, 60, 50, 12, 10, 25, 10, 60, 25, 10)/1000

ex <- .292/10
mean(x)
vx <- (.012294-10*ex*ex)/9
var(x)

ey <- 0.83/10
mean(y)
vy <- (0.0883-10*ey*ey)/9
var(y)

cxy <- (.03185-10*ex*ey)/9
cov(x,y)

cxy/(sqrt(vy)*sqrt(vx))
r2 <- cor(x,y)


r1<-0.8664
5.4867
pt(5.4867, df = 10, lower.tail = F)
(r1*sqrt(12-2))/sqrt(1-r1*r1)


r2<-0.8904
5.5328
(r2*sqrt(10-2))/sqrt(1-r2*r2) 




