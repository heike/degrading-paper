library(x3pr)
library(x3prplus)
library(ggplot2)

bulletAlign_new <- function (data, value = "l30")  {
    bullet <- NULL
    b12 <- unique(data$bullet)
    if (length(b12) != 2) 
        stop("Two surfaces should be compared\n\n")
    data$val <- data.frame(data)[, value]
    miny <- min(data$y, na.rm = T)
    subLOFx1 <- subset(data, bullet == b12[1])
    subLOFx2 <- subset(data, bullet == b12[2])
    subLOFx1$y <- subLOFx1$y - min(subLOFx1$y)
    subLOFx2$y <- subLOFx2$y - min(subLOFx2$y)
    
    whichmin <- which.min(c(length(subLOFx1$val), length(subLOFx2$val)))
    minval <- min(c(length(subLOFx1$val), length(subLOFx2$val)))
    shorter <- list(subLOFx1$val, subLOFx2$val)[[whichmin]]
    longer <- list(subLOFx1$val, subLOFx2$val)[[3 - whichmin]]
    
    mycors <- NULL
    for (i in 1:(length(longer) - length(shorter))) {
        longersub <- longer[i:(i + length(shorter) - 1)]
        mycors <- c(mycors, cor(shorter, longersub, use = "pairwise.complete.obs"))
    }
    
    #ccf <- ccf(subLOFx1$val, subLOFx2$val, plot = FALSE, lag.max = 150, 
    #           na.action = na.omit)
    #lag <- ccf$lag[which.max(ccf$acf)]
    lag <- which.max(mycors)
    incr <- min(diff(sort(unique(subLOFx1$y))))
    subLOFx2$y <- subLOFx2$y + lag * incr
    bullets <- rbind(data.frame(subLOFx1), data.frame(subLOFx2))
    list(ccf = max(mycors), lag = lag * incr, bullets = bullets)
}

processed <- processBullets(read.x3p("~/GitHub/imaging-paper/app/images/Hamby252_3DX3P1of2/Br1 Bullet 1-3.x3p"))
processed2 <- processBullets(read.x3p("~/GitHub/imaging-paper/app/degraded_images//Hamby252_3DX3P1of2/Br1_Bullet_1-3.x3p"))

smoothed <- bulletSmooth(processed)
smoothed2 <- bulletSmooth(processed2)

mydat <- rbind(smoothed, smoothed2)
mydat$bullet <- c(rep("b1", nrow(smoothed)), rep("b2", nrow(smoothed2)))


aligned <- bulletAlign(mydat)
qplot(y, value, data = aligned$bullets, colour = bullet)

aligned <- bulletAlign_new(mydat)
qplot(y, value, data = aligned$bullets, colour = bullet)
