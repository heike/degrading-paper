library(x3pr)
library(x3prplus)

processed <- processBullets(read.x3p("~/GitHub/imaging-paper/app/images/Hamby252_3DX3P1of2/Br1 Bullet 1-3.x3p"))
processed2 <- processBullets(read.x3p("~/GitHub/imaging-paper/app/degraded_images//Hamby252_3DX3P1of2/Br1_Bullet_2-5.x3p"))

smoothed <- bulletSmooth(processed)
smoothed2 <- bulletSmooth(processed2)
