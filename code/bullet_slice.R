library(x3pr)
library(x3prplus)
library(dplyr)

subset_signature <- function(signature, minval = min(signature$y), maxval = max(signature$y)) {
    minval <- signature$y[which.min(abs(minval - signature$y))]
    maxval <- signature$y[which.min(abs(maxval - signature$y))]
    
    signature %>%
        filter(y >= minval, y <= maxval)
}

path <- "~/GitHub/imaging-paper/app/images/Hamby252_3DX3P1of2/Br1 Bullet 1-1.x3p"
signature <- get_crosscut(path)
subbed <- subset_signature(signature, min = 0, max = 100)
