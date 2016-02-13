library(x3pr)
library(x3prplus)
library(dplyr)

subset_bullet <- function(fortified, minval = min(fortified$y), maxval = max(fortified$y)) {
    minval <- fortified$y[which.min(abs(minval - fortified$y))]
    maxval <- fortified$y[which.min(abs(maxval - fortified$y))]
    
    result <- fortified %>%
        filter(y >= minval, y <= maxval)
    
    attr(result, "info") <- length(unique(result$y))
    
    return(result)
}

subset_all_bullets <- function(paths) {
    test <- lapply(paths, function(path) {
        bullet <- fortify_x3p(read.x3p(path))
        subbed <- subset_bullet(bullet, min = 0, max = 500)
        
        return(unfortify_x3p(subbed))
    })
}

