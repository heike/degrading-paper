library(x3pr)
library(x3prplus)
library(dplyr)

subset_bullet <- function(fortified, minval = min(fortified$y), maxval = max(fortified$y)) {
    minval <- fortified$y[which.min(abs(minval - fortified$y))]
    maxval <- fortified$y[which.min(abs(maxval - fortified$y))]
    
    result <- fortified %>%
        filter(y >= minval, y <= maxval)
    
    attr(result, "info")$num.lines <- length(unique(result$y))
    
    return(result)
}

subset_all_bullets <- function(paths) {
    lapply(paths, function(path) {
        bullet <- fortify_x3p(read.x3p(path))
        subbed <- subset_bullet(bullet, min = 0, max = 500)
        
        unfort <- unfortify_x3p(subbed)
        attr(unfort, "path") <- path
        
        return(unfort)
    })
}

paths <- file.path("~/GitHub/imaging-paper/app/images/Hamby252_3DX3P1of2", 
                   dir("~/GitHub/imaging-paper/app/images/Hamby252_3DX3P1of2"))
all_subsets <- subset_all_bullets(paths)

dir.create("~/GitHub/imaging-paper/app/degraded_images/Hamby252_3DX3P1of2", recursive = TRUE)
lapply(all_subsets, function(sub) {
    myfname <- basename(attr(sub, "path"))
    myfname <- gsub(" ", "_", myfname)
                        
    write.x3p(sub$header.info, sub$surface.matrix, fname = myfname,
              move.to.directory = "~/GitHub/imaging-paper/app/degraded_images/Hamby252_3DX3P1of2")
})
