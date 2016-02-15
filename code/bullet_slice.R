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
        subbed <- subset_bullet(bullet, min = 1000, max = 2000)
        
        unfort <- unfortify_x3p(subbed)
        attr(unfort, "path") <- path
        
        return(unfort)
    })
}

paths <- file.path("~/GitHub/imaging-paper/app/images/Hamby252_3DX3P1of2", 
                   dir("~/GitHub/imaging-paper/app/images/Hamby252_3DX3P1of2"))
all_subsets <- subset_all_bullets(paths)

file.path <- system.file("extdata", "glock.x3p", package="x3pr")
glock.x3p.info <- read.x3p(file.path)

this.header.info<-list(
    surf.type      =   "SUR",
    x.axis.type    = "I",
    x.data.type    = "F",
    xinc           = glock.x3p.info[[1]]$x.inc*1e-6,
    x.offset       = 0.0,
    y.axis.type    = "I",
    y.data.type    = "F",
    yinc           = glock.x3p.info[[1]]$y.inc*1e-6,
    y.offset       = 0.0,
    z.axis.type    = "A",
    z.data.type    = "F",
    zinc           = "NA",
    z.offset       = 0.0,
    who.wrote.file = "abe[-at-]potus.gov",
    manufacturer   = "Leitz",
    model          = "Model-A",
    sn             = "A113",
    vers           = "Version: 2001-C.E.",
    cal.dte        = "2013-08-15T08:00:00-03:00",
    probe.type     = "NonContacting",
    probe.id       = "Interferometer",
    meas.comment   = comment.block<-list(
        paste("Surface is from: "," an object, ",sep=""),
        paste("Surface category: ","It was the left chunk, ",sep=""),
        paste("Surface sample#: ",3,", ",sep=""),
        paste("Droupouts/Outliers filled with ..., ",sep=""),
        paste("Estimated Resolution x (meters) ", glock.x3p.info[[1]]$x.inc*1e-6, ", ",sep=""),
        paste("Estimated Resolution y (meters) ", glock.x3p.info[[1]]$y.inc*1e-6, ", ",sep=""),
        paste("Estimated Resolution z (meters) ", "Not Applicable, z-heights stored as doubles.", ", ",sep=""),
        paste("Form removed?: ", "YesOrNo, ",sep=""),
        paste("Filter ", "Type: None/", "Cutoffs: None, ",sep=""),
        paste("Microscope Objective: ", "50x/","0.8NA, ",sep=""),
        paste("Invalid Pixel Value: ", NaN,sep="")),
    pts.per.prof   = glock.x3p.info[[1]]$num.pts.line,
    num.prof       = glock.x3p.info[[1]]$num.lines,
    z.format       = 1)


dir.create("~/GitHub/imaging-paper/app/degraded_images/Hamby252_3DX3P1of2", recursive = TRUE)
lapply(all_subsets, function(sub) {
    myfname <- basename(attr(sub, "path"))
    myfname <- gsub(" ", "_", myfname)
    
    this.header.info$num.prof <- sub[[1]]$num.lines
    this.header.info$pts.per.prof <- sub[[1]]$num.pts.line
    this.header.info$xinc <- sub[[1]]$x.inc*1e-6
    this.header.info$yinc <- sub[[1]]$y.inc*1e-6
                        
    write.x3p(this.header.info, sub$surface.matrix, fname = myfname,
              move.to.directory = "~/GitHub/imaging-paper/app/degraded_images/Hamby252_3DX3P1of2")
})
