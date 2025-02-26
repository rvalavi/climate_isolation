library(terra)
library(ggplot2)
library(Rcpp)

# get the range of a raster or matrix
get_range <- function(x) {
    require(terra)
    if(is(x, "SpatRaster")) {
        if (all(x@ptr@.xData$hasRange)) {
            rng <-  cbind(
                x@ptr@.xData$range_min,
                x@ptr@.xData$range_max
            )
        } else {
            rng <- cbind(
                terra::global(x, fun = "min", na.rm = TRUE),
                terra::global(x, fun = "max", na.rm = TRUE)
            )
        }
    } else {
        rng <- t(apply(x, 2, range))
    }
    
    return(rng)
}


# model the climate categories
categorise <- function(x, n) {
    # get the range
    rng <- get_range(x)
    # make the sequence
    xr <- seq(rng[1,1], rng[1,2], length.out = n + 1)[-1]
    yr <- seq(rng[2,1], rng[2,2], length.out = n + 1)[-1]
    
    out <- list(
        x = xr, y = yr, n = n, 
        data = if(is(x, "SpatRaster")) NULL else x
    )
    class(out) <- "categorise"
    
    return(out)
}


predict.categorise <- function(model, newdata = NULL, ...) {
    # recursive function... run the same function in case of empty newdata
    if (is.null(newdata)) {
        if (is.null(model[["data"]])) stop("The newdata is not provided!")
        return(
            predict(model, model[["data"]])
        )
    }
    
    index <- function(i, j, n) {
        return((i - 1) * n + j)
    }
    
    xr <- model[[1]]
    yr <- model[[2]]
    n <- model[[3]]
    
    x <- newdata[, 1]
    y <- newdata[, 2]
    nr <- nrow(newdata)
    out <- integer(nr) # preallocate vector
    for (v in 1:nr) {
        pc1 <- x[v]
        pc2 <- y[v]
        
        i <- which(pc1 < xr)[1]
        j <- which(pc2 < yr)[1]
        
        if (is.na(i)) i <- n
        if (is.na(j)) j <- n
        
        out[v] <- index(i, j, n)
    }
    
    return(out)
}

print.categorise <- function(x, ...) {
    print(
        paste("Categorise calss:", x[[3]], "splits")
    )
}

plot.categorise <- function(x, ...) {
    dat <- x[["data"]][, 1:2]
    vals <- predict(x)
    rand_colors <- sample(colors(), length(unique(vals)), replace = FALSE)
    plot(
        ggplot(cbind(dat, vals), aes(x, y, color = as.factor(vals))) +
            geom_point(size = 2, alpha = 0.5) +
            scale_color_manual(values = rand_colors) +
            theme_bw() +
            guides(color = "none") +
            labs(x = "PC1", y = "PC2")
    )
}

