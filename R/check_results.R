source("R/scripts.R")

x <- rnorm(10000)
y <- rnorm(10000)
mat <- cbind(x, y)

cats <- categorise(mat, 10)
cats

plot(cats)

vals <- predict(cats)

dt <- cbind(x, y, vals)

n_colors <- length(unique(dt[, 3]))
n_colors
rand_colors <- sample(colors(), n_colors, replace = FALSE)

ggplot(dt, aes(x, y, color = as.factor(vals))) +
    geom_point(size = 2, alpha = 0.5) +
    scale_color_manual(values = rand_colors) +
    theme_bw() +
    guides(color = "none") +
    labs(x = "PC1", y = "PC2")



library(terra)

r <- terra::rast(nrow = 10, ncol = 10)
values(r) <- rnorm(100)
x <- c(r, r**2)
plot(x)






