data <- data.frame(
  x = c(1, 2, 2, 1, 2, 3, 3, 2, 3, 4, 4, 3, 1, 2, 2, 1),
  y = c(1, 3, 4, 3, 3, 1, 2, 5, 1, 3, 2, 2, 6, 5, 4, 5),
  group = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4)
)

data <- data[order(data$group, data$x, data$y), ]
lower <- data[c(TRUE, FALSE, TRUE, FALSE), ]
upper <- data[c(FALSE, TRUE, FALSE, TRUE), ]

add_controls <- function(data, strength) {
  start <- data[c(TRUE, FALSE), ]
  end <- data[c(FALSE, TRUE), ]
  x_diff <- (end$x - start$x) * strength
  mid1 <- start
  mid1$x <- mid1$x + x_diff
  mid2 <- end
  mid2$x <- mid2$x - x_diff
  rbind(start, mid1, mid2, end)
}

strength = 0.5

plot(data$x, data$y, col = data$group)
lower2 <- add_controls(lower, strength)
upper2 <- add_controls(upper, strength)

total_n <- 100

points(lower2$x, lower2$y, col = lower2$group, pch = 4)
lower2 <- lower2[order(lower2$group), ]
upper2 <- upper2[order(upper2$group), ]
a <- ggforce:::getBeziers(lower2$x, lower2$y, id = lower2$group, detail = total_n)
b <- ggforce:::getBeziers(upper2$x, upper2$y, id = upper2$group, detail = total_n)

pal <- viridis::inferno(total_n)

df <- data.frame()

for(j in 1:length(unique(a$pathID))){
  
  u <- a$paths[a$pathID==j,1:2]
  l <- b$paths[b$pathID==j,1:2]
  
  for(i in 1:(length(l[,1])-1)){
    n <- i
    n_ <- n + 1
    polygon(
      c(l[n:n_,1], rev(u[n:n_,1])),
      c(l[n:n_,2], rev(u[n:n_,2])),  
      col =  pal[n],
      border = NA
    )
    
    df <- rbind(df, 
                data.frame(
                  x = c(l[n:n_,1], rev(u[n:n_,1])),
                  y = c(l[n:n_,2], rev(u[n:n_,2])),
                  poly_n = rep((n-1)+(j-1)*(n-1),4),
                  group_n = j))
    
  }
}

library(magrittr)
library(ggplot2)

df %>% 
  ggplot(aes(x = x, y = y, group = interaction(poly_n, group_n))) + 
  geom_polygon(aes(fill = factor(poly_n), colour = factor(poly_n)), show.legend = F)
