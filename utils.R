library(glue)
library(plyr)
library(tidyverse)

pythagoras <- function(a, b, c) {
  
  if (missing(a)) {
    out <- sqrt(c^2 - b^2)
  } else if (missing(b)) {
    out <- sqrt(c^2-a^2)
  } else if (missing(c)) {
    out <- sqrt(a^2 + b^2)
  } else {
    out <- NULL
  }
  
  out
}

h = 8
w1 = 6  
w2 = 10
l1 = 22
l2 = 24
z =  3.1
x = .25
s = .8


# h  : height
# w1 : width at top
# w2 : width at base
# l1 : length at top
# l2 : length at base
# z  : zipper width
# x  : zipper exent (0-100%)
# s  : seam allowance
make_pattern <- function(h, w1, w2, l1, l2, z, x, s, accuracy = .1) {

  
  # internal trigonometry (see figure for reference)
  d <- abs(l1 - l2) / 2
  t <- pythagoras(a = d, b = h)
  a1 <- t * x
  a2 <- t * (1 - x)
  b2 <- abs(w1 - w2) / 2
  v <- atan(t / b2)
  cc <- pythagoras(a = t, b = b2)
  c1 <- a1 / sin(v)
  c2 <- cc - c1
  b1 <- a1 / tan(v)
  a <- (w1 / 2) - (z / 2) + (2 * s)
  
  # side lengths
  A <- (w2 / 2) + h + a
  B <- (l1 / 2) + (t * x) + s
  C <- a + (b1 * ifelse(w1 > w2, -1, 1))
  D <- (w1 / 2) + b1
  E <- (l2 / 2) + (t * (1 - x)) + s
  
  # point coordinates
  alpha_x <- (l1 / 2) + s
  alpha_y <- (w2 / 2) + h
  beta_x <- (l2 / 2) + s
  beta_y <- (w2 / 2)
  
  # volume
  # pyramidal frustum (http://mathworld.wolfram.com/PyramidalFrustum.html)
  V <- (1/3) * h * ((w1*l1) + (w2*l2) + sqrt((w1*l1) * (w2*l2))) / 1000
  
  
  p <- list(A = A, 
            B = B, 
            C = C, 
            D = D, 
            E = E, 
            alpha_x = alpha_x, 
            alpha_y = alpha_y, 
            beta_x = beta_x, 
            beta_y = beta_y,
            V = V, 
            w1 = w1, 
            w2 = w2, 
            l1 = l1, 
            l2 = l2, 
            h = h, 
            x = x * 100,
            s = s)
  
  map(p, ~ round_any(., accuracy))
}

plot_pattern <- function(p, name) {
  
  o_y <- p$beta_y + (p$h / 2) + 2
  o_x <- mean(p$alpha_x, p$beta_x) / 2
  
  ggplot() +
    geom_line(aes(x = c(0, 0), y = c(0,p$A))) +
    geom_line(aes(x = c(0, p$B - p$s), y = c(p$A, p$A))) +
    geom_line(aes(x = c(p$alpha_x, p$B - p$s), y = c(p$alpha_y, p$A - p$C))) +
    geom_line(aes(x = c(0, p$alpha_x), y = c(p$alpha_y, p$alpha_y)), linetype = "dashed") +
    geom_line(aes(x = c(p$alpha_x, p$alpha_x), y = c(p$alpha_y, p$A)), linetype = "dashed") +
    geom_line(aes(x = c(0, p$beta_x), y = c(p$beta_y, p$beta_y)), linetype = "dashed") +
    geom_line(aes(x = c(p$beta_x, p$beta_x), y = c(p$beta_y, 0)), linetype = "dashed") +
    geom_line(aes(x = c(p$beta_x, p$alpha_x), y = c(p$beta_y, p$alpha_y)))  +
    geom_line(aes(x = c(p$beta_x, p$E - p$s), y = c(p$beta_y, p$D))) +
    geom_line(aes(x = c(0, p$E - p$s), y = c(0, 0))) +
    geom_rect(aes(xmin = p$B - p$s, xmax = p$B, ymin = p$A - p$C, ymax = p$A), linetype = "dotted", color = "black", alpha = 0) +
    geom_rect(aes(xmin = p$E - p$s, xmax = p$E, ymin = 0, ymax = p$D), linetype = "dotted", color = "black", alpha = 0) +
    geom_point(aes(x = c(p$alpha_x, p$beta_x), y = c(p$alpha_y, p$beta_y)), size = 4) +
    geom_point(aes(x = c(p$alpha_x, p$beta_x), y = c(p$alpha_y, p$beta_y)), size = 1, color = "white") +
    annotate("text", x = .25, y = p$A / 2, label = p$A, hjust = 0) +
    annotate("text", x = p$B / 2, y = p$A - .35, label = p$B, hjust = .5) +
    annotate("text", x = p$B - .25, y = p$A - (p$C / 2), label = p$C, hjust = 1) +
    annotate("text", x = p$alpha_x - .25, y = p$alpha_y + ((p$A - p$alpha_y) / 2), label = p$A - p$alpha_y, hjust = 1) +
    annotate("text", x = p$alpha_x  / 2, y = p$alpha_y + .35, label = p$alpha_x, hjust = .5) +
    annotate("text", x = p$E - .25, y = p$D / 2, label = p$D, hjust = 1) +
    annotate("text", x = p$E / 2, y = 0 + .35, label = p$E, hjust = .5) +
    annotate("text", x = p$beta_x / 2, y = p$beta_y + .35, label = p$beta_x, hjust = .5) +
    annotate("text", x = p$beta_x - .25, y = p$beta_y / 2, label = p$beta_y, hjust = 1) +
    annotate("text", x = p$beta_x / 2, y = o_y - 1, label = glue("bold('{name}')"), hjust = .5, parse = T, size = 6) +
    annotate("text", x = p$beta_x / 2, y = o_y - 1.8, label = glue("H:{p$h} W:{p$w1}/{p$w2} L:{p$l1}/{p$l2}"), hjust = .5) +
    annotate("text", x = p$beta_x / 2, y = o_y - 2.6, label = glue("R:{p$x}% S:{p$s}"), hjust = .5) +
    annotate("text", x = p$beta_x / 2, y = o_y - 3.4, label = glue("Volume: {signif(p$V, 2)}L"), hjust = .5) +
    scale_y_continuous(limits = c(0, p$A)) +
    scale_x_continuous(limits = c(0, max(p$B, p$E))) +
    theme_void() +
    theme(plot.margin = unit(c(0,0,0,0), "cm"))
  
}

# pattern_old <- function(h, w1, w2, l1, l2, z, x, s) {
#   
#   # internal lengths
#   a <- (w1 / 2) - ((z / 2) - s) + s
#   t <- abs(w2 - w1) / 2
#   r <- x / (1 - x)
# 
#   # internal triangles
#   mid <- {}
#   mid$b <- abs(l1 - l2)
#   mid$h <- h
#   mid$s <- pythagoras(a = mid$b, b = h)
# 
#   v <- atan(mid$h/t)
# 
#   upper <- {}
#   upper$h <- h * x
#   upper$s <- (x * h) / sin(v)
#   upper$b <- pythagoras(a = upper$h, c = upper$s)
# 
#   lower <- {}
#   lower$h <- h * (1 - x)
#   lower$s <- mid$s - upper$s
#   lower$b <- t - upper$b
# 
# 
#   # points
#   alpha <- {}
#   alpha_x <- (l1 / 2) + s
#   alpha$z <- a
# 
#   beta <- {}
#   beta_x <- (l2 / 2) + s
#   beta_y <- (w2 / 2)
# 
#   # lengths
#   A <- round_any((w2 / 2) + h + a, .1)
#   B <- round_any((l1 / 2) + (h * x) + s, .1)
#   C <- round_any(a + (upper$b  * ifelse(w1 > w2, -1, 1)), .1)
#   D <- round_any((w2 / 2) + (lower$b  * ifelse(w1 > w2, 1, -1)), .1)
#   E <- round_any((l2 / 2) + (h * (1- x )) + s, .1)
# 
#   # volume
#   # pyramidal frustum (http://mathworld.wolfram.com/PyramidalFrustum.html)
#   V <- (1/3) * h * ((w1*l1) + (w2*l2) + sqrt((w1*l1) * (w2*l2))) / 1000
#   
#   list(A = A, B = B, C = C, D = D, E =E, alpha = alpha, beta = beta, V = V, s = s, w1 = w1, w2 = w2, l1 = l1, l2 = l2, h = h, x = x)
#   
# }

