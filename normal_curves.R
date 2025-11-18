library(dplyr)
library(stringr)
library(tidyr)
library(ggpubr)
library(ggplot2)
library(latex2exp)

# Normal distribution demonstration plots
normal_distplot <- function(mu, sd) {
  # Plot figurative null hypothesis distribution
  ggplot() +
    stat_function(fun = dnorm,
                  args = list(mean = mu, sd = sd),
                  geom = "line") +
    xlim(-4 * sd, 4 * sd) +
    xlab(TeX("true difference, $\\theta$")) +
    ylab("density") +
    theme_minimal()
}

# Shade a normal curve between zstart and zend
shade_normal <- function(mu, sd, zstart, zend, fill="#00998a", alpha=0.5,
                         xlabels=TRUE, textoffset=0.01) {
  xmin <- qnorm(zstart, mu, sd)
  xmax <- qnorm(zend, mu, sd)
  
  # Mesh of distribution points
  data <- data.frame(x=seq(mu - 4 * sd, mu + 4 * sd, by=0.01)) %>%
    mutate(y=dnorm(x, mu, sd))
  
  # Return ggplot2 shaded area as list of <ggproto> objects
  area <- list(
    geom_area(data=subset(data, x >= xmin & x <= xmax),
              aes(x=x, y=y), fill=fill, color=NA, alpha=0.5)
  )
  labels <- list(
    annotate("segment", color=fill,
             x=xmin, xend=xmin,
             y=0, yend=dnorm(xmin, mu, sd)),
    annotate("text", color=fill,
            x=xmin, y=-textoffset,
            label=ifelse(zstart == 0, "", paste(100 * zstart, "%", sep=""))),
    annotate("segment", color=fill,
             x=xmax, xend=xmax,
             y=0, yend=dnorm(xmax, mu, sd)),
    annotate("text", color=fill,
             x=xmax, y=-textoffset,
             label=ifelse(zend == 1, "", paste(100 * zend, "%", sep="")))
  )
  
  if (xlabels) {
    c(area, labels)
  } else {
    area
  }
  
}

# Add a CI bar to a normal curve
ci_normal <- function(mu, sd, ci, alpha=0.4) {
  xmin <- qnorm(0.5 * (1 - ci), mu, sd)
  xmax <- qnorm(1 - 0.5 * (1 - ci), mu, sd)
  
  # Return ggplot2 annotation as list of<ggproto>s
  list(
    annotate("pointrange",
             xmin=xmin, xmax=xmax,
             x=mu, y=dnorm(xmax, mu, sd), alpha=alpha),
    annotate("text",
             x=mu, y=dnorm(xmax, mu, sd)-0.01,
             label=paste(100 * ci, "%CI", sep=""))
  )
}

# Add a dashed line marker at a given x position
add_x_marker <- function(x, y, label, linecolor, textcolor, textoffset=0.01) {
  list(
    annotate("segment", # show the observed difference as a dashed line
      x = x, xend = x,
      y = 0, yend = y,
      colour = linecolor,
      size = 1, linetype = "dashed"
    ),
    annotate("text", x = x, y = y + textoffset, label = label, color = textcolor)
  )
}

# Add shaded normal curve in specific colour with labels
shaded_normal <- function(mu, sd, zstart=0.05, zend=0.95,
                          fill="orange", color="orange", label="",
                          xlabels=TRUE,
                          alpha=0.5, textoffset=0.01, textyoffset=0) {
  list(
    stat_function(fun=dnorm, args=list(mean=mu, sd=sd), geom="line", colour=color),
    shade_normal(mu=mu, sd=sd, zstart=zstart, zend=zend, fill=fill, alpha=alpha, textoffset=textoffset, xlabels=xlabels),
    annotate("text", colour=color, x=mu + textyoffset, y=dnorm(mu, mu, sd) + 2 * textoffset, label=label),
    annotate("segment", colour=color, x=mu, xend=mu, y=0, yend=dnorm(mu, mu, sd))
  )
}
