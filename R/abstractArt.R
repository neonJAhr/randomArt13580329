#' Random Art Function
#'
#' This function creates an image from randomly generated values.
#' More specifically, it creates a random amount of stars and starlines. 
#' The colors are dependent on the amount of starlines. 
#' @param seed Integer. Set a seed to make the picture reproducible. Defaults to a random value between 0 and 100000.
#' @param stars Integer. Defaults to a random value between 1 and 50.
#' @param starlines Integer. Defaults to a random value between 3 and 10. 
#' @keywords PiPS, Q3.2R.2, random_art
#' @export
#' @examples 
#' make_art(13580329)


make_art <- function(seed = runif(1,0,100000), 
                     stars = sample(1:50, 1, replace = TRUE), 
                     starlines = sample(3:10, 1, replace = TRUE)) {
# Initialize number of stars, starlines
  set.seed(seed)
  require(ggplot2)
  
  # Create the endpoints
  endpoints <- data.frame(x = runif(starlines, 0, 10000), #Paths + 1 for the initial point being the origin
                          y = runif(starlines, 0, 10000))
  
  #calculate the geometric mean
  geo_mean_x <- exp(mean(log(endpoints$x)))
  geo_mean_y <- exp(mean(log(endpoints$y)))
  
  connecting_lines <- data.frame(x = c(rep(geo_mean_x, starlines), endpoints$x),
                                 y = c(rep(geo_mean_y, starlines), endpoints$y))
  connecting_lines$grp <- rep(1:(starlines), times = 2)
  
  
  
  
  #Repeat
  for (i in 1:stars) {
    endpoints <- data.frame(x = runif(starlines, 0, 10000), #Paths + 1 for the initial point being the origin
                            y = runif(starlines, 0, 10000))
    
    geo_mean_x <- exp(mean(log(endpoints$x)))
    geo_mean_y <- exp(mean(log(endpoints$y)))
    
    connecting_lines[(nrow(connecting_lines) + 1):(nrow(connecting_lines) + starlines*2), 1] <- c(rep(geo_mean_x, starlines), endpoints$x)
    connecting_lines[(nrow(connecting_lines) - (starlines * 2 - 1) ):(nrow(connecting_lines) + starlines * 2 - starlines * 2), 2] <- c(rep(geo_mean_y, starlines), endpoints$y)
    
    connecting_lines[((starlines * 2 + 1)+(starlines * 2) *(i-1)):((starlines * 2) + starlines * 2 * i),3]  <- rep((1+starlines*i):(starlines+ starlines * i), times = 2)
  }
  
  connecting_lines$grp <- as.factor(connecting_lines$grp)
  
  
  
  
  # Graph the picture
  ggplot() +
    geom_line(aes(x,
                  y,
                  group = grp),
              data = connecting_lines, color = connecting_lines$grp,
              size = round(runif(1,1,3))) +
    xlim(0, 10000) +
    ylim(0, 10000) +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = "transparent", colour = "transparent"),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", colour = "transparent"),
          plot.margin = unit(rep(0, 4), "cm"), # top, right, bottom, left
          strip.background = element_blank(),
          strip.text = element_blank())

}