### A piper diagram based on the ternary plot example here: http://srmulcahy.github.io/2012/12/04/ternary-plots-r.html
### Original author:
### Jason Lessels jlessels@gmail.com

### This now consists of five functions. 
### - transform_piper_data transforms the data to match the coordinates of the piper diagram.
### - ggplot_piper_blank sets up the diamond and the triangle plots
### - grid_function returns a list of which the first component is the ggplot object
###   The other components are the tibbles with the grid coordinates
### - label_function creates all the labels using the output of grid_function
### - ggplot_piper combines all the three plotting functions

#######################################
#' transform_piper_data
#' 
#' Function to transform vectors of Mg, Ca, Cl and SO4 data into
#' triangular coordinates
#' Original code Jason Lessels 
#' adapted by Willem Vervoort
#' @param Mg vector of Magnesium data in %
#' @param Ca vector of Calcium data in %
#' @param Cl vector of Chloride data in %
#' @param SO4 vector of Sulfate data in %
#' @param name names of the data points
#' @example 
#' 
#' @export
#' 
transform_piper_data <- function(Mg, Ca, Cl, SO4, offset = 120, name=NULL){
  if(is.null(name)){
    name = 1:length(Mg)
  } 
  # data needs to be in %
  #browser()
  # Peeters (2014) https://ngwa.onlinelibrary.wiley.com/doi/abs/10.1111/gwat.12118
  # page 4 does not work
  h <- 0.5*tan(pi/3)
  # y1 <- Mg * h
  # x1 <- (0.5*(2*(1 - Ca) + (Mg)))
  # y2 <- SO4*h
  # x2 <- 100*(1 + 2*offset + 0.5*(2*Cl + SO4))
  # # jlessels original code
  y1 <- Mg * h
  x1 <- 100*(1-(Ca/100) - (Mg/200))
  y2 <- SO4 * h
  x2 <-offset+(100*Cl/100 + 0.5 * 100*SO4/100)
  cations <- tibble(observation = name, x = x1, y = y1, type = "cations")
  anions <- tibble(observation = name, x = x2, y = y2, type = "anions")
  # Peeters equation page 4
  # but note that equation on page 4 is different from implementation in python 
  # code in supplementary material.
  # the code here follows the supplementary material
  diamond_point <- function(x_cat, x_an, y_cat, y_an, h = 0.5*tan(pi/3)) {
    x_d <- 1/(4*h)*(y_an - y_cat) + 0.5*(x_an + x_cat)
    y_d <- 0.5*(y_an + y_cat) + h*(x_an - x_cat)
    return(tibble(x = x_d, y = y_d, type = "diamond"))
  }
  # Original jlessels code
  # new_point <- function(x1, x2, y1, y2, grad=tan(pi/3)){
  #   b1 <- y1-(grad*x1)
  #   b2 <- y2-(-grad*x2)
  #   M <- matrix(c(grad, -grad, -1,-1), ncol=2)
  #   intercepts <- as.matrix(c(b1,b2))
  #   t_mat <- -solve(M) %*% intercepts
  #   data.frame(x=t_mat[1,1], y=t_mat[2,1])
  # }
  # calculate all points in the diamond
  #browser()
  np_list <- lapply(1:length(x1), function(i) diamond_point(x1[i], x2[i], y1[i], y2[i]))
  npoints <- bind_rows(np_list)
  npoints <- npoints %>%
    mutate(observation = name)
  # export a data frame with all points stacked
  data.frame(bind_rows(cations, anions, npoints))
}

#######################################
#' ggplot_piper_blank
#' 
#' plotting function for empty piper plot
#' Original code Jason Lessels 
#' adapted by Willem Vervoort. 
#' clarifying the different hard coded values
#' Can use more clean up so the plot is more scalable
#' Now only generates the blank plot
#' @param off_set x-offset for the right triangle 
#' @param d_off y-offset for the diamond
#' @example 
#' 
#' @export
#' 
ggplot_piper_blank <- function(off_set = 120, d_off = 20) {
  require(ggplot2)
  
  h = 0.5*tan(pi/3)

  
  # make the blank plots
  p <- ggplot() +
    
    ## left hand ternary plot
    geom_segment(aes(x =  0, y =  0,     xend = 100, yend = 0)) +
    geom_segment(aes(x =  0, y =  0,     xend =  50, yend = 100*h)) + 
    geom_segment(aes(x = 50, y = 100*h, xend = 100, yend = 0)) + 
    
    ## right hand ternary plot: offset = 120, i.e. 20 units to the right of right corner left plot
    geom_segment(aes(x = off_set, y = 0, xend = off_set + 100, yend =  0)) +
    geom_segment(aes(x = off_set, y = 0, xend = off_set + 50, yend = 100*h)) +
    geom_segment(aes(x = off_set + 50, y = 100*h, xend = off_set + 100, yend = 0)) +
    
    ## Upper diamond (see description above) d_off = 20*h in the y_direction
    # 110 is the x-off set, halfway between
    geom_segment(aes(x = 110, y = (200 + d_off)*h, 
                     xend =  60, yend = (100 + d_off)*h)) + #left angle top /
    geom_segment(aes(x = 110, y = (200 + d_off)*h, 
                     xend = 160, yend = (100 + d_off)*h)) + # right angle top \
    geom_segment(aes(x = 110, y =  d_off*h, 
                     xend = 160, yend = (100 + d_off)*h)) + #right angle bottom /
    geom_segment(aes(x = 110, y =  d_off*h, 
                     xend =  60, yend = (100 + d_off)*h)) + # left angle bottom \
# specify theme and remove any grids and axis elements
   theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.border = element_blank(), axis.ticks = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.title.x = element_blank(), axis.title.y = element_blank())
  return(p)
}


#####################################
#' grid_function
#' 
#' grid_function returns a list of which the first component is the ggplot object
#' The other components are the grid definition tibbles
#' @param ggplot_object a ggplot object 
#' @param off_set off_set for right triangle
#' @param h y coordinate of top of left triangle
#' @return a list of which the first component is the ggplot object, the other components are the grid definition tibbles
#' @example 
#' 
#' @export
#' 
grid_function <- function(ggplot_obj = p, off_set  = 120, h = 0.5*tan(pi/3)) {
  
  
  # set up the grid lines in the ternary plots and diamond
  # left hand triangle
  grid1p1 <<- tibble(x1 = seq(20,80, by = 20)) %>% # x values for bottom grid lines
    mutate(x2 = 0.5*x1, # x values for top grid lines
           y1 = rep(0, 4), # y values bottom grid lines
           y2 = x1*h) ## top of grid line y-value
  # offset the right triangle grid by off_set
  grid2p1 <<- grid1p1
  grid2p1$x1 <- grid2p1$x1 + off_set
  grid2p1$x2 <- grid2p1$x2 + off_set
  
  # the diamond is centered on 110 with corners on 60 and 160 on the x-axis
  # the bottom point is at y = 20*h, which means the y value for the corners is h*120
  # similarly, the y value for the top corner is h*220
  # y = 20*h is exactly 10 away horizontally from the triangle sides
  grid3p1 <<- tibble(x1 = c(100, 90, 80, 70)) %>% #left side bottom
    mutate(y1 = c(40,60,80,100)*h,
           x2 = x1 + 50) %>% # end of the segment on x 50 to the right
    mutate(y2 = (y1/h + 100)*h) # end of segment is 100 up in y
  # define the second half of the grid for the diamond
  grid3p2 <<- tibble(x1 = sort(grid3p1$x2)) %>% #right side bottom 50 to the right in x
    mutate(y1 = grid3p1$y1, # ends at the same spot as p1 grid y1 
           x2 = sort(grid3p1$x1), # essentially the same as where grid 1 starts
           y2 = grid3p1$y2) # ends on same spot as p1 grid y2
  
 # browser()
  ## Add grid lines to the plots
  # grid lines are: 
  # angle left ((x1,y1):(x2,y2))
  # angle right ((x1,y1):((100 - x2),y2))
  # horizontal ((x2,y2):((100 - x2),y2))
  p1 <- ggplot_obj +  geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2), data = grid1p1, #left triangle
                       linetype = "dashed", linewidth = 0.25, colour = "grey50") +
    geom_segment(aes(x = sort(x1, decreasing = T), y = y1, yend = sort(y2), 
                     xend = 100 - x2), data = grid1p1, 
                 linetype = "dashed", linewidth = 0.25, colour = "grey50") +
    geom_segment(aes(x = x2, y = y2, yend = y2, xend = 100 - x2), data = grid1p1, # horizontal lines
                 linetype = "dashed", linewidth = 0.25, colour = "grey50") +
    geom_segment(aes(x = x1, y = y1, yend = y2,  #right triangle
                     xend = x2), data = grid2p1,  
                 linetype = "dashed", linewidth = 0.25, colour = "grey50") +
    geom_segment(aes(x = sort(x1, decreasing = T), y = y1,  # right triangle
                     yend = y2, xend = sort(x1, decreasing = T) + grid1p1$x2), data = grid2p1, 
                 linetype = "dashed", linewidth = 0.25, colour = "grey50") +
    geom_segment(aes(x = x2, y = y2, yend = y2, 
                     xend = sort(x1, decreasing = T) + grid1p1$x2), data = grid2p1, 
                 linetype = "dashed", linewidth = 0.25, colour = "grey50") +
    # diamond grid
    geom_segment(aes(x = x1, y = y1, yend = y2, xend = x2), data = grid3p1,  # diamond
                 linetype = "dashed", linewidth = 0.25, colour = "grey50") +
    geom_segment(aes(x = x1, y = y1, yend = y2, 
                     xend = x2), data = grid3p2, 
                 linetype = "dashed", linewidth = 0.25, colour = "grey50")
#browser()
    return(list(plot_obj = p1, grid1 = grid1p1, grid2 = grid2p1, grid3_1 = grid3p1,
                grid3_2 = grid3p2))    
}

######################################
#' label_function
#' 
#' Function to label the axes in the piper plot
#' @param plot_obj_list list which is the output from grid_function
#' @param label.size size of the labels
#' @param nudge value to nudge the labels on the axes
#' @returns a ggplot object with labels and grids
#' @example 
#' 
#' @export
label_function <- function(plot_obj_list, label.size = 4, nudge = 5) {
  p <- plot_obj_list
  #browser()
  #### Labels and grid values
  p2 <- p$plot_obj + coord_fixed(ratio = 1) +  
    geom_text(aes(15,   50, label = "Mg^'2+'"), angle = 60, size = label.size, 
              parse = TRUE) +  
    geom_text(aes(83.5, 50, label = "Na^'+'~+~K^'+'"), angle = -60, 
              size = label.size, parse = T) +
    geom_text(aes(50,  -14, label = "Ca^'2+'"), size = label.size, 
              parse = TRUE) +
    geom_text(aes(170,   -14, label = "Cl^'-'"), size = label.size, 
              parse = TRUE) +
    geom_text(aes(205,    50, label = "SO[4]^'2-'"), angle = -60, 
              size = label.size, parse = TRUE) +
    geom_text(aes(137,  50, label = "Alkalinity~as~HCO[3]^'-'"), angle = 60, 
              size = label.size, parse = TRUE) +
    geom_text(aes( 72.5, 150, label = "SO[4]^'2-'~+~Cl^'-'"), angle = 60, 
              size = label.size, parse = TRUE) +
    geom_text(aes(147.5, 150, label = "Ca^'2+'~+~Mg^'2+'"), angle = -60, 
              size = label.size, parse = TRUE) + 
    
    geom_text(aes(p$grid1$x2, p$grid1$y2, label = c(80, 60, 40, 20)), 
              nudge_x = -nudge, 
              size = label.size -1, angle = 0) + # Mg axis
    geom_text(aes(100 - p$grid1$x2, sort(p$grid1$y2), label = c(20, 40, 60, 80)), 
              nudge_x = nudge, 
              size = label.size -1, angle = 0) + # Na axis  , vjust = -1, hjust = 0
    geom_text(aes(p$grid1$x1, p$grid1$y1, label = c(80, 60, 40, 20)), 
              nudge_y = -nudge, 
              size = label.size -1, angle = -60 ) + # Ca axis , vjust = -.5
    geom_text(aes(p$grid2$x2, p$grid2$y2, label = c(20, 40, 60, 80)),  
              nudge_x = -nudge, 
              size = label.size -1, angle = 0) + # HCO3 axis  c(155, 145, 135, 125), vjust = -1, hjust = 1
    geom_text(aes(sort(p$grid2$x2, decreasing = T) + 50, p$grid2$y2, 
                  label = c(20, 40, 60, 80)), nudge_x = nudge,
              size = label.size -1, angle = 0) + # SO4 axis c(215, 205, 195, 185)
    geom_text(aes(p$grid2$x1, p$grid1$y1, label = c(20, 40, 60, 80)),
              size = label.size -1, angle = 60, nudge_y = -nudge) + # Cl axis
    geom_text(aes(p$grid3_1$x2, p$grid3_1$y2, label = c(20, 40, 60, 80)), 
              nudge_y = 0.5*nudge,nudge_x = 0.5*nudge,
              size = label.size -1, angle =  60, vjust = 0, hjust = 0) + # diamond Ca axis
    geom_text(aes(p$grid3_2$x2, p$grid3_2$y2, label = c(20, 40, 60, 80)), 
              nudge_x = -nudge, nudge_y = nudge,
              size = label.size -1, angle = -60, vjust = 0, hjust = 0) # diamond SO4 axis
    
  return(p2)
    
}

#########################################
#' piper_ggplot
#' 
#' piper plot function to put everything together: plot, grid and labels
#' @param label_size size of the axes labels
#' @param nudge_out value to adjust the axes labels in or out
#' @returns a full ggplot piper plot
ggplot_piper <- function(label_size = 4, nudge_out = 5) {
  p <- ggplot_piper_blank()
  p1 <- grid_function(ggplot_obj = p)
  p2 <- label_function(plot_obj_list = p1, label.size = label_size,
                       nudge = nudge_out)
  return(p2)
}

# ## example test
# # ### A plan and simple piper diagram
# data=as.data.frame(list("Ca"=c(43,10,73,26,32),"Mg"=c(30,50,3,14,12),Cl=c(24,10,12,30,43),"SO4"=c(24,10,12,30,43),"WaterType"=c(2,2,1,2,3)),row.names=c("A","B","C","D","E"))
# #transform the data into piper based coordinates
# piper_data <- transform_piper_data(Ca=data$Ca, Mg = data$Mg, Cl=data$Cl, SO4= data$SO4, name=data$WaterType)
# # The piper function now just plots the background
# p <- ggplot_piper_blank()
# p
# # add the grid lines
# p1 <- grid_function(ggplot_obj = p)
# p1$plot_obj
# # add the labels
# p2 <- label_function(plot_obj_list = p1, label.size = 4)
# p2
# ## Or do it all at once
# ggplot_piper()
# # Now points can be added like...
# ggplot_piper() + geom_point(aes(x,y), data=piper_data)
# # colouring the points can be done using the observation value.
# ggplot_piper() + geom_point(aes(x,y, colour=factor(observation)), data=piper_data)
# # The size can be changed like..
# ggplot_piper() + geom_point(aes(x,y, colour=factor(observation)), size=4, data=piper_data)
# ## Change colours and shapes and merging the legends together
# ggplot_piper() + geom_point(aes(x,y, colour=factor(observation), shape=factor(observation)), size=4, data=piper_data) +
#   scale_colour_manual(name="legend name must be the same", values=c("#999999", "#E69F00", "#56B4E9"), labels=c("Control", "Treatment 1", "Treatment 2")) +
#   scale_shape_manual(name="legend name must be the same", values=c(1,2,3), labels=c("Control", "Treatment 1", "Treatment 2"))
