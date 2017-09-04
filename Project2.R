
  library(raster) # to get map shape file
  library(ggplot2) # for plotting and miscellaneuous things
  library(ggmap) # for plotting
  library(plyr) # for merging datasets
  library(scales) # to get nice looking legends
  library(maps)
  library(mapdata)
  library(ggrepel)
  library(gridExtra)
  
  #get the shape file for the states, alaska and hawaii separately
  usa.statesshape = map_data("state")
  colnames(usa.statesshape)[5] <- "State"
  setwd('/home/ramya/Dropbox/Masters/First Semester/Statistical Methods for Data Science')
  usa.dat = read.table("clinton_chance_sep19_2016.csv", header = T, sep = ",")
  usa.dat$State = tolower(usa.dat$State)
  usa.trumpdata = usa.dat
  usa.trumpdata$Chance = 100-usa.trumpdata$Chance
  brks.to.use <- seq(0, 100, by = 20)
  usa.trumpshape = join(usa.statesshape, usa.trumpdata, by = "State", type = "inner")
  
  states = data.frame(state.center, state.abb)
  stateswithlines = c('VT','NH','MA','RI','CT','NJ','DE','MD')
  
  stateswithtextrepel <- states[states$state.abb %in% stateswithlines, ]
  stateswithouttextrepel <- states[!states$state.abb %in% stateswithlines, ]
  stateswithouttextrepel <- states[!states$state.abb %in% c('AK','HI'), ]
  p1 <- function(data, brks, title) {
    ggp <- ggplot() + 
      geom_polygon(data = data, aes(x = long, y = lat, group = group, 
                                    fill = Chance), color = "black", size = 0.15) + 
    
      scale_fill_distiller(palette = "RdBu", breaks = brks,
                           limits = c(min(data$Chance), max(data$Chance))) +
      theme_nothing(legend = TRUE) + labs(title = title, fill = "") +
      geom_text(data = stateswithouttextrepel, aes(x = x, y = y, label = state.abb), size = 3) +
      geom_text_repel(data = stateswithtextrepel, aes(x = x, y = y,label = state.abb), size = 3,box.padding = unit(.3, "lines"),
                point.padding = unit(.1, "lines"),nudge_x = 5,nudge_y = .5)
    return(ggp)
  }
  
  figure.title = "Elections 2016 - Trump vs Clinton"
  usa.al = map_data("world2Hires", "USA:Alaska")
  colnames(usa.al)[6] <- "State"
  usa.al$State = tolower(usa.al$State)
  
  
  usa.al.dat <- usa.dat[usa.dat$State == "alaska", c("State", "Chance")]
  
  usa.al <- join(usa.al, usa.al.dat, by = "State", type = "inner")
  
  stateswithalaska <- states[states$state.abb %in% 'AL', ]

  p2 <- function(data, brks) {
    ggp <- ggplot() + 
      geom_polygon(data = data, aes(x = long, y = lat, group = group, 
                                    fill = usa.al$Chance), color = "black", size = 0.15) + 
      scale_fill_distiller(palette = "RdBu", breaks = brks,
                           limits = c(min(usa.trumpshape$Chance), max(usa.trumpshape$Chance))) +
      theme_nothing(legend = FALSE) + labs(fill = "") +
      annotate("text", x = min(data$long)+ 30, y = min(data$lat) + 12, label = "AL")
    return(ggp)
  }
  usa.hi = map_data("world2Hires", "Hawaii")
  colnames(usa.hi)[5] <- "State"
  usa.hi$State = tolower(usa.hi$State)

  
  
  usa.hi.dat <- usa.dat[usa.dat$State == "hawaii", c("State", "Chance")]
  usa.hi <- join(usa.hi, usa.hi.dat, by = "State", type = "inner")
  stateswithhawaii <- states[states$state.abb %in% 'HI', ]
  p3 <- function(data, brks) {
    ggp <- ggplot() + 
      geom_polygon(data = data, aes(x = long, y = lat, group = group, 
                                    fill = usa.hi$Chance), color = "black", size = 0.15) + 

      scale_fill_distiller(palette = "RdBu",breaks = brks,
                           limits = c(min(usa.trumpshape$Chance), max(usa.trumpshape$Chance))) +
      theme_nothing(legend = FALSE) + labs(fill = "") +
    annotate("text", x = min(data$long)+6, y = min(data$lat) + 5, label = "HI")
    return(ggp)
  }
  

  p1<-p1(usa.trumpshape,brks.to.use,figure.title)
  p2<-p2(usa.al,brks.to.use)
  p3<-p3(usa.hi,brks.to.use)

  lay = rbind(c(1,1,1,1,1),
              c(1,1,1,1,1),
              c(1,1,1,1,1),
              c(1,1,1,1,1),
              c(2,3,NA,NA,NA),
              c(NA,3,NA,NA,NA))

  
  g = arrangeGrob(grobs = list(p1,p2,p3),layout_matrix = lay)
  ggsave("usa_state_selecion.jpg",g)
  hist(usa.dat$Chance, main = "Histogram of Clinton's Chances",xlab="Hillary Clinton Chances in percentage",ylab="Number of states")

  sum(usa.dat$Chance>40 & usa.dat$Chance<60)
  sum(usa.dat$Chance<40)
  sum(usa.dat$Chance>60)
  
  