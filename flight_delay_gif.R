library(ggplot2)
library(readr)
library(maps)
library(ggmap)
library(grid)
library(animation)
library(dplyr)

# Normal days
data_2006 =read_csv("~/Dropbox/Spring 2019/DSO 545/shiny_app/2006_data.csv")
jan13 = data_2006 %>% filter(Month == 1 & DayofMonth== 13)
jan19 = data_2006 %>% filter(Month == 1 & DayofMonth== 19)

#March 7 2008 North American Blizzard of 2008. Tornadoes and blizzards
data_2008 = read_csv("~/Dropbox/Spring 2019/DSO 545/shiny_app/2008_data.csv")
march7 = data_2008 %>% filter(Month == 3 & DayofMonth== 7)

# Sept 11 2001
data_2001 = read_csv("~/Dropbox/Spring 2019/DSO 545/shiny_app/2001_data.csv")
sept11 = data_2001 %>% filter(Month == 9 & DayofMonth== 11)

dec24 = readRDS("~/Downloads/Dec24_2018.rds")
# dec24 = dec24 %>% filter(Dest != "ECP" & Origin != "ECP")
dec24 = dec24 %>% filter(Origin %in%unique(airports$iata), Dest %in%unique(airports$iata))


airports <- read.csv("http://www.public.iastate.edu/~hofmann/looking-at-data/data/airports.csv")
airports = airports %>% distinct(iata,.keep_all = T)



states <- read.csv("http://www.public.iastate.edu/~hofmann/looking-at-data/data/states.csv")

# a set of personal choices for the map display
slider <- data.frame(x=c(-130,-130, -60, -60, -130, -60), y=1+c(20.5,21.5,20.5,21.5,21,21), id=c(1,1, 2,2, 3,3))

ticks <- data.frame(x=rep(seq(-130, -60, length=26), each=2),
                    y=rep(22+c(-0.25,0.25), 26),
                    id = 3+rep(1:26, each=2))
idx <- 1+ 6*0:8

getTSliderX <- function(time) {
    minT <- 3
    maxT <- 28
    time <- time %/% 100 + (time %% 100)/60
    x <- min(ticks$x)+1.0*(time - minT)/(maxT - minT) * diff(range(ticks$x))
    return(x)
}


map.opts <- theme(panel.grid.minor=element_blank(), 
                  panel.grid.major=element_blank(),
                  panel.background=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.line=element_blank(),
                  axis.ticks=element_blank(),
                  axis.text.y  = element_text(colour="#FFFFFF"),
                  axis.text.x = element_text(colour = "#FFFFFF"))






# get all flights in the air and all flights that have been cancelled

inFlight <- function(df, tp, interval) {
    # df is the data set to subset
    # tp is the time point
    
    startHour <- tp
    endHour <- tp + interval
    if (endHour %% 100 >= 60) {
        endHour <- (endHour - 60) + 100
    }		
    
    tm <- subset(df, ((DepTime < endHour) & (ArrTime > startHour)) | (((CRSDepTime %in% (startHour:endHour))) & (Cancelled == 1))) 
return(tm)
}

flightTrack <- function(fromXY, toXY, ratio, seed) {
    # from XY and toXY are GPS coordinates of origin and destination airports
    # ratio is a number between 0 and 1, indicating how much of the distance 
    #	the plane has travelled, with 0 indicating the origin and 1 indicating 
    #	the destination
    # seed is the seed used in the random number generator - here we use 
    #	ArrTime*DepTime to uniquely identify each flight
    
    rand <- sapply(seed, function(x) {
        set.seed(x)
        return(runif(1,-.5,.5))
    })
    
    dir <- data.frame(lon=toXY$dest_lon-fromXY$origin_lon,lat=toXY$dest_lat-fromXY$origin_lat)
    orth <- rev(dir)
    orth[,1] <- orth[,1]*(-1)
    
    location <- fromXY+ratio*dir+(1-ratio)*ratio*orth*rand
    return(location)	
}


airport.location.mod <- function(iata) {
    iata = iata %>% select(Origin,Dest)
    results = lapply(1:nrow(iata), function(i){
        print(i)
        origin_lat_lon = data.frame(airports[which(airports$iata == iata[i,]$Origin),7:6])
        colnames(origin_lat_lon) = c("origin_lon", "origin_lat")
        des_lat_lon = data.frame(airports[which(airports$iata == iata[i,]$Dest),7:6])
        colnames(des_lat_lon) = c("dest_lon", "dest_lat")
        bind_cols(iata[i,],origin_lat_lon,des_lat_lon)
    })
    results  = bind_rows(results)
    return (results)
}

airport.location <- function(iata) {
    idx <- unlist(sapply(iata, function(x) return(which(airports$iata %in% x))))
    x <- airports[idx,7:6]
    return (x)
}

getAircraftLocation <- function(df) { # helper function: get coordinates for airport 
    
    # get GPS coordinates of airports 
    XY <- airport.location.mod(df)
    origXY <- XY %>% select(origin_lon,origin_lat)
    destXY <- XY %>% select(dest_lon,dest_lat)
    # destXY <- airport.location(df$Dest)
    # compute air time based on departure and arrival times 
    airtime <- with(df, (ArrTime %% 100 - DepTime %% 100) + (ArrTime%/%100 - DepTime%/%100)*60) 
    # compute the ratio flown, adjust for possible data errors 
    flown <- with(df, (time %% 100 - DepTime %% 100) + (time%/%100 - DepTime%/%100)*60) 
    flown[flown < 0] <- 0 
    ratio <- flown/airtime 
    ratio[is.na(ratio)] <- 0 
    ratio[ratio > 1] <- 1 
    # render flights on straight line 
    # return(origXY+ratio*(destXY-origXY)) 
    # render flights on arcs with random curvature 
    return(flightTrack(origXY, destXY, ratio, ifelse(!is.na(df$DepTime*df$ArrTime),df$DepTime*df$ArrTime,as.integer(rnorm(1,mean = 20,5))))) 
}

# plotFace <- function(time) {
#     # time is given in the format HHMM
#     ho <- time %/% 100
#     ho <- ho %% 12      # ensure hour between 0 and 12
#     
#     mi <- time %% 100
#     
#     # minute and hour hands	
#     face <- data.frame(cbind(x = c(rep((ho+mi/60)/12,2), rep(mi/60, 2)), y = c(0,0.5, 0,0.9)))
#     face$id <- c("hour", "hour", "min", "min")
#     
#     # clock frame around
#     clock <- data.frame(cbind(x = seq(0,1,length = 100), y = rep(1,100)))
#     clock$id <- "clock"
#     face <- rbind(face,clock)
#     
#     q <- 
#         qplot(x,y, geom = "line", colour = I("grey65"), group = id, data = face, xlim = c(0,1)) + 
#         coord_polar() + 
#         scale_x_continuous(breaks = seq(0,1, length = 5), labels = "") +
#         labs(x = NULL, y = NULL) +
#         opts(plot.margin = unit(rep(0,4), "lines"),
#              panel.background = theme_blank(),
#              panel.grid.minor = theme_blank(),
#              axis.title.x = theme_blank(),
#              axis.title.y = theme_blank(),
#              axis.line = theme_blank(),
#              axis.ticks = theme_blank(),
#              axis.text.y   =  theme_text(colour = "#FFFFFF", lineheight = -10, size = 0),
#              axis.text.x   =  theme_text(colour = "#FFFFFF", lineheight = -10, size = 0))
#     return(q)
# }
plotFace <- function(time) {
    ho <- time %/% 100
    ho <- ho %% 12
    mi <- time %% 100
    
    face <- data.frame(cbind(x=c(rep((ho+mi/60)/12,2), rep(mi/60, 2)), y = c(0,0.5, 0,0.9)))
    face$id <- c("hour", "hour", "min", "min")
    
    clock <- data.frame(cbind(x=seq(0,1,length=100), y=rep(1,100)))
    clock$id <- "clock"
    face <- rbind(face,clock)
    
    
    q <- 
        qplot(x,y, geom="line", colour=I("grey65"), group=id, data=face, xlim=c(0,1)) + scale_x_continuous(breaks=seq(0,1, length=5), labels=rep("",5))+ coord_polar() +
        labs(x=NULL, y=NULL) +
        theme(plot.margin=unit(rep(0,4), "lines"),
              panel.background=element_blank(),
              panel.grid.minor= element_blank(),
              axis.title.x= element_blank(),
              axis.title.y= element_blank(),
              axis.line= element_blank(),
              axis.ticks= element_blank(),
              axis.text.y  = element_text(colour="#FFFFFF", lineheight=-10, size=0),
              axis.text.x  = element_text(colour="#FFFFFF", lineheight=-10, size=0)	  )
    return(q)
}



plotMap <- function(res, time) {
    
    
    cancel <- subset(res, Cancelled == 1)
    res <- subset(res, Cancelled == 0)
    
    
    q <- usamap + geom_point(aes(x = x, y = y), data = data.frame(cbind(x = getTSliderX(time), y = 22)), color = "grey30")
    
    if (nrow(cancel) > 0) {
        cancelxy <- airport.location(cancel$Origin)
        q <- q + geom_point(aes(x = longitude, y = latitude), size = 2, colour  =  I(alpha("red",5/10)), data = cancelxy)
    }
    
    if(nrow(res) > 0) { 
        res$time <- time
        res = res %>% select(Origin, Dest, DepTime, ArrTime, time,ArrDelay)
        loc <- getAircraftLocation(res)
        loc$delay <- with(res, pmax(ArrDelay,0))
        loc$delay <- with(res, pmin(ArrDelay,300))
        loc$longitude = loc$origin_lon
        loc$latitude = loc$origin_lat
        
        q <- q +
            geom_point(aes(x = origin_lon, y = origin_lat, size = delay), 
                       colour  =  I(alpha("black", 7/10)), data = loc) + 
            scale_size(name = "Arrival\nDelays", breaks = c(15, 60, 120, 240), 
                       labels = c("15 min",  "1h", "2h", "4h"), limits = c(0,300))
    } 
    
    print(q)
    
    vp1 <- viewport(x = 0.05, y = 0.25, height = unit(2.5, "cm"), width =  unit(2.5, "cm"), just = c("left","bottom"))
    vp2 <- viewport(x = 0.8, y = 0.25, height = unit(2.5, "cm"), width =  unit(2.5, "cm"), just = c("right","bottom"))
    
    # show Pacific time
    print(plotFace(time-300), vp = vp1)
    # show Eastern time
    print(plotFace(time), vp = vp2)
}

# plotMap(inFlight(jan13, 500, 2), 500) # map for 6:30 pm on January 19


mins <- seq(0,58, by=20)
hour <- seq(700, 1700, by=100)
seqs <- rep(hour, each=length(mins))
seqs <- seqs + mins




# Set ani.option
ani.options(interval = 0.5,ani.width = 960,ani.height=960)

data_list = list(jan19,march7,sept11,dec24)
file_name = c("jan19", "march7","sept11","dec24")
dates = list(c("Jan 19 2006","Jan 20 2006"), c("March 7 2008","March 8 2008"),c("September 11, 2001", "September 12, 2001"),c("December 24, 2018", "December 25, 2018"))

for(j in 1:length(data_list)){
    print(file_name[j])
    start_date = dates[[j]][1]
    end_date = dates[[j]][2]
    # Redraw the us map each time and update the labels
    usamap <- ggplot2::qplot(x, y, data = states, geom = "polygon", fill = I("grey85"), colour = I("white"), xlim = c(-130,-60), ylim = c(20,50)) +
            map.opts + 
            geom_point(aes(x = longitude, y = latitude), size = 0.7, colour = "grey65", data = subset(airports, Volume > 100)) +
        geom_line(aes(x = x, y = y, group = id), data = slider, colour = "grey55", size = 0.25) + 
        geom_line(aes(x = x, y = y, group = id), data = ticks, colour = "grey55", size = 0.25) + 
        annotate("text", x = ticks$x[1], y = 22.8, label = c(start_date), colour = "grey40", size = 3, hjust = 0.25, vjust = 0) + 
        annotate("text", x = ticks$x[nrow(ticks)-2], y = 22.8, label = c(end_date), colour = "grey40", size = 3, hjust = 0.5, vjust = 0) + 
        annotate("text", x = ticks$x[idx], y = ticks$y[idx]-1, label = c("3am EST", "6am", "9am", "12pm", "3pm", "6pm", "9pm", "12am EST", "3am"), colour = "grey40", size = 3, hjust = 0.5, vjust = 0) 
    
    
    saveGIF({for (i in head(seqs, 180)) {
        print(i)
        
        suppressWarnings(plotMap(inFlight(data_list[[j]], i, 5),i))

    }},movie.name = paste0("~/Dropbox/Spring 2019/DSO 545/shiny_app/",file_name[j],".gif"))
}




