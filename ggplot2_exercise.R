### Kyle Chung <alienatio@pixnet.net>
### Data visualization exercise



################################################################################
####                        set up working environment                      ####
################################################################################
setwd('C:/Dropbox/R/datavis')
library(ggplot2)   # package for fancy plotting
library(gridExtra) # multiplot on single page
# use ggsave() to save ggplot object; use arrangeGrob() for multiplot


################################################################################
####                        the bar graph                                   ####
################################################################################
str(ChickWeight)

ggplot(ChickWeight, aes(x=Diet,y=weight)) + 
    geom_bar(stat='identity') +
    xlab('This is my customized xlab!') + 
    ylab('And this is ylab!') +
    ggtitle('This is the MAIN') + 
    theme(axis.text.x=element_text(angle=90,vjust=.5)) # traonspose the x labels

# in aes(), x denotes the categorical variable in the given data.frame, 
# and y is the value to be plotted if stat='identity' is used
# also notice that y is implicitly group-summed by x
check1 <- aggregate(weight ~ Diet, data=ChickWeight, FUN=sum)
ggplot(check1, aes(x=Diet,y=weight)) + geom_bar(stat='identity')

# change colors
ggplot(ChickWeight, aes(x=Diet,y=weight)) + 
    geom_bar(stat='identity', fill='blue')
# and add outline of the bars
ggplot(ChickWeight, aes(x=Diet,y=weight)) + 
    geom_bar(stat='identity', fill='blue', color='red')
# what the hell? check the following graph, too
# y var is actually 'stacked up' instead of simply summed up
ggplot(check1, aes(x=Diet,y=weight)) + 
    geom_bar(stat='identity', fill='blue', color='red')

# x var can be factors or pure strings
check1$Diet.str <- as.character(check1$Diet)
ggplot(check1, aes(x=Diet.str,y=weight)) + geom_bar(stat='identity')

# if x is numeric (i.e., continuous var), possible null will be inserted
ggplot(ChickWeight, aes(x=Time,y=weight)) + geom_bar(stat='identity')
# to cure this (if preferred), use factor()
ggplot(ChickWeight, aes(x=factor(Time),y=weight)) + geom_bar(stat='identity')

# ignore y var to plot the x var by its counts
ggplot(ChickWeight, aes(x=Diet)) + geom_bar(stat='bin') # or simply geom_bar()
# check if these are the correct counts
check3 <- aggregate(weight ~ Diet, data=ChickWeight, FUN=length)
ggplot(check3, aes(x=Diet, y=weight)) + geom_bar(stat='identity')

# a second-level x var (the fill var): result on y seems strange, bug?
ggplot(ChickWeight, aes(x=factor(Time),fill=Diet,y=weight)) + 
    geom_bar(position='dodge', stat='identity')
# the correct result needs pre-summing
check2 <- aggregate(weight ~ Diet + Time, data=ChickWeight, FUN=sum)
ggplot(check2, aes(x=factor(Time),fill=Diet,y=weight)) + 
    geom_bar(position='dodge', stat='identity')
# or just default to stacked bar
ggplot(ChickWeight, aes(x=factor(Time),fill=Diet,y=weight)) + 
    geom_bar(stat='identity')
# reverse the legend in stacked bar
ggplot(ChickWeight, aes(x=factor(Time),fill=Diet,y=weight)) + 
    geom_bar(stat='identity') +
    guides(fill=guide_legend(reverse=TRUE))

# proportional stacked bar
# no automatic approach, calculate the percentage and plot it
check2p <- merge(check2, 
                aggregate(data=check2, weight ~ Time, FUN=sum), 
                by='Time')
check2p$pct <- check2p$weight.x / check2p$weight.y
ggplot(check2p, aes(x=factor(Time),fill=Diet,y=pct)) + 
    geom_bar(stat='identity')

# automatically generate interaction groups
check2pp <- subset(check2p, Time <= 4)
ggplot(check2pp, aes(x=interaction(factor(Time),Diet),y=pct)) + 
    geom_bar(stat='identity')

# choose brewer colors of the fill var
# see ?RColorBrewer::display.brewer.pal for more palette info
ggplot(ChickWeight, aes(x=factor(Time),fill=Diet,y=weight)) + 
    geom_bar(stat='identity') +
    scale_fill_brewer(palette='Pastel1')

# plot different bar with different colors
# notice that this is different from using a secondary group-by var
check2$cate <- ifelse(check2$Diet==1, 1, 0)
ggplot(check2, aes(x=Diet,fill=factor(cate),y=weight)) + 
    geom_bar(stat='identity')

# re-order x var by its height (value of y var)
ggplot(check2, aes(x=reorder(Diet,-weight),fill=factor(cate),y=weight)) + 
    geom_bar(stat='identity')

# plot positive and negative value with different directions
sleepid <- aggregate(data=sleep, extra ~ ID, FUN=sum)
sleepid$pos <- sleepid$extra >= 0 # need dummy var to indicate the sign
ggplot(sleepid, aes(x=ID,y=extra,fill=pos)) +
    geom_bar(stat='identity', position='identity') +
    guides(fill=FALSE) # remove the legend

# change bar width (maximum width = 1)
ggplot(sleepid, aes(x=ID,y=extra,fill=pos)) +
    geom_bar(stat='identity', position='identity', width=.5) 
# change bar width in dodge plot 
# (dodge width > normal width => separated grouped bar)
ggplot(check2, aes(x=factor(Time),fill=Diet,y=weight)) + 
    geom_bar(stat='identity', width=.5, position=position_dodge(.7))
# (dodge width < normal width => overlapped grouped bar)
ggplot(check2, aes(x=factor(Time),fill=Diet,y=weight)) + 
    geom_bar(stat='identity', width=1, position=position_dodge(.7))

# add text (label) to the bar
# positive vjust for label below the bar
ggplot(check1, aes(x=Diet,y=weight)) + 
    geom_bar(stat='identity') +
    geom_text(aes(label=weight), vjust=2, color='white')
# negative vjust for label above the bar
ggplot(check1, aes(x=Diet,y=weight)) + 
    geom_bar(stat='identity') +
    geom_text(aes(label=weight), vjust=-.5)
# label on stacked bar need additional column indicating the cummulative sum
check2 <- check2[with(check2,order(Time,weight)),]# sorting is necessary!
cums <- aggregate(data=check2, weight ~ Time, FUN=cumsum)
check2$cum <- as.vector(t(cums[,-1]))
ggplot(check2, aes(x=factor(Time),fill=Diet,y=weight)) + 
    geom_bar(stat='identity') +
    geom_text(aes(y=cum, label=weight), color='white', size=7) # size the text


################################################################################
####                        the line graph                                  ####
################################################################################
str(AirPassengers)

# ggplot can't deal with matrix/ts objects
air <- as.data.frame(t(
    matrix(AirPassengers, nrow=12, ncol=length(AirPassengers)/12)
    ))
colnames(air) <- paste('m', 1:12, sep='')
air$year <- seq(start(AirPassengers)[1], end(AirPassengers)[1], 1)

# default plot
ggplot(air, aes(x=year,y=m1)) + geom_line()
# add points
ggplot(air, aes(x=year,y=m1)) + geom_line() + geom_point()
# change points' size and shape
ggplot(air, aes(x=year,y=m1)) + geom_line() + geom_point(size=5)
ggplot(air, aes(x=year,y=m1)) + geom_line() + geom_point(size=5, shape=2)
# change linetype, thickness, and color
ggplot(air, aes(x=year,y=m1)) + 
    geom_line(linetype='dashed', size=1.5, color='blue')

# x var can be factors, but need additional parm group=1
# note that a continuous x var may be conceived (not displayed) on the x axis
# use factor(x) to force their presence
ggplot(air, aes(x=factor(year),y=m1,group=1)) + geom_line()

# use a log10 y axis
ggplot(air, aes(x=year,y=m1)) + geom_line() + geom_point() + scale_y_log10()
# set range of y axis
ggplot(air, aes(x=year,y=m1)) + geom_line() + ylim(0, max(air$m1))

# plot multiple lines
# unfortunately, there is no matplot() counterpart in ggplot2
# this means, data must be in long format with a group indicator
air_long <- cbind(stack(air[,1:12]), year=air$year)
# use color=
ggplot(air_long, aes(x=year,y=values,color=ind)) + geom_line()
# change the group var order in the legend (ordered factor)
air_long$ind <- factor(air_long$ind, levels=paste('m', 1:12, sep=''))
# use linetype=
ggplot(air_long, aes(x=year,y=values,linetype=ind)) + geom_line()
# use point shape (only up to 6 categories)
ggplot(air_long[1:(12*6),], aes(x=year,y=values,shape=ind)) + 
    geom_line() + geom_point(size=5)
# use fill= (colorized points) with shape=21 (vaccum circles)
ggplot(air_long[1:(12*6),], aes(x=year,y=values,fill=ind)) + 
    geom_line() + geom_point(size=5, shape=21)
# in case where x is factors: add group=x
ggplot(air_long, aes(x=factor(year),y=values,color=ind,group=ind)) + geom_line()

# line with shaded area
ggplot(air, aes(x=factor(year),y=m1,group=1)) + geom_area()
# change outline, color, alpha (transparency)
ggplot(air, aes(x=factor(year),y=m1,group=1)) + 
    geom_area(color='red', fill='darkblue', alpha=.2)
# top outline only
ggplot(air, aes(x=factor(year),y=m1,group=1)) + 
    geom_area(fill='darkblue', alpha=.2) + geom_line(color='red')

# stacked area plot
ggplot(air_long, aes(x=year,y=values,fill=ind)) + geom_area()
ggplot(air_long, aes(x=year,y=values,color=ind)) + geom_area()
# reverse the order of legend (as in the stacked bar graph)
ggplot(air_long, aes(x=year,y=values,fill=ind)) + geom_area() + 
    guides(fill=guide_legend(reverse=TRUE))

# proportional stacked area plot (as in the stacked bar graph)
air_longp <- merge(air_long,
                   aggregate(data=air_long, values ~ year, FUN=sum),
                   by='year')
air_longp$pct <- air_longp$values.x / air_longp$values.y
# without sorting on group var (horrible!)
ggplot(air_longp, aes(x=year,y=pct,fill=ind)) + geom_area() + 
    guides(fill=guide_legend(reverse=TRUE))
# with sorting on group var
air_longp <- air_longp[order(air_longp$ind),]
ggplot(air_longp, aes(x=year,y=pct,fill=ind)) + geom_area() + 
    guides(fill=guide_legend(reverse=TRUE))


################################################################################
####                        the scatter plot                                ####
################################################################################
str(cars)

ggplot(cars, aes(x=speed,y=dist)) + geom_point()

# change point size (default to 2) or shape (1-25, default to 16)
ggplot(cars, aes(x=speed,y=dist)) + geom_point(size=5) 
ggplot(cars, aes(x=speed,y=dist)) + geom_point(shape=2)  # hollow triangle
ggplot(cars, aes(x=speed,y=dist)) + geom_point(shape=4)  # x-mark
ggplot(cars, aes(x=speed,y=dist)) + geom_point(shape=19) # better solid circle
ggplot(cars, aes(x=speed,y=dist)) + geom_point(shape=21) # hollow circle
# and more...
ggplot(cars, aes(x=speed,y=dist)) + geom_point(shape='#', size=5)
ggplot(cars, aes(x=speed,y=dist)) + geom_point(shape='|', size=5)
ggplot(cars, aes(x=speed,y=dist)) + geom_point(shape='%', size=5)
ggplot(cars, aes(x=speed,y=dist)) + geom_point(shape='+', size=5)
# indeed, some other string literals also work...
ggplot(cars, aes(x=speed,y=dist)) + geom_point(shape='0', size=5)
ggplot(cars, aes(x=speed,y=dist)) + geom_point(shape='1', size=5)
ggplot(cars, aes(x=speed,y=dist)) + geom_point(shape='a', size=5)

# set transparency
ggplot(cars, aes(x=speed,y=dist)) + geom_point(alpha=.2, size=5)

# all possible shapes! (here you know how to hide the axis label)
spoint <- data.frame(x=1,y=1)
symbol_points <- list()
for (i in 1:25) {
    symbol_points[[i]] <- ggplot(spoint, aes(x=x,y=y)) + 
        geom_point(shape=i, size=5) +
        ggtitle(sprintf('shape=%s',i)) + 
        theme(axis.text.x=element_blank(), axis.text.y=element_blank()) +
        xlab('') + ylab('')
}
symbols <- do.call(arrangeGrob,symbol_points)
ggsave(symbols, filename='all_point_symbols.png')

# group points by color or/and shape: need grouping var in the data.frame
cars$grp <- cars$dist >= 50
ggplot(cars, aes(x=speed,y=dist,color=grp)) + geom_point(size=5) 
ggplot(cars, aes(x=speed,y=dist,shape=grp)) + geom_point(size=5) 
ggplot(cars, aes(x=speed,y=dist,shape=grp, color=grp)) + geom_point(size=5)
# set up shapes manually
ggplot(cars, aes(x=speed,y=dist,shape=grp)) + geom_point(size=5) +
    scale_shape_manual(values=c(4,21))

# use continous var as the group var (the third dim for scatter plot)
str(women)
women$bmi <- women$weight*0.45359 / (women$height*2.54/100)^2
ggplot(women, aes(x=weight,y=height,color=bmi)) + geom_point(size=5)
ggplot(women, aes(x=weight,y=height,size=bmi)) + geom_point(shape=19)

# large scale scatter plot
pseudo <- data.frame(x=rnorm(50000), y=runif(50000))
ggplot(pseudo, aes(x=x,y=y)) + geom_point() # unable to see the density!
# bin points plot: deafult to 30*30 grids, each with its own density 
ggplot(pseudo, aes(x=x,y=y)) + stat_bin2d()
# use 50*50 grids with modified colorization
ggplot(pseudo, aes(x=x,y=y)) + stat_bin2d(bins=70) +
    scale_fill_gradient(low='grey',high='black')
# hex point
ggplot(pseudo, aes(x=x,y=y)) + stat_binhex(bins=50) +
    scale_fill_gradient(low='grey',high='red')

# plot regression line
head(faithful)
p <- ggplot(faithful, aes(x=eruptions,y=waiting)) + geom_point()
p + stat_smooth(method=lm, level=.9)  # set 90% CI (default is 95%)
p + stat_smooth(method=lm, se=FALSE)  # remove CI
# change line type (linetype can be string or integer)
p + stat_smooth(method=lm, se=FALSE, color='red', size=1, linetype='dotted')

# locally weighted polynomial fit (the default)
p + stat_smooth()

# logit fit
cars$grp <- as.numeric(cars$grp)
ggplot(cars, aes(x=speed,y=grp)) + 
    geom_point() +
    stat_smooth(method=glm, family=binomial)

# ballon plot
cars$rvalue <- sample(1:1000, size=nrow(cars), replace=TRUE)
ggplot(cars, aes(x=speed,y=dist,size=rvalue)) + 
    geom_point(shape=21) +
    scale_size_area(max_size=10)







