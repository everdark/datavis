### Kyle Chung <alienatio@pixnet.net>
### Data visualization exercise



## set up working environment
setwd('C:/Dropbox/R/datavis')
library(ggplot2)


## bar graph
str(ChickWeight)
ggplot(ChickWeight, aes(x=Diet,y=weight)) + 
    geom_bar(stat='identity') +
    xlab('This is my customized xlab!') + 
    ylab('And this is ylab!')

# in aes(), x denotes the categorical variable in the given data.frame, 
# and y is the value to be plotted if stat='identity' is used
# also notice that y is implicitly group-summed by x:
check1 <- aggregate(weight ~ Diet, data=ChickWeight, FUN=sum)
ggplot(check1, aes(x=Diet,y=weight)) + geom_bar(stat='identity')

# change colors:
ggplot(ChickWeight, aes(x=Diet,y=weight)) + 
    geom_bar(stat='identity', fill='blue')
# and add outline of the bars
ggplot(ChickWeight, aes(x=Diet,y=weight)) + 
    geom_bar(stat='identity', fill='blue', color='red')
# what the hell? check the following graph, too
# y var is actually 'stacked up' instead of simply summed up:
ggplot(check1, aes(x=Diet,y=weight)) + 
    geom_bar(stat='identity', fill='blue', color='red')

# x var can be factors or pure strings:
check1$Diet.str <- as.character(check1$Diet)
ggplot(check1, aes(x=Diet.str,y=weight)) + geom_bar(stat='identity')

# if x is numeric (i.e., continuous var), possible null will be inserted:
ggplot(ChickWeight, aes(x=Time,y=weight)) + geom_bar(stat='identity')
# to cure this (if preferred), use factor():
ggplot(ChickWeight, aes(x=factor(Time),y=weight)) + geom_bar(stat='identity')

# ignore y var to plot the x var by its counts:
ggplot(ChickWeight, aes(x=Diet)) + geom_bar(stat='bin') # or simply geom_bar()
# check if these are the correct counts:
check3 <- aggregate(weight ~ Diet, data=ChickWeight, FUN=length)
ggplot(check3, aes(x=Diet, y=weight)) + geom_bar(stat='identity')

# a second-level x var (the fill var): result on y seems strange, bug?
ggplot(ChickWeight, aes(x=factor(Time),fill=Diet,y=weight)) + 
    geom_bar(position='dodge', stat='identity')
# the correct result needs pre-summing:
check2 <- aggregate(weight ~ Diet + Time, data=ChickWeight, FUN=sum)
ggplot(check2, aes(x=factor(Time),fill=Diet,y=weight)) + 
    geom_bar(position='dodge', stat='identity')
# or just default to stacked bar:
ggplot(ChickWeight, aes(x=factor(Time),fill=Diet,y=weight)) + 
    geom_bar(stat='identity')

# choose brewer colors of the fill var:
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




## scatter plot
str(mtcars)
plot(mtcars$mpg, mtcars$wt) # the base R approach
qplot(mpg, wt, data=mtcars) # teh ggplot approach

## line graph
plot(mtcars$mpg, mtcars$wt, type='l')    # failed due to unsorted
mpg_wt <- as.data.frame(cbind(mpg=mtcars$mpg, wt=mtcars$wt))
mpg_wt <- mpg_wt[order(mpg_wt$mpg),]
plot(mpg_wt[,1],mpg_wt[,2], type='l')    # success after sorting
plot(mpg_wt, type='l')                   # the same
qplot(mpg, wt, data=mtcars, geom='line') # auto pre-sorting

