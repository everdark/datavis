### Kyle Chung <alienatio@pixnet.net>
### Data visualization exercise


library(ggplot2)
data()
str(mtcars)
plot(mtcars$mpg, mtcars$wt)
qplot(mpg, wt, data=mtcars)