### Kyle Chung <alienatio@pixnet.net>
### Dynamic graphing implemented by d3 (data-driven document)


################################################################################
####                        set up working environment                      ####
################################################################################
setwd('./svg')
library(ggplot2)
library(gridSVG)
library(XML)
library(rjson)

# create grid-based plot
check1 <- aggregate(weight ~ Diet, data=ChickWeight, FUN=sum)
gg <- ggplot(check1, aes(x=Diet,y=weight)) + geom_bar(stat='identity')
str(gg)
gg

# list grid component of the current scene
ggsvg <- grid.export("bargraph.svg", xmldecl=NULL, addClasses=TRUE)
str(ggsvg, max.level=1)
cat(saveXML(ggsvg$svg))

cat(
    '<script> data=',
    toJSON(
        apply(gg$data, 1, function(x) list(x))
        ),
    '</script>'
    )


gg$data
