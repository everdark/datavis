### Kyle Chung <alienatio@pixnet.net>
### Dynamic (interactive) graphing



################################################################################
####                        setup working environment                       ####
################################################################################
setwd('./svg')
library(gridSVG)
library(ggplot2) # notice that ggplot2 is grid-based

## official demo code from: https://www.stat.auckland.ac.nz/~paul/gridSVG/
# create sample grid plot
topvp <- viewport(y=1, just="top", name="topvp", height=unit(1, "lines"))
botvp <- viewport(y=0, just="bottom", name="botvp", height=unit(1, "npc") -
                      unit(1, "lines"))
grid.rect(gp=gpar(fill="grey"), vp=topvp, name="toprect")
grid.rect(vp=botvp, name="botrect")

# convert plot as SVG format
gridToSVG('gridscene.svg', xmldecl=NULL)

# do simple animation
widthValues <- unit(c(1, 1), c("npc", "in"))
grid.animate("toprect", width=widthValues, duration=3)
grid.animate("botrect", width=widthValues, duration=3)
gridToSVG("gridanim.svg", xmldecl=NULL)

# add hyperlink text
grid.text("take me there", vp=topvp, name="hypertext")
grid.hyperlink("hypertext", "http://www.r-project.org")
gridToSVG("gridhyper.svg", xmldecl=NULL)

# add mouse click event: pop-up dialog
grid.garnish("botrect", onmousedown="alert('ouch!')", "pointer-events"="all")
gridToSVG("gridmouse.svg", xmldecl=NULL)

# add interaction by JavaScript
grid.garnish("toprect", onmousedown="allblack()", "pointer-events"="all")
grid.script("
allblack = function() {
    rect = document.getElementById('toprect.1');
    rect.setAttribute('style', 'fill:black');
}
            ")
gridToSVG("gridscript.svg", xmldecl=NULL)


################################################################################
####                        toolkit for name display                        ####
################################################################################
## official demo code from: https://www.stat.auckland.ac.nz/~paul/gridSVG/
grobs <- grid.ls()
names <- grobs$name[grobs$type == "grobListing"]
for (i in unique(names)) {
    grid.garnish(i,
                 onmouseover=paste("showTooltip(evt, '", i, "')"),
                 onmouseout="hideTooltip()")
}
grid.script(filename="tooltip.js")
gridToSVG("qplotbrowser.svg", xmldecl=NULL)


################################################################################
####                        dynamic ggplot                                  ####
################################################################################
# create grid-based plot
dev.off()
check1 <- aggregate(weight ~ Diet, data=ChickWeight, FUN=sum)
gg <- ggplot(check1, aes(x=Diet,y=weight)) + geom_bar(stat='identity')
gg

# list grid component of the current scene
grid.ls(fullNames=TRUE)

# try add hyperlink for the bar grobs (on g element)
grid.hyperlink("geom_rect.rect.2", "http://www.r-project.org")
gridToSVG("bargraph.svg", xmldecl=NULL)

# try add hyperlink for specific bar grob (on rect element)
gg
grid.hyperlink("geom_rect.rect.133", c("http://www.r-project.org"), group=FALSE)
gridToSVG("bargraph.svg", xmldecl=NULL)
# only the first bar get hyperlink, notice that the href is NOT recycled







