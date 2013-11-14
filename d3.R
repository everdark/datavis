### Kyle Chung <alienatio@pixnet.net>
### Dynamic graphing implemented by d3 (data-driven document)


################################################################################
####                        setup working environment                       ####
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

gg$data
cat(
    '<script> data=',
    toJSON(
        apply(gg$data, 1, function(x) list(x))
        ),
    '</script>'
    )




## example from http://timelyportfolio.github.io/gridSVG_intro/
set.seed(955)
# Make some noisily increasing data
dat <- data.frame(cond = rep(c("A", "B"), each=10),
                  xvar = 1:20 + rnorm(20,sd=3),
                  yvar = 1:20 + rnorm(20,sd=3))
# cond         xvar         yvar
#    A -4.252354091  3.473157275
#    A  1.702317971  0.005939612
#   ... 
#    B 17.793359218 19.718587761
#    B 19.319909163 19.647899863
g4 <- ggplot(dat, aes(x=xvar, y=yvar)) +
    geom_smooth() +  #we'll see why order is important
    geom_point(shape=19, aes(color = cond), size=5) 
g4
g4.svg <- grid.export("plot1.svg", xmldecl=NULL, addClasses=TRUE)
#print our newly exported SVG inline
cat(saveXML(g4.svg$svg))

# add the following script blocks to a html holding the svg
cat(
    '<script src="https://dl.dropboxusercontent.com/u/210177/d3/d3.v3.min.js"></script>'
)
cat(
    '<script> ourdata=',
    rjson::toJSON(apply(g4$data,MARGIN=1,FUN=function(x)return(list(x)))),
    '</script>'
)
cat(
    '<script> dataToBind = ',
    'd3.entries(ourdata.map(function(d,i) {return d[0]}))',
    '</script>'
)
cat(
    '<script>\n',
    'scatterPoints = d3.select(".points").selectAll("use");\n',
    'scatterPoints.data(dataToBind)',
    '</script>\n'
)
cat('<script>\n',
    'scatterPoints  
    .on("mouseover", function(d) {      
      //Create the tooltip label
      var tooltip = d3.select(this.parentNode).append("g");
      tooltip
        .attr("id","tooltip")
        .attr("transform","translate("+(d3.select(this).attr("x")+10)+","+d3.select(this).attr("y")+")")
        .append("rect")
          .attr("stroke","white")
          .attr("stroke-opacity",.5)
          .attr("fill","white")
          .attr("fill-opacity",.5)
          .attr("height",30)
          .attr("width",50)
          .attr("rx",5)
          .attr("x",2)
          .attr("y",5);
      tooltip.append("text")
        .attr("transform","scale(1,-1)")
        .attr("x",5)
        .attr("y",-22)
        .attr("text-anchor","start")
        .attr("stroke","gray")
        .attr("fill","gray")
        .attr("fill-opacity",1)
        .attr("opacity",1)
        .text("x:" + Math.round(d.value.xvar*100)/100);
      tooltip.append("text")
        .attr("transform","scale(1,-1)")
        .attr("x",5)
        .attr("y",-10)
        .attr("text-anchor","start")
        .attr("stroke","gray")
        .attr("fill","gray")      
        .attr("fill-opacity",1)
        .attr("opacity",1)
        .text("y:" + Math.round(d.value.yvar*100)/100);
    })              
    .on("mouseout", function(d) {       
        d3.select("#tooltip").remove();  
    });',
    '</script>'
)



