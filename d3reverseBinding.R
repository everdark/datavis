### Kyle Chung <alienatio@pixnet.net>
### Dynamic graphing implemented by D3: a reverse data binding approach


################################################################################
####                        setup working environment                       ####
################################################################################
library(ggplot2)
library(gridSVG)



################################################################################
####                        scatter plot                                    ####
################################################################################
## create sample data
case <- iris[,3:5]
colnames(case) = gsub('\\.', '', colnames(case)) # '.' cause reference problem in JS
case$Name <- paste('N', round(runif(nrow(case)),3)*1000, sep='')
head(case)


## base scatter plot
plot(case[,1:2], col=c(1:3)[case$Species], pch=19)
legend('bottomright', levels(case$Species), col=c(1:3), pch=19)


## ggplot2 scatter plot
AES <- aes(x=PetalLength, y=PetalWidth, group=Species, color=Species)
gg <- ggplot(case, AES) + 
    geom_point(size=3) + 
    theme(legend.position='top')
str(gg)


## convert ggplot to SVG format (the resulting html contains svg element only)
ggsvg <- grid.export("./svg_demo/ggplot_scatter.html", xmldecl=NULL, addClasses=TRUE)
file.show('./svg_demo/ggplot_scatter.html')


## hack into .svg
# read back svg inline
raw_svgcode <- readLines('./svg_demo/ggplot_scatter.html', warn=FALSE)
# add d3.js library path
d3js_library_url <- 'https://dl.dropboxusercontent.com/u/210177/d3/d3.v3.min.js'
modified_svgcode <- c(paste('<script src="', d3js_library_url, '"></script>', sep=''),
                      raw_svgcode)
# convert data to json (in object-of-objecs format)
head(gg$data)
tojson <- apply(gg$data, 1, function(x) list(x))
tfile <- file()
cat(
    '<script> data=',
    toJSON(lapply(tojson, function(x) unlist(x))),
    '</script>'
    ,
    file=tfile
    )
# # array-of-array-of-single-object format
# cat(
#     '<script> data=',
#     toJSON(
#         apply(gg$data, 1, function(x) list(x))
#     ),
#     '</script>'
#     )
importData <- readLines(tfile, warn=FALSE)
close(tfile)
modified_svgcode <- c(modified_svgcode, importData)
# bind data
tfile <- file()
cat(
    '
    <script>
    scatterpoints = d3.select(".points")
                      .selectAll("use")
                      .data(data);
    </script>
    ',
    file=tfile
    )
bindData <- readLines(tfile, warn=FALSE)
close(tfile)
modified_svgcode <- c(modified_svgcode, bindData)
# add simple tooltip (use browser default title facility)
tfile <- file()
cat(
    '
    <script>
    d3.selectAll("use")
      .append("title")
      .text(function(d) {return d.Name;});
    </script>
    ',
    file=tfile
)
addTooltip <- readLines(tfile, warn=FALSE)
close(tfile)
modified_svgcode <- c(modified_svgcode, addTooltip)
# add hyperlink event
tfile <- file()
cat(
    '
    <script>
    d3.selectAll("use")
      .on("click", function(d) {
        var url = "http://google.com/search?q=";
        url += d.Name;
        window.location.href = url;}
        );
    </script>
    ',
    file=tfile
)
addHref <- readLines(tfile, warn=FALSE)
close(tfile)
modified_svgcode <- c(modified_svgcode, addHref)

writeLines(modified_svgcode, './svg_demo/ggplot_scatter_modified.html')
file.show('./svg_demo/ggplot_scatter_modified.html')


