### Kyle Chung <alienatio@pixnet.net>
### Explore package rCharts for dynamic graphing


################################################################################
####                        setup working environment                       ####
################################################################################

## check required packages
if ( !'rCharts' %in% dir(.libPaths()) ) {
    if ( !'devtools' %in% dir(.libPaths()) ) install.packages('devtools')
    devtools::install_github('rCharts', 'ramnathv')
}

library(rCharts)



################################################################################
####                        scatter plot                                    ####
################################################################################
## generate NVD3 scatterChart template
case <- iris[,3:5]
colnames(case) = gsub('\\.', '', colnames(case)) # '.' cause reference problem in js
case$Name <- paste('N', round(runif(nrow(case)),3)*1000, sep='')
str(case)
nvd3 <- nvd3Plot(
    PetalLength ~ PetalWidth, data=case, type='scatterChart', 
    group='Species', 
    xAxis=list(axisLabel='PetalWidth'), 
    yAxis=list(axisLabel='PetalLength'),
    # available parameters for chart.xAxis/chart.yAxis:
    #   axisLabel   char
    #   orient      char: 'top', 'bottom', ...
    #   ticks       int (# of ticks, merely suggestion and d3 wont listen to you indeed)
    #   tickFormat  d3.format(".1%") (not able to parse in R, must manually add to js)
    chart=list(showDistX=TRUE, showDistY=TRUE)
    # available parameters for chart:
    #   showDistX       logic
    #   showDistX       logic
    #   showLegend      logic
    #   showControls    logic (fisheye effect)
)
nvd3             # .html saved to tempdir with random-string file name
nvd3$print()
cat(nvd3$html()) # cat the <script> only
nvd3$save(destfile='scatterplot.html')


dir(tempdir())[grep('\\.html$', dir(tempdir()))]

# add graph title
d3.select("#" + opts.id)
.append("text")
.attr("x", 200)             
.attr("y", 100)
.attr("text-anchor", "middle")  
.text("This is TITLE");


## interactivity ( must be manually edit in the resulted js)
# add this to examine data binding in the browser console
# on mouseover event will return something like: 
# Object {PetalLength: 1.9, PetalWidth: 0.4, Species: "setosa", series: 0}
d3.selectAll("circle")
  .on("mouseover", function(d) {
      console.log(d);}
      );

# create pop-up window for point info on click event
d3.selectAll("circle")
  .on("click", function(d) {
      alert(d.Species);}
      );

# create hover effect (without resort to CSS)
d3.selectAll("circle")
.on("mouseover", function() {
    d3.select(this)
      .attr("fill", "red");}
)
.on("mouseout", function() {
    d3.select(this)
      .attr("fill", function(d) {return d.color;});}
);

# tooltip: use browser default tooltip
d3.selectAll("circle")
  .append('title')
  .text(function(d) {return d.Name;});

# tooltip: use HTML div
# add the following CSS style for the tooltip under <style> within <head>
cat('
#tooltip {
    position: absolute;
    width: 100px;
    height: auto;
    padding: 10px;
    background-color: white;
    -webkit-border-radius: 10px;
    -moz-border-radius: 10px;
    border-radius: 10px;
    -webkit-box-shadow: 4px 4px 10px rgba(0, 0, 0, 0.4);
    -moz-box-shadow: 4px 4px 10px rgba(0, 0, 0, 0.4);
    box-shadow: 4px 4px 10px rgba(0, 0, 0, 0.4);
    pointer-events: none;
}
#tooltip.hidden {
    display: none;
}
#tooltip p {
    margin: 0;
    font-family: sans-serif;
    font-size: 16px;
    line-height: 20px;
}
')
# hardcode the folliwng div tag within <body>
cat('
<div id="tooltip" class="hidden">
    <p><strong>Name Info</strong></p>
    <p><span id="value">0</span></p>
</div>
')
# and add the following js into the main function as usual
d3.selectAll("circle")
.on("mouseover", function(d) {
    d3.select("#tooltip")
    .style("left", d3.event.pageX + "px")
    .style("top", d3.event.pageY + "px")
    .select("#value")
    .text(d.Name);
    d3.select("#tooltip").classed("hidden", false);})
.on("mouseout", function() {
    d3.select("#tooltip").classed("hidden", true);});
# alternatively, use circle position rather than mouse position 
# xPosition not stable over zooming? 
# => it may only work if the <body> and <svg> elements have the same positioning
d3.selectAll("circle")
  .on("mouseover", function(d) {
      var xPosition = parseFloat(d3.select(this).attr("cx"));
      var yPosition = parseFloat(d3.select(this).attr("cy"));
      d3.select("#tooltip")
        .style("left", xPosition  "px")
        .style("top", yPosition + "px")
        .select("#value")
        .text(d.Name);
      d3.select("#tooltip").classed("hidden", false);})
  .on("mouseout", function() {
      d3.select("#tooltip").classed("hidden", true);});
# one more thing, add CSS to avoid mouseover event on legend circles
# but it also disable the fisheye effect and point masker!
# the legend transition effects are not well integrated with the hacked d3.js components
cat('
.nv-legendWrap {
    pointer-events: none;
}
.nv-controlsWrap {
    pointer-events: none;
    display: none;
}
')





# tooltip not working
d3.selectAll("circle")
.on("mouseover", function(d) {
    var xPosition = parseFloat(d3.select(this).attr("cx"));
    var yPosition = parseFloat(d3.select(this).attr("cy"));    
    
    .append("text")
    .attr("id", "tooltip")
    .attr("x", xPosition)
    .attr("y", yPosition)
    .attr("text-anchor", "middle")
    .attr("font-family", "sans-serif")
    .attr("font-size", "11px")
    .attr("font-weight", "bold")
    .attr("fill", "black")
    .text(d.Name);
});







