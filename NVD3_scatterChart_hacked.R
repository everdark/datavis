
## check required packages
if ( !'rCharts' %in% dir(.libPaths()) ) {
    if ( !'devtools' %in% dir(.libPaths()) ) install.packages('devtools')
    devtools::install_github('rCharts', 'ramnathv')
}

library(rCharts)

## create sample data
case <- iris[,3:5]
colnames(case) = gsub('\\.', '', colnames(case)) # '.' cause reference problem in js
case$Name <- paste('N', round(runif(nrow(case)),3)*1000, sep='')
str(case)
head(case)

## helper function of visualizeDynamic
injectScript <- function(script, insert, which.line) {
    script <- c(script[1:(which.line-1)],  insert, script[-(1:(which.line-1))])
    script
}
injectScript <- compiler::cmpfun(injectScript)


## scatter plot by NVD3 and d3.js
visualizeDynamic <- function(case, fname='scatterChart_hacked') {
    
    require(rCharts)
    
    fname <- paste(fname, '.html', sep='')
    nn <- nvd3Plot(
        PetalWidth ~ PetalLength, data=case, type='scatterChart', 
        group='Species', 
        xAxis=list(axisLabel=colnames(case)[1]), 
        yAxis=list(axisLabel=colnames(case)[2]), 
        chart=list(showDistX=TRUE, showDistY=TRUE, showControls=FALSE)
    )
    nn$save(destfile=fname)    
    html <- readLines(fname)
    
    # add CSS style for tooltip, block legend pointer event
    tfile <- file()
    cat(
        '
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
        .nv-legendWrap {
            pointer-events: none;
        }
        ',
        file=tfile)
    css <- readLines(tfile, warn=FALSE)
    close(tfile)
    eof_style <- grep('</style>', html, fixed=TRUE)
    html <- injectScript(html, css, eof_style)
    
    # add div element for tooltip
    tfile <- file()
    cat(
        '
        <div id="tooltip" class="hidden">
            <p><strong>Name</strong></p>
            <p><span id="value">name_here</span></p>
        </div>
        ',
        file=tfile)
    tooltip_div <- readLines(tfile, warn=FALSE)
    close(tfile)    
    eof_body <- grep('</body>', html, fixed=TRUE)
    html <- injectScript(html, tooltip_div, eof_body)
    
    # add pointer event via D3
    tfile <- file()
    cat(
        '      
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
        ',
        file=tfile)
    tooltip_d3js <- readLines(tfile, warn=FALSE)
    close(tfile)
    insert_d3js <- grep('nv.utils.windowResize(chart.update)', html, fixed=TRUE)
    html <- injectScript(html, tooltip_d3js, insert_d3js)
    
    # add click event to Domain/IP Census
    tfile <- file()
    cat(
        '
        d3.selectAll("circle")
          .on("click", function(d) {
            var domaincensus = "http://google.com/search?q=";
            domaincensus += d.Name;
            window.location = domaincensus;}
        );
        ',
        file=tfile)
    hlink_d3js <- readLines(tfile, warn=FALSE)
    close(tfile)
    insert_d3js <- grep('nv.utils.windowResize(chart.update)', html, fixed=TRUE)
    html <- injectScript(html, hlink_d3js, insert_d3js)
    
    # add graph title via D3 method
    gtitle <- 'NVD3 template hacked'
    gtitle_js <- paste('  .text("', gtitle, '");')
    tfile <- file()
    cat(
        '  
        d3.select("#" + opts.id)
          .append("text")
          .attr("x", 200)             
          .attr("y", 100)
          .attr("text-anchor", "middle")  
        ',  gtitle_js, sep='', file=tfile)
    js_title <- readLines(tfile, warn=FALSE)
    close(tfile)
    insert_d3js <- grep('nv.utils.windowResize(chart.update)', html, fixed=TRUE)
    html <- injectScript(html, js_title, insert_d3js)
    
    # output modified html
    writeLines(html, fname)
    file.show(fname)
}

visualizeDynamic(case=case)



