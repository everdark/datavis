R and D3
========================================================

R的作圖能力是公認地強悍，雖然在`base`套件裡的作圖語法十分雜亂、參數不容易微調，但我們還有許多其他的套件可以選擇，例如`ggplot2`就是在語法上有良好的統一之外，圖像各個細部上的自動（預設）微調也十分到位。一般來說，以數據分析師的角度，在進行Data Exploration階段時，我習慣順手就用`base`套件作圖，寫個幾行dirty code馬上查看結果，很夠用了。但當分析階段來到了需要對外進行溝通的時候，比方說製作文檔、進行簡報等等，我會改用`ggplot2`（另一個常被人推崇的是`lattice`套件）來「認真地」作圖。

> 我對R的作圖一直很滿意，直到我膝蓋中了一箭。

時代是網路，以Web基準來進行數據分析結果的溝通已是十分尋常的事情。翻譯成資訊科學的語言，也就是HTML、CSS、與Javascript吧！前陣子我才接觸了現在當紅的一套基於Javascript的作圖套件：[D3（Data-driven Document）](http://d3js.org/)。

並不是所有人都把D3當作是一個「作圖」用的js套件，但對我來說這個議題並不重要。我在乎的是他可以畫出**很潮**的圖。一言以蔽之，要有「互動性」。互動性未必就一定要很潮——其實很多D3的圖都花費功夫在製造潮度，讓人覺得這張圖很炫，重點是，背後的資料不會改變、你透過它而想傳遞的訊息也一樣——但如果我可以在一個Scatter Plot上讓使用者隨著滑鼠游標指向資料點，而能提供額外的訊息，那麼這樣的方式就有其數據分析的價值。

好吧，其實潮一點也不錯。哼。

身為一個R的狂信者，我認為不能輸在這裡！（輸什麼啦）
於是我決定也要來研究D3，並且我要試圖在R的環境裡完成Web基準的資料圖像化。想當然耳，很快地我就發現已經有**一海票的先行者們**在做這件事情，其中[`rCharts`這個套件](http://rcharts.io/)額外有趣，我認為有非常好的潛力，雖然它現在還在開發中，而且幾乎沒有文檔可諮詢，不過我還是決定來玩它一玩！

這個套件現在不在CRAN上，我們可以透過`devtool`直接從[作者的Github](https://github.com/ramnathv/rCharts)安裝。


```r
# check required packages
if (!"rCharts" %in% dir(.libPaths())) {
    if (!"devtools" %in% dir(.libPaths())) 
        install.packages("devtools")
    devtools::install_github("rCharts", "ramnathv")
}
```


簡單地說，`rCharts`試圖把好用的高階Javascript作圖套件都整合到R的環境裡，讓使用者可以在**不必接觸**Javascript的情況下完成Javascript可以創造的圖像互動性。這不是很棒嗎？

嘿對，然後我就**開始研究**Javascript了。
——這跟說好的不一樣啊！

沒辦法，由於`rCharts`缺乏說明文檔，又還在開發階段，甚至連一個函式裡面有哪些參數可以用都是無法可循的，所以我們需要去挖一下人家的原始碼才能做更多的探索。這又引導我來到了另一個Javascript作圖套件：[NVD3](http://nvd3.org/)。

情況變得好像有點複雜，我來整理一下。首先，D3是一個讓我們可以很有效地針對可縮放向量圖形（SVG, Scalable Vector Graph）進行操作的Javascript套件，但實際使用它來實作一張圖仍然是相對底層的工作，想像一張Scatter Plot，你眼睛看到的每個元素你都得自己來——包括兩軸的線、刻度、縮放比例、每個小圓點……——這實在有點累。確實，比起你自己去手動刻一個`<svg>`物件出來，D3已經是相對「高階」，它有很多好用又省事的Javascript函式。但我們也都知道人的惰性比奇萊山還堅定，作圖？不就是我想畫Scatter Plot就有個函式叫做Scatter Plot然後我把資料丟進去它就把圖吐出來，搞定。嗯，在R的環境裡這件事情差不多是這樣沒錯，我們有非常多高階的作圖函式隨手取用，以及很多不錯彈性的參數來做進一步的調整。

> 潮是需要代價的。

也因為D3仍然相對底層，但它的潛力又非常好，所以有了許多基於D3的高階Javascript作圖套件就誕生了，其中一個就是NVD3。而`rCharts`嘗試整合的許多Javascript作圖套件中，NVD3就是其中一個。

好，故事差不多可以開始講了。（！
一切就從下面這個我隨便挑的資料所產生的很隨便的圖開始吧。


```r
case <- iris[, 3:5]
colnames(case) = gsub("\\.", "", colnames(case))  # '.' cause reference problem in JS
case$Name <- paste("N", round(runif(nrow(case)), 3) * 1000, sep = "")
head(case)
```

```
##   PetalLength PetalWidth Species Name
## 1         1.4        0.2  setosa N755
## 2         1.4        0.2  setosa N519
## 3         1.3        0.2  setosa   N8
## 4         1.5        0.2  setosa N677
## 5         1.4        0.2  setosa N158
## 6         1.7        0.4  setosa N734
```


使用R的`base`套件，基本上用兩行就可以畫出基本的Scatter Plot。


```r
plot(case[, 1:2], col = c(1:3)[case$Species], pch = 19)
legend("bottomright", levels(case$Species), col = c(1:3), pch = 19)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


但這張圖的「背後」其實有很多缺陷，比方說標籤位置很不容易自動最佳化。接下來嘗試看看用`ggplot2`的方案。


```r
library(ggplot2)
AES <- aes(x = PetalLength, y = PetalWidth, group = Species, color = Species)
ggplot(case, AES) + geom_point(size = 3) + theme(legend.position = "top")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


`ggplot2`的作圖並不單只是細節上的修飾，更重要的是語法的統合，使得畫任何一種圖所需要的語法與參數都是高度整合的。不過因為使用特殊的語法，所以想要用它就非得特別學習它的語法，不過就它所能帶來的價值而言，我認為是非常值得的。

接下來就是重頭戲了。讓我們看看`rCharts`帶來了些什麼火花！


```r
library(rCharts)
nn <- nvd3Plot(PetalWidth ~ PetalLength, data = case, type = "scatterChart", 
    group = "Species", xAxis = list(axisLabel = colnames(case)[1]), yAxis = list(axisLabel = colnames(case)[2]), 
    chart = list(showDistX = TRUE, showDistY = TRUE, showControls = TRUE))
nn
```


<iframe style="width: 100%; height: 400px;" src="https://dl.dropboxusercontent.com/u/210177/R/datavis/demo/file12f044377a8.html" id="frameid"></iframe>

這張圖有些特別的效果，包括除了**潮度滿點之外完全沒有意義**的魚眼效果，以及好像有那麼一點意思又不是非常有意思的群組移除效果，另外還有對應mouseover會有的一些特效，並顯示座標。顯示座標這點其實不差，但難道不能顯示更有價值的資訊嗎？事實上這些特效都遷就於NVD3的作圖函式`scatterChart`，它的[原碼在此](https://github.com/novus/nvd3/blob/master/src/models/scatterChart.js)。

我希望這張圖能有個標題，然後我不要魚眼效果（這點已知可以從`nvd3Plot`直接下參數擋掉）、也不需要群組移除的效果，但我希望滑鼠移到點上的時候，能夠顯示那個點所代表的字串，也就是我在一開始額外追加的一行（隨機）資料。為了達成這個目的，必須使用Javascript駭進上面`nvd3Plot`吐出的成品才行。

> 不過我還有一個堅持：必須在R的環境裡面以自動化的方式完成。

怎麼做呢？先來確定必須修改的部分。首先我要追加一個容納每個點可以顯示其額外關聯字串的tooltip，所以需要一個div標籤來裝它。

```
<div id="tooltip" class="hidden">
    <p><strong>Name</strong></p>
    <p><span id="value">name_here</span></p>
</div>
```

然後用CSS來格式化它，CSS的內容我直接參考了[Scott Murray的書](http://chimera.labs.oreilly.com/books/1230000000345/index.html)。（順帶一提這是一本介紹D3非常棒的書！）

```
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
```

注意最後我還追加了.nv-legendWrap這個類別的指標事件遮罩，目的是擋掉群組移除效果。至於我為什麼知道這個類別的名字？當然是去看了上面那張圖的原碼咩。

接下來就是D3的部分了！我要製造滑鼠互動事件：

```
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
```

這邊運用的原理是當滑鼠移動到點上時就把我們的tooltip隱形屬性拿掉，但滑鼠一移開又要立刻裝回來這樣。從這邊我們就可以清楚看到D3的優勢：**非常方便地追加互動性**。

好，重點是我要在R的環境裡完成上面這些事情，意思是`nvd3Plot`完成畫圖後我要立刻把輸出的HTML給重新讀進R——以純文字的格式。所以我要找到適合每段Javascript或者CSS的插入點，然後在R裡面把上面的程式碼原碼照列地插入到HTML中，完成後再把它輸出！

下面這個函式可以完成這件事情：
（這邊我沒有用highlight因為Rmarkdown內建的highlighter無法順利處理裡面一些verbatim的結構……）

```
## helper function of visualizeDynamic
injectScript <- function(script, insert, which.line) {
    script <- c(script[1:(which.line-1)],  insert, script[-(1:(which.line-1))])
    script
}

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
            var url = "http://google.com/search?q=";
            url += d.Name;
            window.location = url;}
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
```

全部在R環境下完成！
來看看[結果](https://dl.dropboxusercontent.com/u/210177/R/datavis/demo/scatterChart_hacked.html)吧。

事實上我還追加了超連結事件，所以如果你點擊一個點，就會導向Google搜尋頁面搜尋那個字串的結果！不過顯然在這個站被X-Frame-Options給擋住了……各位可以開console後點擊看看，就會看見頁面跳轉的事件發生（然後被阻止……）。


結論。
目前看起來，`rCharts`的野心很大，所以可以讓我們很容易用非常高階的方式在R環境裡就完成Web基準的互動性SVG作圖，但畢竟`rCharts`採用的都是高階Javascript套件，因此客製化的彈性會比較小，這個時候我們還是可以回歸到D3的作法，相當程度地補強一些客製化的需求。

當然，R與Javascript的作圖整合，這裡介紹的也只是眾多嘗試中的其中一個方向。事實上還有另一個我也打算深入研究的方向，我在[這邊](http://timelyportfolio.github.io/gridSVG_intro/)看到了覺得很有意思。作法基本上是，用R的作圖引擎完成圖形，然後轉成SVG格式（這部分有套件可以很無痛地完成，所以不成問題），然後再使用D3追加互動性。換言之，只仰賴Javascript添加互動性，而圖形的骨肉都還是以R完成。這裡比較需要動腦的地方是D3的Data Binding必須在圖產生之後才回過來做，跟以D3作圖的方式剛好相反！

有時間會朝這方面研究看看，畢竟「互動性」始終才是我在乎的焦點。
