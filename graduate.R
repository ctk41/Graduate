require("gWidgets")
require("tseries")
require("astsa")
require("tcltk")
require("xtable")

options(guiToolkit = "RGtk2")

window <- gwindow("Arima Model Analysis", visible = FALSE)
paned <- gpanedgroup(cont = window)

##### Display file name
path <- "D:/Project code/Dropbox/Graduate/Do an/data"
group <- ggroup(cont = paned, horizontal = FALSE)

glabel("Path File: ", cont = group, anchor = c(-1, 0))
txt_pathFile = gedit("", initial.msg =  "Enter path file name", cont = group)


############################# Menu hien thi chuc nang ######y = ###################################
menu <- gframe(gettext("Menu"),cont = group, horizontal = TRUE, expand = TRUE)
lyt <- glayout(cont = menu)
listWidgets <- list()
##Button
lyt[4,1] <- (btn_showData = gbutton("Show Data", cont = lyt))
lyt[4,2] <- (btnRootTest = gbutton("Unit Root Test", cont = lyt))
lyt[2,1] <- "Graph"
lyt[2,2] <- (listGraph = gcombobox(c("","0", "1", "2"), cont = lyt))
lyt[6,1] <- (btn_ACF = gbutton("ACF", cont = lyt))
lyt[6,2] <- (btn_PACF <- gbutton("PACF", cont = lyt))
lyt[8,1] <- (btn_equation = gbutton("Equation", cont = lyt))
lyt[8,2] <- (btn_forecast <- gbutton("Forecast", cont = lyt))
lyt[10, 1] <- (btn_whiteNoise <- gbutton("Test White Noise", cont = lyt))

init <- function(){
}

init()

list = c()
oldList = c()
count = 0

#showdata
addHandlerClicked(btn_showData, handler = function(h,...){
  pathFile = svalue(txt_pathFile)
  pathDirect = getSubString(pathFile)
  
  if(pathFile == "") galert("Unknow file path", parent = window)
  else
  {
    gold = read.csv(pathFile, header = T, dec = ".", sep = ",")
    path_html = paste(pathDirect, "show_data.html", sep="")
    path_css = paste(pathDirect, "css/style.css", sep="")
    
    html.head <- paste("<head>" ,
                       "<link rel='stylesheet' type='text/css' href='css/style.css'/>",
                       "</head>",sep='\n')
    ## the html body
    html.table <- paste(print(xtable(gold),type='html', path_html, collapse = "\n"))
    html.body <- paste("<body><div id='show'>", html.table,"</div></body>")
    ## the html file
    write(paste(html.head,html.body,sep='\n'), path_html)
    childWindow = gwindow("", visible = TRUE)
    gtext("Du lieu da duoc hien thi qua file show_data.html", container=childWindow, font.attr=list(style="bold", size="15"))
  }
})

############# Hien thi cac graph ban dau, sai phan bac 1, sai phan bac ad
addHandlerChanged(listGraph, handler = function(h,...){
  pathFile = svalue(txt_pathFile)
  valueGraph = svalue(listGraph)
  print(valueGraph)
  if(pathFile == "") galert("Unknow file path", parent = window)
  else
  {
    stock = read.csv(pathFile, header = T, dec = ".", sep = ",")
    if(valueGraph == "0")
    {
      stockTimeseries = ts(stock[, 2], start = c(2013, 19, 9), end = c(2015, 6, 4), frequency = 365)
      plot.ts(stockTimeseries, type = "l", col = "red", lwd = 1)
    }
    else if(valueGraph == "1")
    {
      stockTimeseries = ts(diff(stock[, 2]), start = c(2013, 18, 9), end = c(2015, 6, 4), frequency = 365)
      plot.ts(stockTimeseries, type = "l", col = "red", lwd = 1)
    }
    else if(valueGraph == "2")
    {
      stockTimeseries = ts(diff(diff(stock[, 2])), start = c(2013, 18, 9), end = c(2015, 6, 4), frequency = 365)
      plot.ts(stockTimeseries, type = "l", col = "red", lwd = 1)
    }
  }
})
addHandlerChanged(btnRootTest, handler = function(h,...){
  childWindow = gwindow("Augmented Dickey Fuller", visible = TRUE)
  pathFile = svalue(txt_pathFile)
  pathDirect = getSubString(pathFile)
  
  if(pathFile == "") galert("Unknow file path", parent = window)
  else
  {
    gold = read.csv(pathFile, header = T, dec = ".", sep = ",")
    gold_diff = diff(gold[,2])
    
    adf = adf.test(gold[,2])
    adf1 = adf.test(gold_diff)
    
    path_html = paste(pathDirect, "unit_root_test.html", sep="")
    path_css = paste(pathDirect, "css/style.css", sep="")
    
    
    
    html.head <- paste("<head><meta http-equiv='Content-Type' content='text/html; charset=utf-8' />" ,
                       "<title>Chuong trinh kiem dinh Dickey-Fuler</title>",
                       "<link rel='stylesheet' type='text/css' href='css/unitRoot.css'/>",
                       "</head>",sep='\n')
    ## the html body
    html.table <- paste("<table>
                        <tr>
              						<th> Ki hieu chuoi</th>
            							<th colspan='2'>Gia tri kiem dinh</th>
            						</tr>
            						 <tr>
            							<td></td>
            							<td>Dickey-Fuler</th>
            							<td>P-value</td>
            						</tr>
                                    <tr><td>VNDEX</td>
            							<td>", toString(round(adf$statistic, digits = 3)),"</td>", 
            							"<td>", toString(round(adf$p.value, digits = 3)), "</td>
            						</tr>
            						 <tr>
            						 <td>DVNDEX</td>
            							<td>", toString(round(adf1$statistic, digits = 3)), "</td>",
            							"<td>", toString(round(adf1$p.value, digits = 3)),"</td>
            						</tr>
                        </table>")
    html.body <- paste("<body><div id='show'>", html.table,"</div></body>")
    write(paste(html.head,html.body,sep='\n'), path_html)
    
    gtext("Du lieu da duoc hien thi qua file  unit_root_test.html", container=childWindow, font.attr=list(style="bold", size="15"))
}
})
#========================= 2 ham ACF va PACF =====================================
addHandlerChanged(btn_ACF, handler =  function(h,...){
  plot.new()
  gold = read.csv(svalue(txt_pathFile), header = T, dec = ".", sep = ",")
  
  plot.new()
  acf = acf(diff(gold[,2]), lag.max = 20, xlim = c(0, 20), main ="AutoCorrelation Function")
  #createTable()
  createTable_ACF_PACF();
})

addHandlerChanged(btn_PACF, handler = function(h,...){
  plot.new()
  gold = read.csv(svalue(txt_pathFile), header = T, dec = ".", sep = ",")
  plot.new()
  pacf <- pacf(diff(gold[,2]), lag.max = 21, xlim = c(0, 20), main = "Part AutoCorrelation Function")
  #createTable()
  createTable_ACF_PACF();
})

createTable_ACF_PACF <- function(){
  pathFile = svalue(txt_pathFile)
  pathDirect = getSubString(pathFile)
  
  gold = read.csv(pathFile, header = T, dec = ".", sep = ",")
  lag = c(0 : 20)
  acf = acf(diff(gold[,2]), plot = FALSE, lag.max = 20)
  pacf = pacf(diff(gold[,2]), plot = FALSE, lag.max = 20)
  
  x = t(matrix(c(lag, acf$acf, 0, pacf$acf), nrow = 3, ncol = 21, byrow = TRUE))
  write.table(x,file = paste(pathDirect, "acf_pacf.csv"),row.names=FALSE, na="",col.names= c("LAG", "ACF", "PACF"), sep=",")
  
  acf_pacf = read.csv(paste(pathDirect, "acf_pacf.csv"), header = T, dec = ".", sep = ",")

  html.head <- paste("<head>" ,
                     '<link rel="stylesheet" type="text/css" href= "css/style.css"/>',
                     "</head>",sep='\n')
  ## the html body
  html.table <- paste(print(xtable(acf_pacf), type='html', paste(pathDirect, "acf_pacf.html"), collapse = "\n"))
  html.body <- paste("<body><div id='show'>", html.table,"</div></body>")
  ## the html file
  write(paste(html.head,html.body,sep='\n'), paste(pathDirect, "acf_pacf.html"))
}

addHandlerClicked(btn_whiteNoise, handler - function(h,...){
  childWindow = f
})

##=========== nhan dang cac mo hinh arima ==================================================
addHandlerChanged(btn_equation, handler = function(h,...){
  childWindow = gwindow("Nhan dang cac tham so cua AIRMA", visible = TRUE)
  lyt = glayout(cont = childWindow)
 
  lyt[1,1] <- (lbl_p <- glabel(text = "p"))  
  lyt[1,2] <- (txt_p <- gedit())
  lyt[2,1] <- (lbl_d <- glabel(text = "d"))  
  lyt[2,2] <- (txt_d <- gedit())
  lyt[3,1] <- (lbl_q <- glabel(text = "q"))  
  lyt[3,2] <- (txt_q <- gedit())
  lyt[4,1] <- (btn_ARIMA <- gbutton("Enter"))
  lyt[4,2] <- (btn_finish <- gbutton("Finish"))
  
  assign("list",list, envir = .GlobalEnv)
  assign("oldList", oldList, envir = .GlobalEnv)
  assign("count", count, envir = .GlobalEnv)
  
  addHandlerChanged(btn_ARIMA, handler =  function(h,...){
    count = count + 1
    p = as.numeric(svalue(txt_p))
    d = as.numeric(svalue(txt_d))
    q = as.numeric(svalue(txt_q))
    
    print(count)
    if(count != 1) {
      oldList  = c(list)
      print(oldList)
      list = c(oldList, c(p, d, q))
    }
    
    else{
      list = c(p, d, q)
    }
   
    
    print(list)
    print(length(list))
    assign("list",list, envir = .GlobalEnv)
    assign("oldList", oldList, envir = .GlobalEnv)
    assign("count", count, envir = .GlobalEnv)
  }) 
 
  
  addHandlerClicked(btn_finish, handler =  function(h,...)
  {
    print("========btn_finish==========")
    pathFile = svalue(txt_pathFile)
    gold = read.csv(pathFile, header = T, dec = ".", sep = ",")
    pathDirect = getSubString(pathFile)
    path_html = paste(pathDirect, "testARIMA.html")
    
    x = gold[,2]
    html_element = ""
    old_html_element = ""
    for(i in 1 : length(list)){
      
      if(i - round(i/3, digits = 0) * 3 == 1){
        p = list[i]
        d = list[i + 1]
        q = list[i + 2]
        assign("html_element",html_element, envir = .GlobalEnv)
        assign("old_html_element",old_html_element, envir = .GlobalEnv)
        
        arima = arima(gold[,2], order = c(p, d, q)) 
        n = length(gold[,2])
        k = p + d + q
        bic = arima$aic - 2 * k + k * log(n, base = exp(1)) 
        
        tss = var(x) * (length(x) - 1)
        rss = sum((arima$residuals)^2)
        r_square = ((tss - rss)/tss)
        see = sqrt(as.integer(rss/(length(x) - 2)))
        adjust_r_square = 1 - (rss/(n - k - 1))/(tss/(n - 1))
        
        print(paste(toString(arima$aic), " ", toString(bic), " ", toString(adjust_r_square), " ", toString(see)))
        old_html_element = html_element
        name_image = paste0("test_whiteNoise_ARIMA(",toString(p),",",toString(d),",",toString(q) ,").png")
        name_image2 = paste0("ARIMA(",toString(p),",",toString(d),",",toString(q) ,").png")
        
        coef = arima$coef
        for(i in 1 : length(coef)){
          coef[i] = round(coef[i], digits = 4)
        }
        
        html_element <- paste0(old_html_element,"<tr>",
                              "<td>ARIMA(", toString(p),",", toString(d),",", toString(q) ,")</td>",
                              "<td>", toString(round(bic, digits = 3)),"</td>", 
                              "<td>", toString(round(adjust_r_square, digits = 3)),"</td>",
                              "<td>", toString(round(see, digits = 3)),"</td>",
                              "<td>", toString(coef),"</td>",
                              "<td>",name_image2,"</td>",
                              "</tr>")  
        
        path_img = paste0(pathDirect, "image/", name_image)
        png(file = path_img)
        acf(arima$residuals, main = paste("Ki???m tra nhi???u tr???ng mô hình", paste0("ARIMA(",toString(p),",",toString(d),",",toString(q) ,")")))
        dev.off()
      }      
    }
    
    html.head <- paste("<head><meta http-equiv='Content-Type' content='text/html; charset=utf-8' />" ,
                       "<title>Bang tinh toan cac gia tri ARIMA</title>",
                       "<link rel='stylesheet' type='text/css' href='css/test_arima.css'/>",
                       "</head>",sep='\n')
    print(html_element)
    html.table <- paste("<table>
                            <tr>
                              <th>ARIMA Model</th>
                            	<th>BIC</th>
                            	<th>Adjusted R_SQuare</th>
                            	<th>SEE</th>
                            	<th>ARIMA Full</th>
                              <th>Test White Noise</th>
                        	  </tr>", html_element,
                                               
            							"</table>")
    
    html.body <- paste("<body><div id='show'>", html.table,"</div></body>")
    write(paste(html.head,html.body,sep='\n'), path_html)
    
    
    list = c()
    oldList = c()
    count = 0
    assign("list",list, envir = .GlobalEnv)
    assign("oldList", oldList, envir = .GlobalEnv)
    assign("count", count, envir = .GlobalEnv)
  })
})

##==================== Hien thi du bao ===============================
addHandlerChanged(btn_forecast, handler =  function(h,..){
 
})

getSubString <- function(s){
  u = 0;
  for(i in nchar(s) : 1){
    if(substring(s, i, i) == '/'){
      u = i;
      break
    }
  }  
  substring(s, 1, u)
}

##Dung ggrahics de hien thi len frame
gg = ggraphics(cont = paned)

visible(window) = TRUE
# D:/Project code/Dropbox/Graduate/Do an
# D:/Project code/Dropbox/Graduate/Do an/ctk41.csv
#residual sum of squares
#p = 0, 10, q = 1, 10, 13
#
#
