library(plotly)
library(ggplot2)

numericDistr <- function(data){
  # make histograms for each numeric column
  p1 <- plot_ly(data = data, x = ~age, color = ~yearly.income, type ="histogram", showlegend = TRUE, name = "Age") %>% layout(title = "Age")
  p2 <- plot_ly(data = data, x = ~fnlwgt, color = ~yearly.income, type ="histogram", showlegend = T, visible = TRUE)  %>% layout(title = "FnlWgt")
  p3 <- plot_ly(data = data, x = ~education.num, color = ~yearly.income, type ="histogram", showlegend = T)  %>% layout(title = "Education Num")
  p4 <- plot_ly(data = data, x = ~hours.per.week, color = ~yearly.income, type ="histogram", showlegend = T)  %>% layout(title = "Hours per Week")
  
  p <- subplot(p1, p2, p3, p4,nrows = 2); p
  
  # examine capital gains and losses
  p5 <- plot_ly(data = data, x = ~capital.gain, color = ~yearly.income, type ="histogram", showlegend = TRUE)  %>% layout(title = "Capital Gain")
  p6 <- plot_ly(data = data, x = ~capital.loss, color = ~yearly.income, type ="histogram", showlegend = TRUE)  %>% layout(title = "Capital Loss")

  p_capital <- subplot(p5, p6); p_capital
  
  # zoom in on capital gains greater than 0
  p5 <- plot_ly(data = data[which(data$capital.gain > 0),], x = ~capital.gain, color = ~yearly.income, type ="histogram", showlegend = TRUE)  %>% layout(title = "Capital Gain")
  p6 <- plot_ly(data = data[which(data$capital.loss > 0),], x = ~capital.loss, color = ~yearly.income, type ="histogram", showlegend = TRUE)  %>% layout(title = "Capital Loss")

  p_capital <- subplot(p5, p6); p_capital
  
}

boxPlots <- function (data){
  # view the standardized variances
  p1 <- plot_ly(data = data, x = ~age, type ="box", showlegend = TRUE, name = "Standard Scores")  
  p2 <- plot_ly(data = data, x = ~fnlwgt, type ="box", showlegend = TRUE, name = "Final Weight")  
  p3 <- plot_ly(data = data, x = ~education.num, type ="box", showlegend = TRUE, name = "Education Yrs")  
  p4 <- plot_ly(data = data, x = ~hours.per.week, type ="box", showlegend = TRUE, name = "Hrs / Week")  
  
  p <- subplot(p1, p2, p3, p4, shareX = TRUE, shareY =TRUE)
  p  
}

categDistr <- function(data){
  # make plots for each categorical
  p1 <- plot_ly(data = data, x = ~workclass, color = ~yearly.income, type ="histogram", showlegend = TRUE)  %>% layout(title = "Working Class")
  p2 <- plot_ly(data = data, x = ~education, color = ~yearly.income, type ="histogram", showlegend = TRUE)  %>% layout(title = "Education")
  p3 <- plot_ly(data = data, x = ~marital.status, color = ~yearly.income, type ="histogram", showlegend = FALSE)  
  p4 <- plot_ly(data = data, x = ~occupation, color = ~yearly.income, type ="histogram", showlegend = TRUE)  %>% layout(title = "Occupation")
  p5 <- plot_ly(data = data, x = ~relationship, color = ~yearly.income, type ="histogram", showlegend = TRUE)  
  p6 <- plot_ly(data = data, x = ~race, color = ~yearly.income, type ="histogram", showlegend = TRUE)   %>% layout(title = "Race")
  p7 <- plot_ly(data = data, x = ~sex, color = ~yearly.income, type ="histogram", showlegend = FALSE)  
  p8 <- plot_ly(data = data, x = ~native.country, color = ~yearly.income, type ="histogram", showlegend = FALSE)  
  p9 <- plot_ly(data = data, x = ~yearly.income, color = ~yearly.income, type ="histogram", showlegend = FALSE)  
  
}

unknownData <- function(categUnknown, n){
  unknownRec <- categUnknown[10,'Number of Unknown']
  
  # remove grid lines
  ax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )
  
  # define colors and labels for the plot
  colors <- c('rgb(211,94,96)','rgb(128,133,133)')
  labelVals <- c(">= 1 Missing Value","0 Missing Value")
  
  # make 4 pie charts for the total missing count and each feature with at least 1 missing
  p1 <- plot_ly(data = categUnknown[10,],  values = c(unknownRec,n), 
                insidetextfont = list(color = '#FFFFFF'),
                labels = labelVals, type="pie", marker = list(colors = colors)) %>%
    layout(title = "Total Records with Unknowns", xaxis = ax, yaxis = ax,
           legend = list(orientation = 'h')); p1
  
  p2 <- plot_ly(data = categUnknown[1,], values = c(categUnknown[1,2], n),
                insidetextfont = list(color = '#FFFFFF'),
                labels = labelVals, type="pie", marker = list(colors = colors)) %>%
    layout(title = "Working Class Unknowns", xaxis = ax, yaxis = ax,
           legend = list(orientation = 'h')); p2
  
  p3 <- plot_ly(data = categUnknown[4,], values = c(categUnknown[4,2], n),
                insidetextfont = list(color = '#FFFFFF'),
                labels = labelVals, type="pie", marker = list(colors = colors)) %>%
    layout(title = "Occupation Unknowns", xaxis = ax, yaxis = ax,
           legend = list(orientation = 'h')); p3
  
  p4 <- plot_ly(data = categUnknown[8,], values = c(categUnknown[8,2], n),
                insidetextfont = list(color = '#FFFFFF'),
                labels = labelVals, type="pie", marker = list(colors = colors)) %>%
    layout(title = "Native Country Unknowns", xaxis = ax, yaxis = ax,
           legend = list(orientation = 'h')); p4

}

