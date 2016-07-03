threshold_Curves <- function(values,classes){
  
  par(mfrow(1,2))
  library(ROCR)
  pred <- prediction(values, classes.cv)
  roc.perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  plot(roc.perf, colorize = T)
  abline(a=0, b= 1)
  
  X <- 0
  i <- 0
  while(x <= 1.0) {
    i = i + 1
    temp.values <- values
    temp.values[which(temp.values <= x)] = 0
    temp.values[which(temp.values > x)] = 1
    
    chek <- classes-temp.values
    
    spec[i] <- length(which(chek[which(classes == 0)] == 0))/length(which(classes == 0))
    sens[i] <- length(which(chek[which(classes == 1)] == 0))/length(which(classes == 1))
    acc[i] <- length(which(chek == 0))/length(classes)
    xplot[i] <- x
    x <- x+0.02; 
    
  }
  
  plot(xplot,sens*100,type = "b",col = 'blue',ylim = c(0,100),xlab = 'Threshold' , ylab = '%')
  lines(xplot,spec*100,type = "b",col = 'magenta')
  lines(xplot,acc*100,type = "b",col = 'orange')
  lines(xplot,50*(sens+spec),type = "b",col = 'red')
  
  legend("bottomright", inset= 0.01,
         c('Sensitivity','Specificity','Accuracy','(Sensitivity+Specificity)/2'), fill=c('blue','magenta','orange','red'), horiz=FALSE)
  
}