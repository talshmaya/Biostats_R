data<-read.table('midterm.txt', header=TRUE)
attach(data)

treatment.label<-c('Control','CBT','FT')
TREAT <- as.factor(treatment.label[data$Treat])
DIFF = data$Postwt - data$Prewt
PCNT.DIFF = (DIFF / data$Prewt) * 100

b<-cbind(TREAT,PCNT.DIFF)
boxplot(b[,2][TREAT=='CBT'],b[,2][TREAT=='Control'],b[,2][TREAT=='FT'],names=treatment.label)


median.test<-function(x, f, round=3){
  print(data.frame(cbind(n=table(f),
  median=round(tapply(x,f,median,na.rm=T),round),
  min=round(tapply(x,f,min,na.rm=T),round),
  max=round(tapply(x,f,max,na.rm=T),round))))
  
    #treat<-as.factor(x) #assuming we factor by first col
    n<-nlevels(as.factor(f))
    if(n<3){
      w <-wilcox.test(x~f)
      print(w$p.value)
      print(w$method)
  }
  else{
    k <- kruskal.test(x,f)
    print(k$p.value)
    k$method
  }
}

attach(survey)

median.test(data[,2],data[,1])
median.test(income,hbp)