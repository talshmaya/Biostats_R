#--------------------------------------------------------------------
# PH700A - Data Analysis using R
# Spring 2017
# Functions for uni- and bi-variate stats
#--------------------------------------------------------------------


# Despriptive statistics such as n, mean, sd, median, range of input variable x.
# You determine if the variable x is symmetric or skewed, the default argument 
# is the variable x is symmetric (skewed=FALSE).


#	DESCRIPT()
descript<-function(x, skewed=F){
  if(skewed){
    n<-length(which(!is.na(x)))
    med<-median(x,na.rm=T)
    min<-min(x,na.rm=T);max<-max(x,na.rm=T)
    cat('n=',n,', Median(Range) is',med,'(',min,'-',max,')\n')
  }
  else{
    n<-length(which(!is.na(x)))
    xbar<-round(mean(x,na.rm=T),1);sx<-round(sd(x,na.rm=T),1)
    cat('n=',n,', Mean(SD) is ',xbar,'(',sx,')\n')
  }
}

# Creates a table containing n, percent for a categorical variable.

#	FREQ()
freq<-function(x,round=3){
  data.frame(cbind(n=table(x),percent=round(table(x)/sum(table(x)),round)*100))
}


# Calculate the group means, where the variable x is quanitative and the variable f is a factor.

#  	MEANS.GRP()
means.grp<-function(x,f,round=3){
  data.frame(cbind(n=table(f[x!="NA"]),mean=round(tapply(x,f,mean,na.rm=T),round),sd=round(tapply(x,f,sd,na.rm=T),round)))
}

