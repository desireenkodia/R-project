#'Calculates ttest
#'
#'
#'@param x y as dataset
#'@return model
#'@export

myttest<-function(x,y,alpha=0.05,paired=FALSE){
    #Validation of Variances
    var_x<-var(x)
    var_y<-var(y)
    ratio<-ifelse(var_x>var_y,var_x/var_y,var_y/var_x)
    F_Crit<-qt(alpha,df=length(x)-1,lower.tail = FALSE)
    bool<-ifelse(ratio<F_Crit,TRUE,FALSE)
    model<-t.test(x,y,var.equal = bool,alpha=alpha,paired = paired)

    #Defining Return Types
    type_test<-model$method
    conclusion<-ifelse(model$p.value<alpha,"We reject the null hypothesis, there is statistally significant difference in mean",
                       "We cannot reject the null hypothesis, there is no difference in mean")
    data<-c(list(x),list(y))
    result<-c("Test" = type_test,"Conclusion" = conclusion,model,"Data" = data,"Paired" = paired)
    return(result)
}

print<-function(result){
  result$Conclusion
}

plot<-function(result){
  if(result$Paired==FALSE){
    boxplot(result$Data1,result$Data2,ylab = "Values", xlab = "Variables", names=c("X","Y"))
  }
  else {
    boxplot(result$Data1-result$Data2,ylab = "Values", xlab = "Variables")
    points(result$conf.int[1])
    points(result$conf.int[2])
  }
}



