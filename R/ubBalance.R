ubBalance <-
function(X,Y,type="ubSMOTE",...){
  
  if(any(is.na(Y))) 
    stop("Y has NAs")
  
  if(!is.factor(Y) || !all(Y %in% c(0,1))){
    print(summary(Y))
    stop("Y must be a factor taking values 0s (Majority class) and 1s (Minority class)")
  }
  
  if(length(type)>1) 
    stop("balance type does not support multiple selection")
  
  N.0<-length(which(Y==0))
  N.1<-length(which(Y==1))
  if(N.0==0) {
    cat("Warning: No negative instances, skip balance \n")
    return(list(X=X,Y=Y))
  }
  
  if(N.1==0) {
    cat("Warning: No positive instances, skip balance \n")
    return(list(X=X,Y=Y))
  }
  
  if(N.0==N.1) {
    #cat("Warning: N.1 == N.0, skip balance \n")
    return(list(X=X,Y=Y))
  }
  
  if(N.0<N.1) {
    cat("Warning: N.1",N.1,"N.0",N.0,"\n")
    stop("ERROR: 1 class is the majority class")
  }
  
  data<-NULL
    
  if(type=="ubOver")   	    data<-ubOver(X,Y)
  if(type=="ubUnder") 		  data<-ubUnder(X,Y,...)
  if(type=="ubSMOTE")   	  data<-ubSMOTE(X,Y,...)
  if(type=="ubOSS") 		    data<-ubOSS(X,Y,...)
  if(type=="ubCNN") 		    data<-ubCNN(X,Y,...)
  if(type=="ubENN") 		    data<-ubENN(X,Y,...)
  if(type=="ubNCL") 		    data<-ubNCL(X,Y,...)
  if(type=="ubTomek") 		  data<-ubTomek(X,Y,...)
  
  if(is.null(data))
    stop("technique",type," not supported")
  
  X<-data$X
  Y<-data$Y
  id.rm<-data$id.rm
  if(is.null(id.rm)) id.rm<-NA
  
  N<-length(Y)
  Id<-sample(1:N)
  if (!is.vector(X)) 
    X=X[Id,]
  else {
    #is.vector
    X=X[Id]
    if(any(is.na(X)))
      cat("WARNINGS: vector has NAs \n")
    if (all(X==X[1]))
      cat("WARNINGS: constant vector after '",type,"' \n")
  }
  
  Y=Y[Id]
  
  if(type!="unbalanced"){
    cat("Proportion of positives after '", type,"' :",
        round(length(which(Y==1))/N*100,digits=2),"% of",N,"total instances \n")	
  }
  
  
  return(list(X=X,Y=Y,id.rm=id.rm))
}
