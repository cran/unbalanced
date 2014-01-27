ubUnder <-
function(X,Y,propMinClass=50,w=NULL){
  N<-length(Y)
  
  propMajClass<-100-propMinClass
  classRatio=propMinClass/propMajClass
  
  i.1<-which(Y==1)
  N.1<-length(i.1)
  i.0<-which(Y==0)
  N.0<-length(i.0)
  if(N.1>=N.0) {
    stop("less 0s instances than 1s, the minority class has to be class 1")
  }
  
  p.1<-N.1/N
  if(propMinClass<(p.1*100))
    stop("Minority class accounts for ",round(N.1/N*100,digits=2),
         "%, propMinClass = ",propMinClass,"means removing minority observations")
  
  if(propMinClass>100)
    stop("propMinClass must be less than 100")
  
  N.0.sub<-round(N.1/classRatio)
  
  #if the weights are not given, assign equal probability to all examples.
  if(is.null(w))
    w<-rep(1/N.0,N.0)
  
  if(N.0.sub<=N.0)
    i.0.sub<-sample(i.0,N.0.sub,prob=w)
  else
    #N.0.sub>N.0"
    stop("subset of majoirty instances bigger than orginal set of majoirty instances")
  
  
  i.0.rm<-setdiff(i.0,i.0.sub)
  
  Id=c(i.0.sub,i.1)
  # Id<-sample(Id)
  if (is.vector(X)!=TRUE) X=X[Id,]
  else  X=X[Id]
  Y=Y[Id]
  
  return(list(X=X,Y=Y,id.rm=i.0.rm))
}
