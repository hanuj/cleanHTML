cleanHTML<-function(w){
  
  if(grepl(pattern="<",x=w)){
    
    repeat{
      
      i<-regexpr(pattern="<",text=w,fixed=T)[[1]]
      
      if(i==-1){break}
      
      j<-regexpr(pattern=">",text=w,fixed=T)[[1]]
      
      a<-substr(x=w,start=i,stop=j)
      
      w<-gsub(pattern=a,replacement="",x=w,fixed=T)
      
    }
    
    w<-gsub(pattern="[^a-zA-Z0-9\\.,:/ ]",replacement="",x=w)
    
    w<-trimws(w)
    
    w<-gsub(pattern="  ",replacement=" ",x=w)
    
    w<-gsub(pattern="nbsb",replacement="",x=w,fixed=T)
    
    return(w)
    
  }else{
    
    w<-gsub(pattern="[^a-zA-Z0-9\\.,:/ ]",replacement="",x=w)
    
    w<-trimws(w)
    
    w<-gsub(pattern="  ",replacement=" ",x=w)
    
    w<-gsub(pattern="nbsb",replacement="",x=w,fixed=T)
    
    return(w)
    
  }
  
}
