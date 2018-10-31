cleanHTML<-function(w){
  
  if(grepl(pattern="<",x=w)){
    
    l<-gregexpr(pattern="<",text=w,fixed=T)[[1]]
    
    for(k in 1:length(l)){
      
      i<-gregexpr(pattern="<",text=w,fixed=T)[[1]]
      
      j<-gregexpr(pattern=">",text=w,fixed=T)[[1]]
      
      if(length(i)==1){
        
        a<-substr(x=w,start=i,stop=j)
        
        w<-gsub(pattern=a,replacement="",x=w,fixed=T)
        
        break
        
      }else{
        
        a<-substr(x=w,start=i[k],stop=j[k])
        
        w<-gsub(pattern=a,replacement="",x=w,fixed=T)
        
      }
      
    }
    
    w<-gsub(pattern="[^a-zA-Z0-9\\.,:/ ]",replacement="",x=w)
    
    w<-trimws(w)
    
    w<-gsub(pattern="  ",replacement=" ",x=w)
    
    return(w)
    
  }
    
}
