#Build a corrplot equivalent of categorical variables, using Cramer's V

cramerv.plot <- function(y, low="white", high="black"){
  v<-data.frame(matrix(nrow=ncol(y)^2,ncol=3,
            dimnames = list(NULL, c("Var1","Var2","CramerV"))))
  v[,3] <- as.numeric(v[,3])
  r <- 1
  for(i in 1:ncol(y)){
    for(j in 1:ncol(y)){
      v[r,1] <- colnames(y)[i]
      v[r,2] <- colnames(y)[j]
      v[r,3] <- cv(y[,i],y[,j])
      r <- r+1
    }
  }
  v$Var1 <- factor(v$Var1)
  v$Var2 <- factor(v$Var2)
  ggplot(aes(x=Var1, y=Var2, fill=CramerV), data=v)+
  geom_tile() +
  scale_fill_gradient(low=low, high=high) +
  theme_classic()
}
