## Limma wrapper

fit.limma <- function(data,MM,CM){
  m <- lmFit(data,MM)
  cm <- contrasts.fit(m,CM)
  ecm <- eBayes(cm)
  return(ecm)
}
