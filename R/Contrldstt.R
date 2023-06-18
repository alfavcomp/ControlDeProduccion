#'control de lechones
#'
#' realiza el control de la produccion dentro de un sistema de crianza de lechones con la finalidad de conocer parametros que nos indiquen el desarrollo de nuestro sistema.
#'
#'@param df(dataframe)nustros datos originales los forzamos a ser un dataframe
#'@return un dataframe con los promedios de peso por animal, total de lechones vivos y muertos, el numero de lechones muertos y la mortalidad predestete.
#'@export
cdl<-function(df){

  kg_prom_animal<-df$peso_total/df$lnv
  df$kg_prom_animal <-  kg_prom_animal
  total_lnv <- sum(df$lnv)
  total_ld<-sum(df$nld)
  lech_muert <- total_lnv-total_ld
  mort_pd <- (lech_muert/total_ld)*100
  df$mort_pd <- mort_pd

  n <- kg_prom_animal < 4.5
  m <- kg_prom_animal > 4.5
  df$crias_bp <- n
  df$crias_pa <- m
  x<-c(df$lnv)
  y<-c(df$nld)
  plot(x,y,
       main="relacion lechones nacidos y lechones destetados",
       xlab="lechones nacidos", ylab="mortalidad")
  abline(lm(y~x), col="blue", lwd=3)
  return(df)
}

