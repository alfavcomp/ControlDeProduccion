#' contro maternidad
#'
#' realiza el control de la maternidad en un sistema de maternidad con la finalidad de conocer el desarrollo de nustro sistema.
#'
#' @param df(dataframe)forzamos a nustros datos originales a ser un dataframe
#' @return un dataframe con los porcentajes de mortalidad, promedio de cerdos nacidos,prolificidad y productividad de cerdas.
#' @export

#mostrar la direccion de lo base de datos
ruta<-"C:/Users/favian/Documents/datosR"
setwd(ruta)
#exportando los datos
datos3<-openxlsx::read.xlsx("ciclo_productivo_cerdas_por_año.xlsx")
View(datos3)
df2<-data.frame(datos3)
print(df2)
c_m<-function(df2){
  porc_mort <- (df2$nlm_por_camada*100)/df2$nlnv
  df2$porc_mort <- porc_mort
  prom_cnc<-(df2$nlnv/df2$no_partos)
  df2$prom_cnc <- prom_cnc
  porc_cn <- (df2$prom_cnc/df2$no_partos)*100
  df2$porc_cn <- porc_cn
  prolificidad<-(df2$porc_cn/df2$no_partos)*100
  df2$prolificidad <- prolificidad
  prod_cerdas <- df2$no_partos*prolificidad*(1-porc_mort/100)
  df2$prod_cerdas <- prod_cerdas

  x<-c(df2$nlnv)
  y<-c(df2$nld )
  plot(x,y,
       main="relacion lechones nacidos y lechones destetados",
       xlab="madres", ylab="crias")
  abline(lm(y~x), col="black", lwd=2)
  x<-c(df2$no_partos)
  y<-c(df2$nlm_por_camada )
  plot(x,y,
       main="relacion de partos con mortaliada",
       xlab="partos", ylab="mortalidad")
  abline(lm(y~x), col="black", lwd=3)

  return(df2)

}

