#'control de engorda
#'
#'realiza el control de la produccion en un sistema de engorda, con la finanlidad de conocer como se esta desarrollado nuestro sistema.
#'
#'@param df(dataframe)a nustros datos originales los forzamos a ser un dataframe.
#'@return un data frame con la gdp,conversion alimenticia,costo alimentacion y una grafica de la reacion del tiempo con el peso.
#'@export
c_e<-function(df1, d=30,ds=7){
  gdp <- df1$peso4-df1$peso1/d
  df1$gdp <-gdp
  ca <- df1$cons_alim/df1$gdp
  df1$ca <-ca
  c_a1<-df1$ca*df1$precio_kg_a1
  df1$c_a1 <- c_a1
  c_a2<-df1$ca*df1$precio_kg_a2
  df1$c_a2 <- c_a2
  pp1 <- mean(df1$peso1)
  pp2 <- mean(df1$peso2)
  data_frame0<-data.frame(peso1=c(df1$peso1),
                          peso2=c(df1$peso2),
                          peso3=c(df1$peso3),
                          peso4=c(df1$peso4))
  # Aplicar la función mean a cada columna del data frame
  promedios<-apply(data_frame0, MARGIN=2, FUN=mean)
  x<-c(1,2,3,4)#semanas
  y<-c(promedios )
  plot(x,y,
       main="relacion de engorde con tiempo",
       xlab="semanas", ylab="peso")
  abline(lm(y~x), col="blue", lwd=3)


  return(df1)
}


