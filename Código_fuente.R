#Punto 1
menu= function() {
  print("1. Exportar el conjunto de datos gapminder en formato “xlsx”. El 10 % de los valores de las columnas lifeEx, pop, y gdpPercap serán reemplazados de forma aleatoria por valores no asignados NA")
  print("2. Importar el archivo gapminder en formato “xlsx”")
  print("3. Graficar el diagrama de dispersión lifeEx vs pop")
  print("4. Graficar el diagrama de dispersión gdpPercap vs pop")
  print("5. Graficar los diagramas de cajas de la variable gdpPercap discriminados por continentes desde 1990 a 2007")
  
  val = as.integer(readline("ingresar opción"))
  if(val == 1){
    attach(gapminder)
    length(lifeExp)
    porc=length(lifeExp)*0.1
    
    h=sample (1:1704,170,replace = F)
    
    newgap = gapminder
    newgap$lifeExp[h]=NA
    newgap
    
    k = sample(1:1704,170,replace = F)
    newgap$pop[k]=NA
    newgap
    
    m = sample(1:1704,170,replace = F)
    newgap$gdpPercap[m] = NA
    newgap
    
    write.xlsx(newgap,"Experimento1.xlsx")
    read.xlsx("Experimento1.xlsx", sheetIndex = 1)
    
    }
    
  else if(val == 2){
    write.xlsx(gapminder,"Gapminder.xlsx")
    read.xlsx("Gapminder.xlsx", sheetIndex = 1)
    }
  
  else if(val==3){
    plot(gapminder$lifeExp,gapminder$pop)
    }
  
  else if(val==4){
    plot(log(gapminder$gdpPercap),log(gapminder$pop))
    }
  
  else if(val==5){
  
    diagrama=gapminder%>%select(gdpPercap,continent,year)%>%
      filter(year>1990)
    boxplot(gdpPercap ~ continent, data = diagrama)
    }
  
  else{
    print("selecciona un número que esté en el menú")
    }
  }
#Punto 2
menu2=function(p){
  print("1. Cargar dos archivos de datos en formato “csv” llamados “Experimento a.csv” y “Experimento b.csv” e indicar si la diferencia en la media de los datos es estadísticamente significativa." )
  print("2. Cargar dos archivos de datos en formato “csv” llamados “Experimento a.csv” y “Experimento b.csv” y mostrar en pantalla la correlación  de Pearson y Spearman de los datos.")
  print("3. Cargar dos archivos de datos en formato “csv” llamados “Experimento a.csv” y “Experimento b.csv” y graficar el diagrama de dispersión y una línea recta que aproxime los datos calculada por una regresión lineal por mínimos cuadrados.")
  
  val=as.integer(readline("Ingresar opción"))
  if(val==1){
    Expe1=read.csv("Experimento_a.csv")

    crimenes= Expe1%>%select(Año,Denuncias,Parámetro)%>%
      filter(Parámetro== "Secuestro"|Parámetro=="Tráfico de drogas")%>%
      group_by(Año)
   print(crimenes)
    
   ttest = t.test(data= crimenes , Denuncias ~ Parámetro)
   pvalor=ttest$p.value
   l=sprintf("el pvalor es %f",pvalor)
   print(l)
   print("Como el pvalor es menor a 0.05, la diferencia de las medias es estadísticamente significativa")
   
   Expe2=read.csv("Experimento_b.csv")
   migración= Expe2%>%select(Año,Periodo,Han.salido.de.España,Han.vuelto.a.España)%>%
      filter(Periodo=="Semestre 1"|Periodo=="Semestre 2")%>%
      group_by(Año)
   print(migración)
   
   ttest2=t.test(data=migración,Han.salido.de.España~Periodo)
   pvalor2=ttest2$p.value
   
   h=sprintf("el pvalor es %f",pvalor2)
   print(h)
   print("Como el pvalor es mayor a 0.05, la diferencia de las medias no es estadísticamente significativa")}
  else if (val==2){
    
    crimenes1= Expe1%>%select(Año,Denuncias,Parámetro)%>%
      filter(Parámetro== "Secuestro"|Parámetro=="Tráfico de drogas")%>%
      group_by(Año)
    print(crimenes1)
    correlación1=cor(crimenes1$Año,crimenes1$Denuncias)
    j=sprintf("La correlación de Pearson de los años y las denuncias en Experimento_a es %f",correlación1)
    print(j)
    
    correlación11=cor(crimenes1$Año,crimenes1$Denuncias,method = "kendall")
    n=sprintf("La correlación de Spearman de los años y las denuncias en Experimento_a es %f",correlación11)
    print(n)
    
    migración= Expe2%>%select(Año,Periodo,Han.salido.de.España,Han.vuelto.a.España)%>%
      group_by(Año)
    print(migración)
    correlación2=cor(migración$Han.salido.de.España,migración$Año)
    k=sprintf("La correlación de Pearson de las personas que han salido en España en Experimento_b es %f",correlación2)
    print(k)
    correlación22=cor(migración$Han.salido.de.España,migración$Año,method = "kendall")
    f=sprintf("La correlación de Spearman de las personas que han salido en España en Experimento_b es %f",correlación22)
    print(f)
  }
  else if (val==3){
    cor(crimenes1$Año,crimenes1$Denuncias)
    regresion = lm(Año ~ Denuncias, data = crimenes1)
    regresion
    summary(regresion)
    plot(crimenes1$Denuncias,crimenes1$Año, xlab='Denuncias', ylab='Años')
    abline(regresion)
    print("La primera grafica muestra el diagrama de dispersión y la línea recta que aproxima los datos de Experimento_a calculados por una regresión lineal por mínimos cuadrados")
    
    cor(migración$Han.salido.de.España,migración$Año)
    regresion2 = lm(Han.salido.de.España ~ Año, data = migración)
    regresion2
    summary(regresion2)
    plot(migración$Año,migración$Han.salido.de.España,ylab='Han salido de España', xlab='Año')
    abline(regresion2)
    print("La segunda grafica muestra el diagrama de dispersión y la línea recta que aproxima los datos de EXperimento_b  calculados por una regresión lineal por mínimos cuadrados")
  }
  else{
    print("selecciona un número que esté en el menú 2")
  }
}
#Punto 3
menu3=function(){
  print("1. Graficar la función de densidad de una distribución uniforme")
  print("2. Graficar la función de densidad de una distribución Bernoulli")
  print("3. Graficar la función de densidad de una distribución Poisson")
  print("4. Graficar la función de densidad de una distribución Exponencial")
  val=as.integer(readline("Ingresar opción"))
  if(val==1){
    plotunif = function(x, min = 0, max = 1, lwd = 1, col = 1, ...) {
      if (missing(x)) {
        x = seq(min - 0.5, max + 0.5, 0.01)
      }
      
      if(max < min) {
        stop("'min' must be lower than 'max'")
      }
      
      plot(x, dunif(x, min = min, max = max),
           xlim = c(min - 0.25, max + 0.25), type = "l",
           lty = 0, ylab = "f(x)", ...) 
      segments(min, 1/(max - min), max, 1/(max - min), col = col, lwd = lwd)
      segments(min - 2, 0, min, 0, lwd = lwd, col = col)
      segments(max, 0, max + 2, 0, lwd = lwd, col = col)
      points(min, 1/(max - min), pch = 19, col = col)
      points(max, 1/(max - min), pch = 19, col = col)
      segments(min, 0, min, 1/(max - min), lty = 2, col = col, lwd = lwd)
      segments(max, 0, max, 1/(max - min), lty = 2, col = col, lwd = lwd)
      points(0, min, pch = 21, col = col, bg = "white")
      points(max, min, pch = 21, col = col, bg = "white")
      plotunif(min = 0, max = 1, lwd = 2, col = 4, main = "FD uniforme")}}
  else if(val==2){
    x <- 1:80
    plot(dbinom(x, size = 80, prob = 0.2), type = "h", lwd = 2,
         main = "Función de probabilidad binomial",
         ylab = "P(X = x)", xlab = "Número de éxitos")
    lines(dbinom(x, size = 80, prob = 0.3), type = "h",
          lwd = 2, col = rgb(1,0,0, 0.7))
    legend("topright", legend = c("80  0.2", "80  0.3", "80  0.4"),
           title = "n     p", title.adj = 0.85,
           lty = 1, col = 1:3, lwd = 2, box.lty = 0)
  }
  else if(val==3){
    x <- 0:50
    lambda <- 5
    plot(dpois(x, lambda), type = "h", lwd = 2,
         main = "Función de masa de probabilidad",
         ylab = "P(X = x)", xlab = "Número de eventos")
  }
  else if(val==4){
    x <- seq(0, 8, 0.1)
    plot(x, dexp(x, 2), type = "l",
         ylab = "f(x)", lwd = 2, col = "red")
    lines(x, dexp(x, rate = 1), col = "blue", lty = 1, lwd = 2)
    legend("topright", c(expression(paste(lambda)), "2", "1"),
           lty = c(0, 1, 1), col = c("blue", "red"), box.lty = 0, lwd = 2)
  }
  else{
    print("Selecione un número que esté en el menú 3")
  }
}
