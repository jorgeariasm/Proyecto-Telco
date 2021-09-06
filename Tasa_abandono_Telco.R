#Formato Rmarkdown

---
Proyecto: "Proyecto TELCO, tasa de abandono"
Recurso: "Codigo de desarrollo"
Autor: Jorge Arias
editor_options: 
  chunk_output_type: console
---

0. - Parametros

```{r}
options(scipen=999)#Desactiva la notacion cientifica
```


1. - Preparacion del entorno
1.1 - Cargamos las librerias que vamos a utilizar

```{r}
#lista de paquetes que vamos a usar
paquetes <- c('data.table',#para leer y escribir datos de forma rapida
              'dplyr',#para manipulacion de datos
              'tidyr',#para manipulacion de datos
              'ggplot2',#para graficos
              'randomForest',#para crear los modelos
              'ROCR',#para evaluar modelos
              'purrr',#para usar la funcion map que aplica la misma funciona a varios componentes de un dataframe
              'smbinning',#para calcular la para importancia de las variables
              'rpart',#para crear arboles de decision
              'rpart.plot'#para el grafico del arbol
)
#Crea un vector logico con si estan instalados o no
instalados <- paquetes %in% installed.packages()
#Si hay al menos uno no instalado los instala
if(sum(instalados == FALSE) > 0) {
  install.packages(paquetes[!instalados])
}
lapply(paquetes,require,character.only = TRUE)
```

1.2 - Cargamos los datos
Usamos fread de data.table para una lectura mucho mas rapida
```{r}
df <- fread('Telco.csv')
```

2 - Analisis exploratorio
2.1 - Analisis exploratorio general y tipo de datos
```{r, message=TRUE, warning=TRUE}
as.data.frame(sort(names(df)))
str(df)
glimpse(df)
```

#Contenido del Dataset:
Podemos observar un archivo de una empresa de telecomunicaciones, con 7.043 registros y 21 variables, entre las que se encuentran productos contratados, métodos de pago, permanencias, facturacion mensual, género...

Nuestra target será Churn, detecteremos la tasa de abandono.

Debemos cambiar de formato algnunas variables actualmente en formato "character" a "factor" 

Reservamos las variables a pasar a factor
```{r}
a_factor <- c('gender','Partner','Dependents', 'PhoneService','MultipleLines','InternetService', 'OnlineSecurity', 'OnlineBackup', 'DeviceProtection', 'TechSupport', 'StreamingTV', 'StreamingMovies', 'Contract', 'PaymentMethod', 'PaperlessBilling', 'Churn')

```
    
2.2 - Calidad de datos: Estadísticos básicos
Hacemos un summary, con lapply que sale en formato de lista y se lee mejor
```{r}
lapply(df,summary)
```

En un principio los estadisticos básicos se muestran normales, bien es cierto que aplican a pocas variables, porque la gran mayoría son actualmente tipo character.

Observamos también algunos aspectos como tenure (permanencia), que tiene un plazo máximo de 72 meses. También podemos ver los cargos mensuales, de media rondan los 65€.

2.3 - Calidad de datos: Análisis de nulos
```{r}
data.frame(colSums(is.na(df)))
```

Pocos datos nulos, exáctamente 11 registros en la variable TotalCharges. Sospechamos que pueden estar reconvertidos a ceros

2.4 - Calidad de datos: Análisis de ceros

No es algo que se haga siempre, pero en el analisis general superior habiamos detectado muchos ceros.
Vamos a constuir una funcion concreta para analizar esto

```{r}
contar_ceros <- function(variable) {
    temp <- transmute(df,if_else(variable==0,1,0))
    sum(temp)
}

num_ceros <- sapply(df,contar_ceros)
num_ceros <- data.frame(VARIABLE=names(num_ceros),CEROS = as.numeric(num_ceros),stringsAsFactors = F) #el as.numeric es para sacar solo el valor de num_ceros, sin el nombre
num_ceros <- num_ceros %>%
  arrange(desc(CEROS)) %>%
  mutate(PORCENTAJE = CEROS / nrow(df) * 100)
num_ceros
```

Encontramos que el mayor número de ceros es el de SeniorCitizen, lo que puede parecer normal, siendo una operadora con múltiples servicios enfocada a perfiles más jóvenes. En Tenure también has algunos registros, 11 en total, pero pueden ser contrataciones nuevas y es un número poco significativo. 


2.5 - Calidad de datos: Análisis de atípicos

2.5.1 - Analizamos las que son de tipo numerico

```{r}
out <- function(variable){
  t(t(head(sort(variable,decreasing = T),20))) #la doble traspuesta es un truco para que se visualice la salida, si no lo que crearia es una coleccion de dataframes que no se ven bien
}
lapply(df,function(x){
  if(is.double(x)) out(x)
})
```

2.5.2 - Analizamos las que son de tipo integer

```{r}
out <- function(variable){
  t(t(table(variable))) #la doble traspuesta es un truco para que se visualice la salida, si no lo que crearia es una coleccion de dataframes que no se ven bien
}
lapply(df,function(x){
  if(is.integer(x)) out(x)
})
```

Aqui observamos un número significativo de clientes cuya tenure es de 1 mes, esto puede ser motivo de posible abandono.

2.6 - Analisis longitudinal

```{r}
longi <- df %>% 
  summarise_all(mean) %>% #calcular la media de cada variable
  t() %>% #transponerlo para tenerlo en una sola columna y leerlo mejor
  as.data.frame() #reconvertirlo a dataframe porque t() lo pasa a matriz
data.frame(variable = rownames(longi), media = longi$V1) %>% #crear un nuevo dataframe para poder ordenar por el nombre
  arrange(desc(variable)) #ordenar por el nombre para tener la vision longitudinal
```

Conclusiones: Todos los datos son aparentemente normales.

2.7 -  Acciones resultado del analisis de calidad de datos y exploratorio

Vamos a hacer lo siguiente:
- Convertir a factor las variables almacenadas en a_factor
- Eliminamos los valores nulos
- Eliminamos la variable ID, no es relevante

```{r}
df <-df %>%
  mutate_at(a_factor,as.factor)%>%
  na.omit(df)%>%
  select(-customerID)
```

2.8 - Graficos
```{r}
ggplot(data = df, aes(x = tenure)) + geom_bar() + 
  xlab("Permanencia") + 
  ylab("Número de Usuarios") + 
  ggtitle("Histograma de permanencia ")
```

```{r}
ggplot(data = df, aes(x = MonthlyCharges)) + geom_histogram(binwidth = 5) + 
  xlab("Factura mensual") + 
  ylab("Número de Usuarios") + 
  ggtitle("Histograma de Cargos mensuales ")
```

```{r}
ggplot(data = df, aes(x = TotalCharges)) + geom_histogram(binwidth = 75) + 
  xlab("Cargos totales") + 
  ylab("Número de Usuarios") + 
  ggtitle("Histograma de Cargos totales ")
```


3 - Transformación de datos

3.1 - Creación de la target
```{r}
df <- df %>% 
  mutate(TARGET = as.numeric(ifelse((Churn == "Yes"),1,0))) %>% #el as.numeric es para que los niveles del factor se queden como 0 y 1, y no como 1 y 2
  select(-Churn) #eliminamos la original para que no confunda
```

3.2 - Preparacion de las variables independientes

3.2.1 - Preseleccion de variables independientes
Creamos una lista con todas las variables independientes. Pero para ver si predicen solo necesitamos un mes de las que son historicas (en nuestro caso tenemos datos de sólo 1 mes, así que entran todas menos la target)

```{r}
independientes <- setdiff(names(df), "TARGET")
```

Creamos una muestra m menor para que los calculos sean mas rapidos
```{r}
set.seed(12345)
m <- sample_n(df,500)
```

3.2.1.1 - Preseleccion con RandomForest
```{r}
pre_rf <- randomForest(formula = reformulate(independientes,'TARGET'), data= m,mtry=2,ntree=50, importance = T)
imp_rf <- importance(pre_rf)[,2] #como importance devuelve una matriz con varias metricas, tenemos que extraer asi el decrecimiento en Gini que es el que mas nos interesa
imp_rf <- data.frame(VARIABLE = names(imp_rf), IMP_RF = imp_rf) #lo transformamos a dataframe
imp_rf <- imp_rf %>% arrange(desc(IMP_RF)) %>% mutate(RANKING_RF = 1:nrow(imp_rf)) #creamos el ranking
```

visualizamos
```{r}
imp_rf
```

3.2.1.2 - Preseleccion con Information Value
```{r}
m2 <- mutate(m,TARGET = as.numeric(as.character(TARGET))) #transformo la target a numerico temporalmente porque este algoritmo necesita que este en numerico, y el as.character es para que lo convierta a 0 y 1, y no a 1 y 2
imp_iv <- smbinning.sumiv(m2[c(independientes,'TARGET')],y="TARGET")
imp_iv <- imp_iv %>% mutate(Ranking = 1:nrow(imp_iv)) %>% select(-Process)
names(imp_iv) <- c('VARIABLE','IMP_IV','RANKING_IV')
```

visualizamos
```{r}
imp_iv
```

3.2.1.3 - Preseleccion final
```{r}
imp_final <- inner_join(imp_rf,imp_iv,by='VARIABLE') %>% 
  select(VARIABLE,IMP_RF,IMP_IV,RANKING_RF,RANKING_IV) %>% #ponerlos en orden mas legible
  mutate(RANKING_TOT = RANKING_RF + RANKING_IV) %>% 
  arrange(RANKING_TOT)
imp_final
```

¿Son los metodos fiables? Vamos a hacer una correlacion entre ellos a ver si dan cosas similares
```{r}
cor(imp_final$IMP_RF,imp_final$IMP_IV,use = 'complete.obs')
```
 los resultados de la correlacion son... 0,4. no me convencen

Decision: vamos a descartar aquellas variables que no hayan salido entre las 10 mas importantes en ninguno de los dos sistemas de seleccion de variables

```{r}
ind_corta <- imp_final %>%
  filter(RANKING_RF <= 10 | RANKING_IV <= 10) %>% 
  select(VARIABLE) #nos quedamos solo con el nombre
ind_corta <- as.character(ind_corta$VARIABLE) #lo pasamos a un vector en vez de un dataframe
```

Estas son las variables predictoras con las que vamos a trabajar finalmente
```{r}
ind_corta
```

4. - Variables Sinteticas
Creación de variables a través de otras para aumentar la capacidad de predicción del modelo.

4.1 - Creacion de variables sinteticas

4.1.1 - Productos contratados
Crearemos una variable que cuantifique los servicios contratados

Primero extraemos un valor lógico si el servicio está contratado o no:

```{r}
telefono <- as.numeric(if_else(df$PhoneService == "Yes", 1,0))
multiples <- as.numeric(if_else(df$MultipleLines == "Yes", 1,0))
internet <- as.numeric(if_else(df$InternetService == "No", 0,1))
seguridad <- as.numeric(if_else(df$OnlineSecurity == "Yes", 1,0))
backup <- as.numeric(if_else(df$OnlineBackup == "Yes", 1,0))
proteccion <- as.numeric(if_else(df$DeviceProtection == "Yes", 1,0))
soporte <- as.numeric(if_else(df$TechSupport == "Yes", 1,0))
tv <- as.numeric(if_else(df$StreamingTV == "Yes", 1,0))
pelis <- as.numeric(if_else(df$StreamingMovies == "Yes", 1,0))
```

Hacemos sumatoria y almacenamos en servicios contratados
```{r}
df <-df %>%
  mutate (serv_contratados = (
    telefono + multiples + internet + 
    seguridad + backup + proteccion + 
      soporte + tv + pelis))
```

Eliminamos variables creadas

```{r}
rm('telefono', 'multiples','internet', 'seguridad', 'backup' , 'proteccion' , 'soporte' , 'tv' , 'pelis')
```

4.1.2 - Finalización permanencia

Nos interesa saber si la vinculación con la empresa está a punto de expirar, por lo que estableceremos una variable lógica que nos indique si su tenure es de 1 mes.

```{r}
df<- df%>%
  mutate(permanencia = ifelse(tenure == 1, 1, 0))
```


4.1.3 - Consumos altos

Igualmente crearemos una variable lógica con indicador de consumos altos mensuales, para ello establecemos como alto consumo a los cargos superiores al 3er cuartil.

```{r}
cuartil <- quantile (df$MonthlyCharges, prob = c(0.75))

df <- df%>%
  mutate(consumo = ifelse(df$MonthlyCharges >= cuartil, 1, 0))
```

Guardamos  cache temporal
```{r}
saveRDS(df,'cacheT1.rds')
```

Cargamos el cache temporal
```{r}
df <- readRDS('cacheT1.rds')
```

4.2 - Discretizacion
Primero vamos a crear la funcion que va a discretizar de forma automatica maximizando la capacidad predictiva de la nueva variable
Ademas, como vamos a usar en la modelizacion un algoritmo lineal, que es la regresion logistica, vamos a intentar que la discretizacion sea monotonica
```{r}
discretizar <- function(vi,target){
  temp_df <- data.frame(vi = vi, target = target)
  #smbinning necesita que la target sea numerica
  temp_df$target <- as.numeric(as.character(temp_df$target))
  disc <- smbinning(temp_df, y = 'target', x = 'vi')
  return(disc)
}
```

4.2.1 - Discretizamos tenure

```{r}
disc_temp_tenure <- discretizar(df$tenure,df$TARGET)
df_temp <- select(df,tenure,TARGET) #creamos este temporal porque smbinning.gen necesita que el df tenga el mismo numero de columnas que la salida de la funcion discretizar
df_temp <- smbinning.gen(df_temp,disc_temp_tenure,chrname = 'tenure_DISC')
#Metemos en df la nueva variable discretizada y eliminamos la original
df <- cbind(df,df_temp[3]) %>% select(-tenure)
```


4.2.2 - Discretizamos MonthlyCharges
```{r}
disc_temp_MonthlyCharges <- discretizar(df$MonthlyCharges,df$TARGET)
df_temp <- select(df,MonthlyCharges,TARGET) #creamos este temporal porque smbinning.gen necesita que el df tenga el mismo numero de columnas que la salida de la funcion discretizar
df_temp <- smbinning.gen(df_temp,disc_temp_MonthlyCharges,chrname = 'MonthlyCharges_DISC')
#Metemos en df la nueva variable discretizada y eliminamos la original
df <- cbind(df,df_temp[3]) %>% select(-MonthlyCharges)
```


4.2.3 - Discretizamos TotalCharges
```{r}
disc_temp_TotalCharges <- discretizar(df$TotalCharges,df$TARGET)
df_temp <- select(df,TotalCharges,TARGET) #creamos este temporal porque smbinning.gen necesita que el df tenga el mismo numero de columnas que la salida de la funcion discretizar
df_temp <- smbinning.gen(df_temp,disc_temp_TotalCharges,chrname = 'TotalCharges_DISC')
#Metemos en df la nueva variable discretizada y eliminamos la original
df <- cbind(df,df_temp[3]) %>% select(-TotalCharges)
```


Vamos a hacer una inspeccion visual de todas las variables a ver si han salido bien
```{r}
df %>% 
  select_if(is.factor) %>% 
  gather() %>% 
  ggplot(aes(value)) +
    geom_bar() +
    facet_wrap(~ key, scales = "free") +
    theme(axis.text=element_text(size=4))#esto es para cambiar el tamaño del texto del eje y que se lea bien
```

Ahora vamos a analizar la penetración de la target en cada categoría para ver si las variables han salido monotónicas
```{r}
a <- function(var1,var2) {
  df_temp <- data.frame(var1 = df[[var1]],var2 = df[[var2]])
  df_temp %>% 
    group_by(var1) %>% 
    summarise(Conteo = n(), Porc = mean(as.numeric(as.character(var2)))) %>% 
  ggplot(aes(var1,Porc)) + geom_bar(stat='identity') + xlab(var1)
}
df2_nombres <- df %>% select_if(is.factor) %>% names()
lapply(df2_nombres,function(x){a(x,'TARGET')})
```

Antes de continuar vamos a guardar en un objeto de R las discretizaciones, porque las necesitaremos despues para poner el modelo en produccion
```{r}
#Vamos a crear un objeto de tipo lista que es lo ideal para guardar objetos complejos como las discretizaciones
discretizaciones <- list(
 disc_temp_tenure = disc_temp_tenure,
 disc_temp_MonthlyCharges = disc_temp_MonthlyCharges, 
 disc_temp_TotalCharges = disc_temp_TotalCharges
 )
saveRDS(discretizaciones,'01_CortesDiscretizaciones.rds')
```

4.3 - Ordenación y limpieza del dataframe

4.3.1 - Dejaremos el df con las variables elegidas como preferentes para la modelización, obteniéndolas de los procesos de selección de independientes (ind_corta), sinteticas y discretizadas.

Para hacerlo más facil identificamos las variables actuales en el df y eliminamos las que no coinciden con ind_corta, manteniendo las sinteticas y discretizadas.

```{r}
names(df)
ind_corta 
```

Aplicamos un método menos "técnico" al hacerlo de forma manual, pero es la forma más rápida y sencilla dadas las características del df.

```{r}
df <- df%>%
  select(-gender, -SeniorCitizen, -Partner, -Dependents, -PhoneService, -MultipleLines)
```

Ahora incluiremos un id al dataframe y ordenaremos los nombres de las variables

```{r}
df <- df %>% mutate(id = row_number())

centrales <- setdiff(names(df),c('id','TARGET'))
df <- df %>% select(
  id,
  one_of(centrales),
  TARGET)
```

4.3.2.  - Limpieza
Limpiamos el entorno de cualquier cosa que no sea el dataframe
```{r}
a_borrar <- setdiff(ls(),'df')
rm(list=c(a_borrar,'a_borrar'))
```

Guardamos otro cache temporal
```{r}
saveRDS(df,'cacheT2.rds')
```

Cargamos el cache temporal
```{r}
df <- readRDS('cacheT2.rds')
```

5. Modelizacion

5.1 - Preparar las funciones que vamos a necesitar

Funcion para crear una matriz de confusion
```{r}
confusion<-function(real,scoring,umbral){ 
  conf<-table(real,scoring>=umbral)
  if(ncol(conf)==2) return(conf) else return(NULL)
}
```

Funcion para calcular las metricas de los modelos: acierto, precision, cobertura y F1
```{r}
metricas<-function(matriz_conf){
  acierto <- (matriz_conf[1,1] + matriz_conf[2,2]) / sum(matriz_conf) *100
  precision <- matriz_conf[2,2] / (matriz_conf[2,2] + matriz_conf[1,2]) *100
  cobertura <- matriz_conf[2,2] / (matriz_conf[2,2] + matriz_conf[2,1]) *100
  F1 <- 2*precision*cobertura/(precision+cobertura)
  salida<-c(acierto,precision,cobertura,F1)
  return(salida)
}
```

Funcion para probar distintos umbrales y ver el efecto sobre precision y cobertura

```{r}
umbrales<-function(real,scoring){
  umbrales<-data.frame(umbral=rep(0,times=19),acierto=rep(0,times=19),precision=rep(0,times=19),cobertura=rep(0,times=19),F1=rep(0,times=19))
  cont <- 1
  for (cada in seq(0.05,0.95,by = 0.05)){
    datos<-metricas(confusion(real,scoring,cada))
    registro<-c(cada,datos)
    umbrales[cont,]<-registro
    cont <- cont + 1
  }
  return(umbrales)
}
```


Funciones que calculan la curva ROC y el AUC
```{r}
roc<-function(prediction){
  r<-performance(prediction,'tpr','fpr')
  plot(r)
}

auc<-function(prediction){
  a<-performance(prediction,'auc')
  return(a@y.values[[1]])
}
```

5.2 - Creamos las particiones de training (70%) y test (30%)

Establecemos una semilla para que nos salgan los mismos resultados
```{r}
set.seed(12345)
```

Generamos una variable aleatoria con una distribucion 70-30
```{r}
df$random<-sample(0:1,size = nrow(df),replace = T,prob = c(0.3,0.7)) 
```

Creamos los dos dataframes
```{r}
train<-filter(df,random==1)
test<-filter(df,random==0)
#Eliminamos ya la random para que no moleste
df$random <- NULL
```

5.3 - Creación del modelo de propensión
Nota: Vamos a probar tres algoritmos diferentes para ver cual funciona mejor y aprender como se comparan

5.3.1 - Identificamos las variables
```{r}
#Las independientes seran todas menos el codigo cliente y la target
independientes <- setdiff(names(df),c('id','TARGET'))
target <- 'TARGET'
```

5.3.2 - Creamos la formula para usar en el modelo
```{r}
formula <- reformulate(independientes,target)
```

5.3.3 - Modelizamos con regresion logistica

Primero vamos a hacer un modelo con todas las variables
```{r}
formula_rl <- formula
rl<- glm(formula_rl,train,family=binomial(link='logit'))
summary(rl)
```

Revisamos la significatividad y mantenemos todas las variables que tengan tres estrellas en alguna categoria, menos SALDO1ER_PASIVO_TEND porque ya estan entrando otras dos de la misma variable origen
```{r}
a_mantener <- c(
  'InternetService',
  'OnlineSecurity',
  'TechSupport',
  'Contract',
  'PaperlessBilling',
  'tenure_DISC',
  'permanencia'
)
```

Volvemos a modelizar
```{r}
formula_rl <- reformulate(a_mantener,target)
rl<- glm(formula_rl,train,family=binomial(link='logit'))
summary(rl)
```

Vemos que ahora ya todas las variables tienen al menos una categoria con 3 estrellas de significacion.
Comprobaremos este modelo sobre el conjunto de test:

Y calculamos el pseudo R cuadrado:
```{r}
pr2_rl <- 1 -(rl$deviance / rl$null.deviance)
pr2_rl
```

Aplicamos el modelo al conjunto de test, generando un vector con las probabilidades
```{r}
rl_predict<-predict(rl,test,type = 'response')
```

Vemos que pinta tiene
```{r}
plot(rl_predict~test$TARGET)
```

Ahora tenemos que transformar la probabilidad en una decision de si el cliente va a comprar o no

Con la funcion umbrales probamos diferentes cortes
```{r}
umb_rl<-umbrales(test$TARGET,rl_predict)
umb_rl
```

Seleccionamos el umbral que maximiza la F1
```{r}
umbral_final_rl<-umb_rl[which.max(umb_rl$F1),1]
umbral_final_rl
```

Evaluamos la matriz de confusion y las metricas con el umbral optimizado
```{r}
confusion(test$TARGET,rl_predict,umbral_final_rl)
rl_metricas<-filter(umb_rl,umbral==umbral_final_rl)
rl_metricas
```

Evaluamos la ROC
```{r}
#creamos el objeto prediction
rl_prediction<-prediction(rl_predict,test$TARGET)
#visualizamos la ROC
roc(rl_prediction)
```

Sacamos las metricas definitivas incluyendo el AUC
```{r}
rl_metricas<-cbind(rl_metricas,AUC=round(auc(rl_prediction),2)*100)
print(t(rl_metricas))
```


5.3.4 - Modelizamos con Arboles de decision

Creamos el primer modelo
```{r}
formula_ar <- formula
ar<-rpart(formula_ar, train, method = 'class', parms = list(
  split = "information"), 
  control = rpart.control(cp = 0.00001))
```

Revisamos donde el error de validacion cruzada empieza a crecer
```{r}
printcp(ar)
plotcp(ar)
```

Parece que minimiza aprox en cp = 0.0016 de complejidad
Generamos un nuevo arbol con ese parametro
Ademas vamos a incluir un nuevo parametro para que el arbol no tenga mas de 7 niveles
```{r}
ar<-rpart(formula, train, method = 'class', parms = list(
  split = "information"), 
  control = rpart.control(cp = 0.0016,maxdepth = 7))
```

Revisamos de nuevo la complejidad
```{r}
printcp(ar)
plotcp(ar)
```

Conseguimos con estos parámetros que el error cruzado no llegue a subir, así que seleccionamos este árbol como definitivo.

Vamos a crear el grafico del arbol para analizarlo

```{r}
rpart.plot(ar,type=2,extra = 7, under = TRUE,under.cex = 0.7,fallen.leaves=F,gap = 0,cex=0.2,yesno = 2,box.palette = "GnYlRd",branch.lty = 3)
```


Vamos a sacar las reglas que podrian ser utilizadas por ejemplo para hacer una implantacion del arbol
```{r}
rpart.rules(ar,style = 'tall',cover = T)
#style sirve para que la salida sea mas legible y cover añade el % de casos e los que aplica la regla
```

Podemos llevarnos el nodo final de cada cliente a un data.frame para poder hacer una explotacion posterior
```{r}
#Para ello usarmos el predict especficio de rpart y con el parametro nn
ar_numnodos<-rpart.predict(ar,test,nn = T)
head(ar_numnodos)
```

Vamos a calcular los scorings y evaluar el modelo
```{r}
ar_predict<-predict(ar,test,type = 'prob')[,2]
```

Vemos que pinta tiene
```{r}
plot(ar_predict~test$TARGET)
```

```{r}
ggplot(test, aes(test$TARGET,ar_predict))+
  geom_boxplot()
```


Con la funcion umbrales probamos diferentes cortes
```{r}
umb_ar<-umbrales(test$TARGET,ar_predict)
umb_ar
```

Seleccionamos automaticamente el mejor umbral
```{r}
umbral_final_ar<-umb_ar[which.max(umb_ar$F1),1]
umbral_final_ar
```

Evaluamos la matriz de confusion y las metricas con el umbral optimizado
```{r}
confusion(test$TARGET,ar_predict,umbral_final_ar)
ar_metricas<-filter(umb_ar,umbral==umbral_final_ar)
ar_metricas
```

Evaluamos la ROC
```{r}
#creamos el objeto prediction
ar_prediction<-prediction(ar_predict,test$TARGET)
#visualizamos la ROC
roc(ar_prediction)
```

Sacamos las metricas definitivas incluyendo el AUC
```{r}
ar_metricas<-cbind(ar_metricas,AUC=round(auc(ar_prediction),2)*100)
print(t(ar_metricas))
```

5.4 - Comparamos los 2 metodos
```{r}
comparativa <- rbind(rl_metricas,ar_metricas)
rownames(comparativa) <- c('Regresion Logistica','Arbol Decision')
t(comparativa) #t simplemente transpone para leerlo mejor
```
Conclusion: todos serían igualmente predictivos, entonces por un criterio de parsimonia vamos a quedarnos con la regresion logistica

5.4.1 - Escribimos el scoring final en el dataset y guardamos el modelo
```{r}
df$SCORING_ABANDONO <- predict(rl,df,type = 'response')
saveRDS(rl,'modelo_final_telco.rds')
```


Cargamos el cache temporal del modelo
```{r}
rl <- readRDS('modelo_final_telco.rds')
```


6. Evaluacion y analisis de negocio

Vamos a visualizar la tasa de abandono por tramos de scoring. Este grafico es muy potente para ver que el modelo es consistente, ya que debe presentar una linea descente en la tasa de contratacion conforme se desciende en el scoring
```{r}
#Creamos una funcion para visualizar el abandono real por percentiles de scoring
vis <- function(scoring,real) {
    #Preparar el dataframe de visualización
    vis_df <- data.frame(Scoring = scoring, Perc_Scoring = cut_number(scoring, 20), Real = real)
    levels(vis_df$Perc_Scoring) <- seq(from = 100,to = 5,by = -5)
    vis_gr <- vis_df %>% group_by(Perc_Scoring) %>% summarise(Tasa_Churn = mean(as.numeric(as.character(Real)))) %>% arrange(Perc_Scoring)
    #ordenar el factor para el gráfico
    vis_gr$Perc_Scoring <- factor(vis_gr$Perc_Scoring, levels = vis_gr$Perc_Scoring[order(vis_gr$Perc_Scoring, decreasing = T)])
    #hacemos el gráfico
    ggplot(vis_gr,aes(Perc_Scoring, Tasa_Churn)) + 
      geom_col(fill='grey') + 
      geom_hline(aes(yintercept =      mean(as.numeric(as.character(vis_df$Real)))), col = 'black') +
      labs(title = 'Abandono real por tramo de scoring', x = 'Tramo de scoring', y = 'Abandono real')
}
vis(df$SCORING_ABANDONO,df$TARGET)
```

6.1 - Simulación de acción comercial para evitar la tasa de abandono.

El tamaño de campaña viene definido por un criterio de negocio como por ejemplo el presupuesto total asignado a la campaña
```{r}
#Supongamos que tenemos un presupuesto de 20.000€ para retener al cliente
#Y que la campaña se realizara por call center, con un coste unitario de 20€ por cliente contactado
#Entonces el numero de clientes a contactar sera de 20.000 / 20 = 1.000
#Para extraerlos simplemente cogemos los 1.000 primeros ordenados por scoring
tamaño_campaña <- 2000
bote_campaña <- df %>% 
  arrange(desc(SCORING_ABANDONO)) %>% 
  slice(1:tamaño_campaña) %>%
  select(id,SCORING_ABANDONO)
#Previsualizamos la salida
head(bote_campaña,50)
#Vamos a ver gráficamente si de esta forma estamos aprovechando el potencial de nuestro modelo
penetracion_target <- mean(as.numeric(as.character(df$TARGET)))
df %>% 
  arrange(desc(SCORING_ABANDONO)) %>% 
  ggplot(aes(y = SCORING_ABANDONO, x = seq_along(SCORING_ABANDONO))) +
  geom_line() + 
  geom_vline(xintercept = tamaño_campaña, col = 'orange') +
  geom_hline(yintercept = penetracion_target,col='blue') +
  labs(x = 'CLIENTES ORDENADOS POR SCORING', y = 'SCORING')
```


Según la visualización en la gráfica, podemos comprobar que con una inversion de 20.000€ más la fidelización acordada por la empresa, podríamos retener a la gran mayoría de abandonos potenciales.