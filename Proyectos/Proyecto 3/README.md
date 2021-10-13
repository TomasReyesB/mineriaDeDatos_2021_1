Proyecto N°3
================
Tomás Reyes
Juan Lisboa
# Análisis de deportes de resistencia

En el presente proyecto se busca clasificar las actividades hechas por
deportistas de resistencia, que estos rastrean utilizando dispositivos
digitales. Es importante clasificar de buena manera estas actividades ya
que se mantienen sistemas de ranking y premios que se pueden ver
gravemente afectados por actividades mal ingresadas por los usuarios, ya
sea de manera ccidental o intencionada, como por ejemplo registrar una
actividad de bicicleta como de running para aumentar la distancia
recorrida en el ranking o la velocidad promedio del usuario.

El objetivo principal de este proyecto es diferenciar las actividades
realizadas en bicicleta o a pie de acuerdo a características como la
distancia recorrida, la velocidad promedio, entre otros atributos
capturados por los dispositivos de rastreo como celulares u otros
wearables. Como objetivo secundario se plantea identificar las
actividades que fueron registradas errónameante por los usuarios.

## Cargar datos

Los datos a analizar constan de 167615 actividades con 17 variables cada
una, los que serán almacenados en un data frame llamado
**actividadesRaw**. Luego se muestra un resumen del dataset y sus
variables con la función **summary()**

``` r
actividadesRaw <- readRDS("endurance.rds")
actividadesRaw %>% summary()
```

    ##        id             type              athlete            calories       
    ##  Min.   :     1   Length:167615      Min.   : 8558143   Min.   :     0.0  
    ##  1st Qu.: 41905   Class :character   1st Qu.:14204866   1st Qu.:   281.0  
    ##  Median : 83808   Mode  :character   Median :15621719   Median :   629.0  
    ##  Mean   : 83808                      Mean   :15359426   Mean   :   816.7  
    ##  3rd Qu.:125712                      3rd Qu.:16685446   3rd Qu.:  1076.8  
    ##  Max.   :167615                      Max.   :18078075   Max.   :326157.3  
    ##     distance         elev_low           records           elev_high        
    ##  Min.   :      0   Length:167615      Length:167615      Length:167615     
    ##  1st Qu.:   6159   Class :character   Class :character   Class :character  
    ##  Median :  12968   Mode  :character   Mode  :character   Mode  :character  
    ##  Mean   :  21998                                                           
    ##  3rd Qu.:  27494                                                           
    ##  Max.   :1479320                                                           
    ##   max_speed         device_name         moving_time        elapsed_time      
    ##  Length:167615      Length:167615      Min.   :       0   Min.   :        0  
    ##  Class :character   Class :character   1st Qu.:    2248   1st Qu.:     2582  
    ##  Mode  :character   Mode  :character   Median :    3853   Median :     4450  
    ##                                        Mean   :    5141   Mean   :    32102  
    ##                                        3rd Qu.:    6516   3rd Qu.:     7934  
    ##                                        Max.   :11025474   Max.   :511111044  
    ##  average_speed      has_heartrate      start_date_local             
    ##  Length:167615      Length:167615      Min.   :1999-04-25 17:36:38  
    ##  Class :character   Class :character   1st Qu.:2016-11-23 17:11:10  
    ##  Mode  :character   Mode  :character   Median :2019-01-26 13:37:56  
    ##                                        Mean   :2018-05-12 07:50:32  
    ##                                        3rd Qu.:2020-04-30 04:33:10  
    ##                                        Max.   :2021-02-02 11:37:09  
    ##  total_elevation_gain
    ##  Min.   :    0.0     
    ##  1st Qu.:   15.1     
    ##  Median :  171.0     
    ##  Mean   :  397.0     
    ##  3rd Qu.:  595.0     
    ##  Max.   :59595.0

## Limpieza de datos

Antes de comenzar con el análisis de los datos y creación de modelos de
clasificación de actividades, primero es necesario realizar una limpieza
de estos, así como también verificar su integridad, esto es, que no
hayan datos faltantes y escalarlos en la medidia de lo posible para
evitar bias en los cálculos posteriores.

Primero se obtendrán todos los tipos de actividades que hay en el
dataset, para lo cual se utilizará la función **unique()**. Luego se
eliminarán las columnas **id** y **athlete** ya que corresponden a
identificadores de actividad y usuario, respectivamente. Además también
se eliminarán las variables **device\_name**, **start\_date\_local**,
**records** y **has\_heartrate**. Sin embargo, para no modificar el
dataset original es que se hará una copia de este, llamada
**actividades**. Además, se crea una segunda copia de este dataset, que
leugo se tuilizará para determinar aquellas que fueron erróneamente
catalogadas.

``` r
actividades <- actividadesRaw
actividades2 <- actividadesRaw

actividades$type %>% unique()
```

    ## [1] "Ride"      "Run"       "Walk"      "Hike"      "EBikeRide"

``` r
actividades$id <- NULL
actividades$athlete <- NULL
actividades$device_name <- NULL
actividades$start_date_local <- NULL
actividades$records <- NULL
actividades$has_heartrate <- NULL

actividades2$athlete <- NULL
actividades2$device_name <- NULL
actividades2$start_date_local <- NULL
actividades2$records <- NULL
actividades2$has_heartrate <- NULL
```

Ahora corresponde cambiar las variables que son de tipo character a
numéricas, como las referentes a la elevación, velocidad máxima y
velocidad promedio. También se categorizarán las actividades de acuerdo
a como fueron realizadas, convirtiendo la variable **type** a un factor
que será 1 para las acividades en bicicleta y 0 para las actividades a
pie.

``` r
actividades$elev_low <- as.numeric(actividades$elev_low)
actividades$elev_high <- as.numeric(actividades$elev_high)
actividades$max_speed <- as.numeric(actividades$max_speed)
actividades$average_speed <- as.numeric(actividades$average_speed)

actividades2$elev_low <- as.numeric(actividades$elev_low)
actividades2$elev_high <- as.numeric(actividades$elev_high)
actividades2$max_speed <- as.numeric(actividades$max_speed)
actividades2$average_speed <- as.numeric(actividades$average_speed)

actividades$typeCode <- (actividades$type == "Ride" | actividades$type == "EBikeRide") %>% as.numeric()
actividades2$typeCode <- (actividades$type == "Ride" | actividades$type == "EBikeRide") %>% as.numeric()

actividades$type <- NULL
actividades2$type <- NULL
```

Luego, se procederá a la detección de datos faltantes o NAs en cada una
de las variables del dataset.

``` r
columnNames <- c("calories", "distance", "elev_low", "elev_high", "max_speed", "moving_time", "elapsed_time", "average_speed", "total_elevation_gain", "typeCode")
vectorNA <- vector(mode = "list" , 10)

for (i in 1:ncol(actividades)) {
  
  vectorNA[i] <- sum(is.na(actividades[, i]))
    
}

datasetNA <- rbind.data.frame(columnNames, vectorNA)

datasetNA %>% str()
```

    ## 'data.frame':    2 obs. of  10 variables:
    ##  $ c..calories....0..            : chr  "calories" "0"
    ##  $ c..distance....0..            : chr  "distance" "0"
    ##  $ c..elev_low....13519..        : chr  "elev_low" "13519"
    ##  $ c..elev_high....13519..       : chr  "elev_high" "13519"
    ##  $ c..max_speed....0..           : chr  "max_speed" "0"
    ##  $ c..moving_time....0..         : chr  "moving_time" "0"
    ##  $ c..elapsed_time....0..        : chr  "elapsed_time" "0"
    ##  $ c..average_speed....0..       : chr  "average_speed" "0"
    ##  $ c..total_elevation_gain....0..: chr  "total_elevation_gain" "0"
    ##  $ c..typeCode....0..            : chr  "typeCode" "0"

Como se puede ver, todos los valores NA encontrados pertenecen a las
variables referentes a la altura, por lo que se analizarán estas
actividades para determinar alguna característica común y tomar acciones
a partir de estas.

``` r
actividadesNA <-actividades %>% filter(is.na(actividades$elev_high))

actividadesNA %>% summary()
```

    ##     calories         distance         elev_low       elev_high    
    ##  Min.   :   0.0   Min.   :     0   Min.   : NA     Min.   : NA    
    ##  1st Qu.: 163.0   1st Qu.:     0   1st Qu.: NA     1st Qu.: NA    
    ##  Median : 384.0   Median :  5006   Median : NA     Median : NA    
    ##  Mean   : 410.6   Mean   : 10085   Mean   :NaN     Mean   :NaN    
    ##  3rd Qu.: 582.0   3rd Qu.: 16958   3rd Qu.: NA     3rd Qu.: NA    
    ##  Max.   :6498.0   Max.   :243611   Max.   : NA     Max.   : NA    
    ##                                    NA's   :13519   NA's   :13519  
    ##    max_speed       moving_time      elapsed_time    average_speed   
    ##  Min.   : 0.000   Min.   :     0   Min.   :     0   Min.   : 0.000  
    ##  1st Qu.: 0.000   1st Qu.:  1897   1st Qu.:  1922   1st Qu.: 0.000  
    ##  Median : 3.600   Median :  2987   Median :  3001   Median : 2.687  
    ##  Mean   : 4.793   Mean   :  3248   Mean   :  3388   Mean   : 3.206  
    ##  3rd Qu.: 8.400   3rd Qu.:  3719   3rd Qu.:  3731   3rd Qu.: 5.867  
    ##  Max.   :78.500   Max.   :608912   Max.   :715053   Max.   :57.880  
    ##                                                                     
    ##  total_elevation_gain    typeCode     
    ##  Min.   : 0.000000    Min.   :0.0000  
    ##  1st Qu.: 0.000000    1st Qu.:0.0000  
    ##  Median : 0.000000    Median :1.0000  
    ##  Mean   : 0.001405    Mean   :0.6869  
    ##  3rd Qu.: 0.000000    3rd Qu.:1.0000  
    ##  Max.   :14.000000    Max.   :1.0000  
    ## 

Se eliminarán todas las actividades con **distance** igual a 0, ya que
no aportan valor al análisis.

``` r
actividadesNA[actividadesNA == 0] <- NA

actividadesNA %>% summary()
```

    ##     calories         distance           elev_low       elev_high    
    ##  Min.   :   0.1   Min.   :     0.7   Min.   : NA     Min.   : NA    
    ##  1st Qu.: 272.0   1st Qu.:  5076.9   1st Qu.: NA     1st Qu.: NA    
    ##  Median : 435.0   Median : 11557.4   Median : NA     Median : NA    
    ##  Mean   : 476.5   Mean   : 15410.4   Mean   :NaN     Mean   :NaN    
    ##  3rd Qu.: 621.0   3rd Qu.: 22580.0   3rd Qu.: NA     3rd Qu.: NA    
    ##  Max.   :6498.0   Max.   :243611.0   Max.   : NA     Max.   : NA    
    ##  NA's   :1868     NA's   :4672       NA's   :13519   NA's   :13519  
    ##    max_speed       moving_time      elapsed_time    average_speed   
    ##  Min.   : 0.100   Min.   :     1   Min.   :     1   Min.   : 0.001  
    ##  1st Qu.: 3.600   1st Qu.:  1910   1st Qu.:  1931   1st Qu.: 2.748  
    ##  Median : 6.800   Median :  2996   Median :  3002   Median : 4.479  
    ##  Mean   : 7.326   Mean   :  3258   Mean   :  3399   Mean   : 4.901  
    ##  3rd Qu.:10.000   3rd Qu.:  3721   3rd Qu.:  3732   3rd Qu.: 6.946  
    ##  Max.   :78.500   Max.   :608912   Max.   :715053   Max.   :57.880  
    ##  NA's   :4674     NA's   :42       NA's   :41       NA's   :4675    
    ##  total_elevation_gain    typeCode   
    ##  Min.   : 1.000       Min.   :1     
    ##  1st Qu.: 2.500       1st Qu.:1     
    ##  Median : 4.000       Median :1     
    ##  Mean   : 6.333       Mean   :1     
    ##  3rd Qu.: 9.000       3rd Qu.:1     
    ##  Max.   :14.000       Max.   :1     
    ##  NA's   :13516        NA's   :4233

Una vez que todos las entradas con valores igual a 0 se han convertido
en NAs, se eliminarán para así obtener las actividades que si aportan
información pero tienen NA en la variable **elev\_gain**.

``` r
actividadesNA$elev_low <- NULL
actividadesNA$elev_high <- NULL

actividadesNA <- na.omit(actividadesNA)

actividadesNA %>% str()
```

    ## 'data.frame':    3 obs. of  8 variables:
    ##  $ calories            : num  48 193 678
    ##  $ distance            : num  1349 5370 17098
    ##  $ max_speed           : num  10.5 10.1 10
    ##  $ moving_time         : num  148 616 2493
    ##  $ elapsed_time        : num  148 616 2493
    ##  $ average_speed       : num  9.12 8.72 6.86
    ##  $ total_elevation_gain: num  1 4 14
    ##  $ typeCode            : num  1 1 1
    ##  - attr(*, "na.action")= 'omit' Named int [1:13516] 1 2 3 4 5 6 7 8 9 10 ...
    ##   ..- attr(*, "names")= chr [1:13516] "1" "2" "3" "4" ...

Al eliminar las activiades con datos igual a 0 solo quedan 3 entradas
que si tienen sus datos completos a excepción de **elev\_high** y
**elev\_low**, por lo que eliminarlas no implica una gran impacto en el
análisis del dataset original. Por esto es que se decide eliminar todas
las actividades con datos faltantes del dataframe **actividades**.

``` r
actividades <- na.omit(actividades)
actividades2 <- na.omit(actividades2)

actividades %>% summary()
```

    ##     calories           distance          elev_low         elev_high      
    ##  Min.   :     0.0   Min.   :      0   Min.   :-3257.1   Min.   : -500.0  
    ##  1st Qu.:   297.0   1st Qu.:   6890   1st Qu.:  222.8   1st Qu.:  476.8  
    ##  Median :   665.0   Median :  13534   Median :  595.8   Median :  728.8  
    ##  Mean   :   852.3   Mean   :  23043   Mean   :  557.4   Mean   :  848.2  
    ##  3rd Qu.:  1128.0   3rd Qu.:  28813   3rd Qu.:  774.0   3rd Qu.: 1141.4  
    ##  Max.   :326157.3   Max.   :1479320   Max.   :11302.4   Max.   :12606.8  
    ##    max_speed       moving_time        elapsed_time       average_speed     
    ##  Min.   :  0.00   Min.   :       0   Min.   :        0   Min.   :   0.000  
    ##  1st Qu.:  5.10   1st Qu.:    2293   1st Qu.:     2694   1st Qu.:   2.680  
    ##  Median : 10.30   Median :    4032   Median :     4764   Median :   3.593  
    ##  Mean   : 10.12   Mean   :    5307   Mean   :    34621   Mean   :   4.228  
    ##  3rd Qu.: 14.00   3rd Qu.:    6814   3rd Qu.:     8337   3rd Qu.:   5.739  
    ##  Max.   :244.60   Max.   :11025474   Max.   :511111044   Max.   :2296.088  
    ##  total_elevation_gain    typeCode     
    ##  Min.   :    0.0      Min.   :0.0000  
    ##  1st Qu.:   34.1      1st Qu.:0.0000  
    ##  Median :  236.3      Median :1.0000  
    ##  Mean   :  431.8      Mean   :0.6544  
    ##  3rd Qu.:  635.0      3rd Qu.:1.0000  
    ##  Max.   :59595.0      Max.   :1.0000

Una vez que se han eliminado las entradas con datos faltantes, es
necesario realizar un simple análisis de outliers, ya que como se ve en
las tablas entregadas por la función **summary()**, existen datos
atípicos en cada una de las variables. Para visualizarlo de mejor manera
se utilizarán gráficos boxplot.

``` r
for (i in 1:(ncol(actividades) - 1)) {
  
  boxplot(actividades[, i])
    
}
```

![](README_files/figure-gfm/boxplot%20sin%20filtrar-1.png)<!-- -->![](README_files/figure-gfm/boxplot%20sin%20filtrar-2.png)<!-- -->![](README_files/figure-gfm/boxplot%20sin%20filtrar-3.png)<!-- -->![](README_files/figure-gfm/boxplot%20sin%20filtrar-4.png)<!-- -->![](README_files/figure-gfm/boxplot%20sin%20filtrar-5.png)<!-- -->![](README_files/figure-gfm/boxplot%20sin%20filtrar-6.png)<!-- -->![](README_files/figure-gfm/boxplot%20sin%20filtrar-7.png)<!-- -->![](README_files/figure-gfm/boxplot%20sin%20filtrar-8.png)<!-- -->![](README_files/figure-gfm/boxplot%20sin%20filtrar-9.png)<!-- -->

Los gráficos de boxplot comprueban lo mencionado acerca de la presencia
de datos atípicos en cada una de las variables, por lo que se aplicarán
filtros a cada una de estas para así eliminar las actividades que sean
anormales.

``` r
actividades <- filter(actividades ,actividades$calories < 2500)
actividades <- filter(actividades ,actividades$distance < 50000)
actividades <- filter(actividades ,actividades$elev_low < 2500)
actividades <- filter(actividades ,actividades$elev_low > -1000)
actividades <- filter(actividades ,actividades$elev_high < 5000)
actividades <- filter(actividades ,actividades$max_speed < 50)
actividades <- filter(actividades ,actividades$moving_time < 25000)
actividades <- filter(actividades ,actividades$elapsed_time < 15000)
actividades <- filter(actividades ,actividades$average_speed < 30)
actividades <- filter(actividades ,actividades$total_elevation_gain < 2000)

actividades2 <- filter(actividades2 ,actividades2$calories < 2500)
actividades2 <- filter(actividades2 ,actividades2$distance < 50000)
actividades2 <- filter(actividades2 ,actividades2$elev_low < 2500)
actividades2 <- filter(actividades2 ,actividades2$elev_low > -1000)
actividades2 <- filter(actividades2 ,actividades2$elev_high < 5000)
actividades2 <- filter(actividades2 ,actividades2$max_speed < 50)
actividades2 <- filter(actividades2 ,actividades2$moving_time < 25000)
actividades2 <- filter(actividades2 ,actividades2$elapsed_time < 15000)
actividades2 <- filter(actividades2 ,actividades2$average_speed < 30)
actividades2 <- filter(actividades2 ,actividades2$total_elevation_gain < 2000)

actividades$typeCode <- as.factor(actividades$typeCode)
actividades2$typeCode <- as.factor(actividades2$typeCode)
```

``` r
for (i in 1:(ncol(actividades) - 1)) {
  
  boxplot(actividades[, i])
    
}
```

![](README_files/figure-gfm/boxplot%20con%20filtro-1.png)<!-- -->![](README_files/figure-gfm/boxplot%20con%20filtro-2.png)<!-- -->![](README_files/figure-gfm/boxplot%20con%20filtro-3.png)<!-- -->![](README_files/figure-gfm/boxplot%20con%20filtro-4.png)<!-- -->![](README_files/figure-gfm/boxplot%20con%20filtro-5.png)<!-- -->![](README_files/figure-gfm/boxplot%20con%20filtro-6.png)<!-- -->![](README_files/figure-gfm/boxplot%20con%20filtro-7.png)<!-- -->![](README_files/figure-gfm/boxplot%20con%20filtro-8.png)<!-- -->![](README_files/figure-gfm/boxplot%20con%20filtro-9.png)<!-- -->

En estos gráficos ya se puede observar una clara diferencia con los
anteriores, y con un bias mucho menor por la reducción del ruido que los
datos atípicos generaban.

Finalmente, antes de comenzar a entrenar modelos de clasificación, es
necesario escalar los datos para evitar bias por la diferencia en las
magnitudes de cada variable. Además, se crea una copia del dataset
**actividades**, llamado **actividadesCopia**, el cual será utilizado
más tarde para determinar las actividades que fueron etiquetadas
erróneamente.

``` r
tipos <- data.frame(actividades$typeCode)  

actividades <-data.frame(scale(actividades[0:9]))  

actividades <- cbind.data.frame(actividades, tipos)

actividadesCopia <- actividades
```

# Modelo con máquinas de soporte vectorial

Como se trata de un problema de clasificación con dos posibles
categorías, es que se decidió por realizar un modelo de Máquinas de
Soporte Vectorial, e iterar en el grado del vector de soporte con tal de
encontrar la mejor separáción de las actividades en su respectivo
espacio. Antes de esto, es necesario realizar una separación del dataset
**actividades**, en dos nuevos datasets, uno llamado
**actividadesTrain** para entrenar el modelo, y otro llamado
**actividadesTest** que se utilizará para testear el modelo y determinar
su precisión luego de haber sido entrenado.

``` r
actividadesSample <- actividades[sample(nrow(actividades), 25000),]

set.seed(500)
actividadesSplit <- initial_split(actividadesSample, prop = 0.7)

actividadesTrain  <- training(actividadesSplit)
actividadesTest   <- testing(actividadesSplit)
```

Ahora prosigue crear la receta para la máquina de soporte vectorial,
inspirada en el ejemplo de clases, que será nombrada **modeloSVM**. Esta
función creará, entrenará y testeará un modelo SVM del grado que se le
indique en el argumento.

``` r
receta <- 
  recipe(actividades.typeCode ~ ., data = actividadesTrain) 

receta
```

    ## Data Recipe
    ## 
    ## Inputs:
    ## 
    ##       role #variables
    ##    outcome          1
    ##  predictor          9

Luego se crea la función mencionada. Esta se compone de tres partes, en
primer lugar se crea el modelo de máquinas de sopote vectorial con el
grado especificado en el argumento y se almacena en **modelo**, luego se
entrena a este modelo y esta nueva versión se guarda en **modeloFit**,
el paso siguiente es realizar predicciones basadas en los datos de
testeo previamente apartados, las que se guardan en
**modeloPrediccion**, finalmente se retorna el valor del área bajo la
curva AUC de la curva, valga la redundancia, ROC de este modelo.

``` r
library(kernlab)
```

    ## 
    ## Attaching package: 'kernlab'

    ## The following object is masked from 'package:scales':
    ## 
    ##     alpha

    ## The following object is masked from 'package:purrr':
    ## 
    ##     cross

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     alpha

``` r
modeloSVM <- function(grado){
  
  modelo <- svm_poly(degree = grado)    %>%
            set_engine("kernlab")       %>%
            set_mode("classification")  %>%
            translate()
  
  modeloFit <-  workflow()              %>%
                add_model(modelo)       %>%
                add_recipe(receta)      %>%
                fit(data = actividadesTrain)
  
  modeloPrediccion <- predict(modeloFit, actividadesTest, type = "prob")  %>%
                      bind_cols(actividadesTest)
  
  modeloPrediccion %>% roc_auc(truth = actividades.typeCode, .pred_0)
  return(modeloPrediccion %>% roc_auc(truth = actividades.typeCode, .pred_0))
}
```

Así, ahora es posible realizar análisis de los datos con modelos de
Máquinas de Soporte Vectorial de diferentes grados, lo que facilitará en
gran medida la elección del mejor para el modelo.

``` r
modeloSVM(1)
```

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.933

Como se puede ver, el modelo ya entrega resultados que, si bien no
excelentes, son más que aceptables, sin embargo, se realizarán pruebas
con otros modelos utilizando ajustes polinomiales de grado 2 y 3, para
verificar si es que utilizar alguno de estos sería provechoso.Primero se
muestra en modelo de segundo grado.

``` r
modeloSVM(2)
```

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.990

Y luego el modelo de tercer grado.

``` r
modeloSVM(3)
```

    ## # A tibble: 1 x 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.990

Al aumentar el grado de la curva utilizada para la separación de las
actividades se obtuvo un incremento de más de un 5%, con lo cual los
resultados ya son considerados como muy buenos, con un AUC sobre el
99.% para el modelo de grado 2, mientras que el de grado 3 también entrega un
AUC de un 99.9%. En esta sección es importante destacar que se realizaron múltiples pruebas entre estos modelos y el de grado 3 siempre entregó un AUC mayor o igual al de grado 2.

# Modelo Naive Bayes

Por curiosidad se decidió realizar un modelo utilizando el método Naive
Bayes, para esto, primero se debe realizar una aproximación lineal de
las variables independientes, la cual fue guardada en un dataset llamado
**DPLinear**.

``` r
DP_model <-  naiveBayes(actividades.typeCode ~ calories + distance + elev_low +
                          elev_high + max_speed + moving_time + elapsed_time +
                          average_speed + total_elevation_gain,
                          data = actividadesTrain)
```

Luego corresponde evaluar el modelo, obtener su curva ROC y
posteriormente su AUC.

``` r
PredictionModel <- predict(DP_model, newdata = actividadesTest, type = "raw")

actividadesTest$prob <- PredictionModel[, 2]

curvaROC <- roc(actividades.typeCode ~ prob, data = actividadesTest)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
plot(curvaROC)
```

![](README_files/figure-gfm/naive%20bayes%20predictions-1.png)<!-- -->

``` r
auc(curvaROC)
```

    ## Area under the curve: 0.9324

Como se puede ver, este modelo también es bastante bueno en sus
predicciones, con un AUC de un 93.24%, ubicándose así sobre el modelo
SVM de grado 1 sin embargo, no es suficiente para superar a los modelos
de grado 2 y 3 de SVM que se encuentran mucho más cercanos al 100%. Por
esto es que se decidió escoger al modelo de grado 3 como el apropiado
para determinar si las actividades se han catalogado de manera correcta
o no al ser ingresadas en las distintas aplicaciones de rastreo de
actividades, ya que como el objetivo secundario es identificarlas, se
requiere de una herramienta que permita hacerlo con la mayor precisión
posible. Cabe destacar que no se intentaron modelos de grado 4 o
superior debido a limitaciones de hardware.

# Identificación de actividades calificadas erróneamente

Luego de haber determinado el modelo a utilizar, este se utilizará para
verificar si es que las actividades fueron catalogadas correctamente por
los usuarios que las ingresaron, las cuales serán guardadas en un nuevo
dataframe llamado **actividadesErroneas**. Aunque primero es necesario
entrenar el modelo y realizar las predicciones del dataset
**actividadesCopia**, el cual comprende los datos previamente limpiados
y escalados.

``` r
modelo <- svm_poly(degree = 3)    %>%
          set_engine("kernlab")       %>%
          set_mode("classification")  %>%
          translate()

modeloFit <-  workflow()              %>%
              add_model(modelo)       %>%
              add_recipe(receta)      %>%
              fit(data = actividadesTrain)

modeloPrediccion <- predict(modeloFit, actividadesCopia, type = "prob")  %>%
                    bind_cols(actividadesCopia)
```

Una vez obtenidas las probabilidades de pertenencia de las actividades a
cada tipo de actividad, se puede determinar a qué tipo pertenece cada
una comparando sus probabilidades, si la probabilidad de pertenencia a
tipo 0 es mayor a la de tipo 1, entonces la actividad se cataloga como
actividad hecha a pie, y si ocurre lo contrario entonces se cataloga
como hecha en bicicleta. Esta calificación será guardada en el dataframe
previamente creado llamado **actividades2**, ya que este contiene los
IDs de las actividades, con lo que luego podrán ser identificadas en el
dataset original.

``` r
actividades2$prediccion <- ifelse(modeloPrediccion$.pred_0 >= modeloPrediccion$.pred_1, 0, 1)
```

Luego se pueden seleccionar aquellas actividades erróneamente
categorizadas comparando las variables **actividades.typeCode** y
**prediccion** en el dataframe **actividades2**, las cuales serán
guardadas en el dataset **actividadesErroneas**.

``` r
actividadesErroneas <- actividades2 %>% filter(typeCode != prediccion)
```

Para comprobar la correcta selección de las actividades catalogadas
erróneamente se muestran los primeros 20 elementos del dataset
**actividadesErroneas**, para una rápida inspección visual.

``` r
head(actividadesErroneas, n = 20)
```

    ##     id calories distance elev_low elev_high max_speed moving_time elapsed_time
    ## 1   17    114.6   2944.7    604.0     647.2    15.500        1069         1260
    ## 2   23    104.3   2423.4    603.9     635.6     6.800         799          937
    ## 3   41    549.7   2403.9    349.3     379.0    11.600        3213         3243
    ## 4   53    291.4   3456.9    627.8     649.0    15.400         308          325
    ## 5  107    771.1  26650.8    563.9     783.9    33.679        6168         6785
    ## 6  125    195.4   5294.7    338.2     369.9     8.100        2254         2408
    ## 7  208     62.4   3632.1    628.7     650.1     6.100        1228         1400
    ## 8  232   2233.0  30123.1    651.0     726.6     6.600       10227        10480
    ## 9  235    640.0  14463.2   1222.4    1266.2     9.500       13227        13869
    ## 10 241     17.0      0.0    703.0     706.0     0.000        1094         1094
    ## 11 380    299.4   4450.2    628.8     857.2     6.400        1465         1466
    ## 12 471    239.0  12567.4    671.8     725.2     8.300        5262         5854
    ## 13 554   1116.0   2682.5    667.0     677.0     3.100        1241         4964
    ## 14 575   1705.0  24035.9    -21.6     396.2    11.300        9070         9461
    ## 15 598    715.0  11253.7    873.8    1036.0    17.200        4531         6215
    ## 16 661    952.0   5359.8    606.4     955.2     9.200        3032         7974
    ## 17 865     57.0   1909.9    170.8     175.4     5.500         687          794
    ## 18 944    174.9   2820.8    182.4     315.1     2.600        1484         3592
    ## 19 946    706.0   8990.3    148.6     243.6     9.100        3579         5267
    ## 20 968      0.5     10.7    417.5     417.6     2.300           5            5
    ##    average_speed total_elevation_gain typeCode prediccion
    ## 1          2.755                 49.1        1          0
    ## 2          3.033                 31.6        1          0
    ## 3          0.748                 32.9        1          0
    ## 4         11.224                 11.0        0          1
    ## 5          4.321                333.2        1          0
    ## 6          2.349                 65.3        1          0
    ## 7          2.958                 20.0        1          0
    ## 8          2.945                182.8        1          0
    ## 9          1.093                161.6        0          1
    ## 10         0.000                  0.0        0          1
    ## 11         3.038                230.4        1          0
    ## 12         2.388                 74.0        1          0
    ## 13         2.162                  4.0        0          1
    ## 14         2.650                911.0        1          0
    ## 15         2.484                216.0        1          0
    ## 16         1.768                344.0        0          1
    ## 17         2.780                  7.0        1          0
    ## 18         1.901                 99.0        1          0
    ## 19         2.512                155.0        1          0
    ## 20         2.140                  0.0        1          0

Como se puede ver, las 20 actividades mostradas tienen una predicción
diferente a su tipo original, lo que sumado a una rápida inspección
visual del dataframe, se deduce una correcta selección de estas
actividades mal catalogadas en base a la predicción realizada. Por
último, se muestra el número de actividades que el modelo determinó que
fueron catalogadas erróneamente.

``` r
nrow(actividadesErroneas)
```

    ## [1] 4906
