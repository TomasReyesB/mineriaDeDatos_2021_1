Proyecto N°2 - Spotify Playlist
================

## Cargando los datos

Para cargar los datos a analizar se emplea la función **load()**, la
cual acepta como parámetro la dirección en el directorio del archivo a
cargar, sin embargo, como en este caso el archivo **beats.RData** se
encuentra en el directorio root del proyecto, solo se indica el nombre
del archivo.

``` r
load("beats.RData")
```

## Preprocesamiento de los datos

Una vez cargados los datos, es necesario realizar una limpieza de estos,
para así poder realizar el posterior análisis de mejor manera y sin
errores en los procesos de clustering debido a datos faltantes por
ejemplo. Así, lo primero corresponde a escoger solo aquellas columnas
que sean relevantes para el caso de estudio, las que en este caso son:
-**Danceability** -**Energy** -**Key** -**Loudness** -**Mode**
-**Speechiness** -**Acousticness** -**Instrumentalness** -**Liveness**
-**Valence** -**Tenpo** Además, también se deben conservar las columnas
**track\_id** para identificar la canción, **duration\_ms** para luego
determinar el largo de la playlists a crear, y finalmente
**track\_name** y **artist\_name**, para poder entregar una presentación
final de la playlist un tanto más agradable de lo que sería presentar
sólamente los nombres de las canciones. Estas columnas serán guardadas
en un nuevo dataframe llamado **songs**, luego se muestra un resumen de
este.

``` r
songs <- beats[, c(1,8:19, 23, 27)]

summary(songs)
```

    ##  artist_name         danceability        energy            key        
    ##  Length:447622      Min.   :0.0000   Min.   :0.0000   Min.   : 0.000  
    ##  Class :character   1st Qu.:0.2520   1st Qu.:0.0756   1st Qu.: 2.000  
    ##  Mode  :character   Median :0.3700   Median :0.2100   Median : 5.000  
    ##                     Mean   :0.3911   Mean   :0.3405   Mean   : 5.061  
    ##                     3rd Qu.:0.5140   3rd Qu.:0.5820   3rd Qu.: 8.000  
    ##                     Max.   :0.9860   Max.   :1.0000   Max.   :11.000  
    ##     loudness            mode         speechiness       acousticness   
    ##  Min.   :-60.000   Min.   :0.0000   Min.   :0.00000   Min.   :0.0000  
    ##  1st Qu.:-24.445   1st Qu.:0.0000   1st Qu.:0.03780   1st Qu.:0.3940  
    ##  Median :-19.477   Median :1.0000   Median :0.04430   Median :0.9230  
    ##  Mean   :-18.672   Mean   :0.6834   Mean   :0.06892   Mean   :0.6987  
    ##  3rd Qu.:-11.644   3rd Qu.:1.0000   3rd Qu.:0.05840   3rd Qu.:0.9860  
    ##  Max.   :  0.496   Max.   :1.0000   Max.   :0.97100   Max.   :0.9960  
    ##  instrumentalness     liveness         valence           tempo       
    ##  Min.   :0.00000   Min.   :0.0000   Min.   :0.0000   Min.   :  0.00  
    ##  1st Qu.:0.00169   1st Qu.:0.0968   1st Qu.:0.0894   1st Qu.: 82.39  
    ##  Median :0.71500   Median :0.1230   Median :0.2740   Median :105.72  
    ##  Mean   :0.50607   Mean   :0.2217   Mean   :0.3374   Mean   :108.74  
    ##  3rd Qu.:0.90100   3rd Qu.:0.2530   3rd Qu.:0.5370   3rd Qu.:131.05  
    ##  Max.   :1.00000   Max.   :1.0000   Max.   :0.9960   Max.   :244.95  
    ##    track_id          duration_ms       track_name       
    ##  Length:447622      Min.   :   1066   Length:447622     
    ##  Class :character   1st Qu.: 123440   Class :character  
    ##  Mode  :character   Median : 194961   Mode  :character  
    ##                     Mean   : 229110                     
    ##                     3rd Qu.: 271560                     
    ##                     Max.   :4796395

Luego, es necesario verificar la presencia de valores faltantes que
serán eliminados del dataframe. Para esto se utiliza la función
**summarize\_all**, la cual, de acuerdo a los argumentos entregados,
cuenta la cantidad de valores NA en cada columna, luego, este resutado
se guarda en un nuevo dataframe auxiliar llamado **aux**.

``` r
#Valores faltantes se reemplazan por NA
songs[songs == ''] <- NA

#se verifica la presencia de NAs
aux <- songs %>% summarise_all(funs(sum(is.na(.))))
```

    ## Warning: `funs()` was deprecated in dplyr 0.8.0.
    ## Please use a list of either functions or lambdas: 
    ## 
    ##   # Simple named list: 
    ##   list(mean = mean, median = median)
    ## 
    ##   # Auto named with `tibble::lst()`: 
    ##   tibble::lst(mean, median)
    ## 
    ##   # Using lambdas
    ##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))

``` r
head(aux)
```

    ##   artist_name danceability energy key loudness mode speechiness acousticness
    ## 1           0            0      0   0        0    0           0            0
    ##   instrumentalness liveness valence tempo track_id duration_ms track_name
    ## 1                0        0       0     0        0           0          0

Como no se detecta le presencia de valores NA, se prosigue con la
filtración y remoción de datos duplicados. Para esto se utilizará el
campo **track\_id**, logrando así eliminar todas las canciones
exacatamente iguales duplicadas, sin eliminar demás, cosa que ocurriría
al eliminar duplicados de acuerdo al nombre de la canción por ejemplo.

``` r
songsNoDup <- songs[!duplicated(songs$track_id),]
```

A continuación se muestra la cantidad de canciones en el dataframe
**songs**, con duplicados, y en el dataframe **songsNoDup**, sin
duplicados, entre los que se puede ver una diferencia de 2525 canciones.

``` r
nrow(songs)
```

    ## [1] 447622

``` r
nrow(songsNoDup)
```

    ## [1] 445097

Como los valores restantes de interés son numéricos, y en el caso de los
campos de tipo character, estos no tienen relevancia en el análisis de
clustering. Por esto, luego de revisar el archivo **lista\_variables**,
donde se incluye una descripción de cada columna de los datos
originales, es que los datos numéricos se transformarán a **double**,
meramente para respetar la naturaleza de estos y evitar posibles
variaciones indeseadas en el análisis.

``` r
songsNoDup$danceability <- as.double(as.character(songsNoDup$danceability))
songsNoDup$energy <- as.double(as.character(songsNoDup$energy))
songsNoDup$key <- as.double(as.character(songsNoDup$key))
songsNoDup$loudness <- as.double(as.character(songsNoDup$loudness))
songsNoDup$mode <- as.double(as.character(songsNoDup$mode))
songsNoDup$speechiness <- as.double(as.character(songsNoDup$speechiness)) 
songsNoDup$acousticness <- as.double(as.character(songsNoDup$acousticness))
songsNoDup$instrumentalness <- as.double(as.character(songsNoDup$instrumentalness))
songsNoDup$liveness <- as.double(as.character(songsNoDup$liveness))
songsNoDup$valence <- as.double(as.character(songsNoDup$valence))
songsNoDup$tempo <- as.double(as.character(songsNoDup$tempo))
songsNoDup$duration_ms <- as.double(as.character(songsNoDup$duration_ms))
```

Luego, se realiza el mismo procedimiento con el resto de las columnas
previamente seleccionadas.

``` r
songsNoDup$track_id <- as.character(songsNoDup$track_id)
songsNoDup$track_name <- as.character(songsNoDup$track_name)
songsNoDup$artist_name <- as.character(songsNoDup$artist_name)
```

Luego de estas transformaciones, es necesario volver a comprobar la
presencia de valores faltantes NA en el dataframe **songsNoDup** y
eliminarlos.

``` r
songsNoDup <- songsNoDup %>% filter(!(is.na(key)|is.na(danceability)|is.na(energy)|is.na(loudness)|is.na(mode)|is.na(speechiness)))

songsNoDup <- songsNoDup %>% filter(!(is.na(acousticness)|is.na(instrumentalness)|is.na(liveness)|is.na(valence)|is.na(tempo)|is.na(duration_ms)))

summary(songsNoDup)
```

    ##  artist_name         danceability        energy            key        
    ##  Length:445097      Min.   :0.0000   Min.   :0.0000   Min.   : 0.000  
    ##  Class :character   1st Qu.:0.2520   1st Qu.:0.0754   1st Qu.: 2.000  
    ##  Mode  :character   Median :0.3690   Median :0.2090   Median : 5.000  
    ##                     Mean   :0.3907   Mean   :0.3400   Mean   : 5.061  
    ##                     3rd Qu.:0.5130   3rd Qu.:0.5800   3rd Qu.: 8.000  
    ##                     Max.   :0.9860   Max.   :1.0000   Max.   :11.000  
    ##     loudness            mode         speechiness       acousticness   
    ##  Min.   :-60.000   Min.   :0.0000   Min.   :0.00000   Min.   :0.0000  
    ##  1st Qu.:-24.468   1st Qu.:0.0000   1st Qu.:0.03780   1st Qu.:0.3970  
    ##  Median :-19.513   Median :1.0000   Median :0.04430   Median :0.9240  
    ##  Mean   :-18.699   Mean   :0.6836   Mean   :0.06896   Mean   :0.6993  
    ##  3rd Qu.:-11.698   3rd Qu.:1.0000   3rd Qu.:0.05840   3rd Qu.:0.9860  
    ##  Max.   :  0.496   Max.   :1.0000   Max.   :0.97100   Max.   :0.9960  
    ##  instrumentalness     liveness         valence           tempo       
    ##  Min.   :0.00000   Min.   :0.0000   Min.   :0.0000   Min.   :  0.00  
    ##  1st Qu.:0.00177   1st Qu.:0.0968   1st Qu.:0.0889   1st Qu.: 82.35  
    ##  Median :0.71900   Median :0.1230   Median :0.2730   Median :105.71  
    ##  Mean   :0.50728   Mean   :0.2215   Mean   :0.3368   Mean   :108.71  
    ##  3rd Qu.:0.90100   3rd Qu.:0.2520   3rd Qu.:0.5360   3rd Qu.:131.05  
    ##  Max.   :1.00000   Max.   :1.0000   Max.   :0.9960   Max.   :244.95  
    ##    track_id          duration_ms       track_name       
    ##  Length:445097      Min.   :   1066   Length:445097     
    ##  Class :character   1st Qu.: 123036   Class :character  
    ##  Mode  :character   Median : 194613   Mode  :character  
    ##                     Mean   : 228908                     
    ##                     3rd Qu.: 271106                     
    ##                     Max.   :4796395

## Muestreo aleatorio

Para realizar el muestreo aleatorio de los datos se utilizará la función
**sample()** de R base, a la cual se le debe entregar como argumento el
largo de las filas que se obtendrán, así como también la cantidad de
datos que la muestra deberá contener, en este caso se indican 25000, que
serán guardados en un nuevo dataframe llamado **sampleSongs**. Luego, se
muestra un resumen de los datos en este nuevo dataframe recién creado.

``` r
set.seed(500)

sampleSongs <- songsNoDup[sample(nrow(songsNoDup), 25000),]

summary(sampleSongs)
```

    ##  artist_name         danceability        energy            key        
    ##  Length:25000       Min.   :0.0000   Min.   :0.0000   Min.   : 0.000  
    ##  Class :character   1st Qu.:0.2520   1st Qu.:0.0744   1st Qu.: 2.000  
    ##  Mode  :character   Median :0.3670   Median :0.2080   Median : 5.000  
    ##                     Mean   :0.3889   Mean   :0.3391   Mean   : 5.052  
    ##                     3rd Qu.:0.5090   3rd Qu.:0.5790   3rd Qu.: 8.000  
    ##                     Max.   :0.9720   Max.   :1.0000   Max.   :11.000  
    ##     loudness            mode        speechiness      acousticness  
    ##  Min.   :-60.000   Min.   :0.000   Min.   :0.0000   Min.   :0.000  
    ##  1st Qu.:-24.512   1st Qu.:0.000   1st Qu.:0.0379   1st Qu.:0.406  
    ##  Median :-19.592   Median :1.000   Median :0.0444   Median :0.926  
    ##  Mean   :-18.774   Mean   :0.683   Mean   :0.0686   Mean   :0.702  
    ##  3rd Qu.:-11.883   3rd Qu.:1.000   3rd Qu.:0.0584   3rd Qu.:0.986  
    ##  Max.   : -0.531   Max.   :1.000   Max.   :0.9690   Max.   :0.996  
    ##  instrumentalness     liveness         valence           tempo       
    ##  Min.   :0.00000   Min.   :0.0000   Min.   :0.0000   Min.   :  0.00  
    ##  1st Qu.:0.00199   1st Qu.:0.0967   1st Qu.:0.0897   1st Qu.: 82.26  
    ##  Median :0.73700   Median :0.1230   Median :0.2710   Median :105.53  
    ##  Mean   :0.51125   Mean   :0.2216   Mean   :0.3350   Mean   :108.51  
    ##  3rd Qu.:0.90200   3rd Qu.:0.2550   3rd Qu.:0.5320   3rd Qu.:130.83  
    ##  Max.   :1.00000   Max.   :1.0000   Max.   :0.9850   Max.   :244.95  
    ##    track_id          duration_ms       track_name       
    ##  Length:25000       Min.   :   4066   Length:25000      
    ##  Class :character   1st Qu.: 121209   Class :character  
    ##  Mode  :character   Median : 193927   Mode  :character  
    ##                     Mean   : 229151                     
    ##                     3rd Qu.: 271803                     
    ##                     Max.   :4684626

## Escalamiento de los datos

Para poder escalar de manera correcta los datos es que se separará el
dataframe en dos nuevos dataframes, uno con las variables numéricas y
otro con las de tipo character.

``` r
songsChar <- sampleSongs %>% 
  select(c("artist_name", "track_id", "track_name"))

songsNum <- sampleSongs %>%
  select(c("key", "danceability", "energy", "loudness", "mode", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo", "duration_ms"))
```

Una vez realizado esto ya se pueden escalar los datos para su posterior
análisis de clustering.

``` r
songsScale <- data.frame(sapply(songsNum, scale))  
```

# Procesamiento de los Datos

## Análisis de Cluster

Con los datos ya escalados, es posible comenzar con el análisis de
clusters. Para un primer acercamiento se utilizarán 10 clusters, lo que
luego será modificado acorde a los resultados obtenidos.

``` r
modelo_kmeans10 <- kmeans(songsScale, centers = 10)

songsScale$clus <- modelo_kmeans10$cluster %>% as.factor()

ggplot(songsScale, aes(liveness, energy, color=clus)) +
  geom_point(alpha=0.5, show.legend = T) +
  theme_bw()
```

![](README_files/figure-gfm/k%20means-1.png)<!-- -->

``` r
ggplot(songsScale, aes(acousticness, speechiness, color=clus)) +
  geom_point(alpha=0.5, show.legend = T) +
  theme_bw()
```

![](README_files/figure-gfm/k%20means-2.png)<!-- -->

``` r
ggplot(songsScale, aes(loudness, instrumentalness, color=clus)) +
  geom_point(alpha=0.5, show.legend = T) +
  theme_bw()
```

![](README_files/figure-gfm/k%20means-3.png)<!-- -->

En los gráficos se puede ver que los clústers son desordenados y
esparcidos, por lo que K = 10 no parece ser una buena elección, y
suamado a que buscamos que las canciones similares estén en el mismo
cluster, y por lo tanto cercanas entre si, es que se realizará una
análisis de la evolución de la suma de cuadrados de la distancia entre
los puntos de un mismo cluster a medida que varía K, a fin de encontrar
el mejor valor de este.

``` r
kmeansSumaCuadrados <- numeric(30)

for(k in 1:30){
  modelo <- kmeans(songsScale, centers = k)
  kmeansSumaCuadrados[k] <- modelo$tot.withinss
}

plot(kmeansSumaCuadrados)
```

![](README_files/figure-gfm/suma%20cuadrados-1.png)<!-- -->

De este gráfico se puede deducir que desde alrededor de K = 7 el cambio
resulta no ser muy significativo, por lo que este valor será candidato a
utilizarse.

## Coeficiente de silueta para encontrar el mejor valor de K

Para confirmar o rechazar la propuesta K = 7 es que se realizará un
análisis del coeficiente de silueta para encontrar el mejor valor de K.

``` r
coefSil=numeric(30)

for (k in 2:30){
  modelo <- kmeans(songsScale, centers = k)
  temp <- silhouette(modelo$cluster,dist(songsScale))
  coefSil[k] <- mean(temp[,3])
}
tempDF=data.frame(CS=coefSil,K=c(1:30))

ggplot(tempDF, aes(x=K, y=CS)) + 
  geom_line() +
  scale_x_continuous(breaks=c(1:30))
```

![](README_files/figure-gfm/coeficiente%20silueta-1.png)<!-- -->

Este gráfico rechaza categóricamente el valor de K = 7 anteriormente
propuesto, e inlcuso, indica que el mejor valor K para maximizar el
coeficiente de silueta corresponde a 2, esto es, hacer 2 clusters de
datos, lo que en este caso va completamente contra la intuición, es por
esto que se aplicarán otros métodos de clustering al primero de los
clusters resultantes con K = 2. Luego se grafican los resultados
obtenidos se acuerdo a diferentes combinaciones de variables para
intentar distinguir, dentro de lo posible, la agrupación realizada.

``` r
modeloKmeans2 = kmeans(songsScale, centers = 2)

songsScale$clus <- modeloKmeans2$cluster %>% as.factor()

ggplot(songsScale, aes(liveness, energy, color=clus)) +
  geom_point(alpha=0.5, show.legend = T) +
  theme_bw()
```

![](README_files/figure-gfm/kmeans%202%20clusters-1.png)<!-- -->

``` r
ggplot(songsScale, aes(acousticness, speechiness, color=clus)) +
  geom_point(alpha=0.5, show.legend = T) +
  theme_bw()
```

![](README_files/figure-gfm/kmeans%202%20clusters-2.png)<!-- -->

``` r
ggplot(songsScale, aes(loudness, instrumentalness, color=clus)) +
  geom_point(alpha=0.5, show.legend = T) +
  theme_bw()
```

![](README_files/figure-gfm/kmeans%202%20clusters-3.png)<!-- -->

A continuación, es necesario extarer los datos pertenecientes al primer
cluster de este modelo, con el fin de analizarlos con mayor detalle,
estos serán guardados en un nuevo dataframe llamado **primerCluster**.

``` r
primerCluster <- songsScale %>% filter(songsScale$clus == 1)
summary(primerCluster)
```

    ##       key            danceability         energy           loudness      
    ##  Min.   :-1.45365   Min.   :-2.1385   Min.   :-1.0666   Min.   :-2.8497  
    ##  1st Qu.:-0.87817   1st Qu.:-0.1478   1st Qu.:-0.2302   1st Qu.: 0.2952  
    ##  Median :-0.01494   Median : 0.5891   Median : 0.6929   Median : 1.0736  
    ##  Mean   : 0.04001   Mean   : 0.6015   Mean   : 0.6158   Mean   : 0.8392  
    ##  3rd Qu.: 0.84829   3rd Qu.: 1.3316   3rd Qu.: 1.4710   3rd Qu.: 1.4537  
    ##  Max.   : 1.71151   Max.   : 3.2068   Max.   : 2.0822   Max.   : 2.1193  
    ##       mode           speechiness        acousticness     instrumentalness 
    ##  Min.   :-1.46782   Min.   :-0.73693   Min.   :-1.9081   Min.   :-1.2360  
    ##  1st Qu.:-1.46782   1st Qu.:-0.34482   1st Qu.:-1.8013   1st Qu.:-1.2360  
    ##  Median : 0.68126   Median :-0.23202   Median :-0.9324   Median :-1.2350  
    ##  Mean   : 0.05095   Mean   : 0.25099   Mean   :-0.7243   Mean   :-0.9695  
    ##  3rd Qu.: 0.68126   3rd Qu.: 0.07093   3rd Qu.: 0.3750   3rd Qu.:-1.1112  
    ##  Max.   : 0.68126   Max.   : 9.67286   Max.   : 0.7990   Max.   : 1.1526  
    ##     liveness          valence            tempo          duration_ms      
    ##  Min.   :-1.0136   Min.   :-1.2260   Min.   :-3.4195   Min.   :-1.23629  
    ##  1st Qu.:-0.5424   1st Qu.:-0.3477   1st Qu.:-0.4188   1st Qu.:-0.32528  
    ##  Median :-0.3183   Median : 0.3294   Median : 0.3011   Median :-0.05432  
    ##  Mean   : 0.1805   Mean   : 0.4155   Mean   : 0.3203   Mean   : 0.02486  
    ##  3rd Qu.: 0.4913   3rd Qu.: 1.1455   3rd Qu.: 0.8936   3rd Qu.: 0.26560  
    ##  Max.   : 3.5604   Max.   : 2.3678   Max.   : 3.3993   Max.   :24.56211  
    ##  clus    
    ##  1:9823  
    ##  2:   0  
    ##          
    ##          
    ##          
    ## 

Una vez obtenidos los datos pertenecientes al primer cluster del modelo
K-means implementado, es posible realizar el análisis de clusters GMM,
con el fin de profundizar en el análisis de estos. Este modelo será
guardado en la lista **modeloKmeansGMM**.

``` r
modeloKmeansGMM <- Mclust(primerCluster)
plot(modeloKmeansGMM, what = 'classification')
```

![](README_files/figure-gfm/kmeans%20GMM-1.png)<!-- -->

Luego, con la función **summary()**, se puede visualizar la cantidad de
clusters obtenidos al aplicar este método al primer cluster obtenido.

``` r
summary(modeloKmeansGMM)
```

    ## ---------------------------------------------------- 
    ## Gaussian finite mixture model fitted by EM algorithm 
    ## ---------------------------------------------------- 
    ## 
    ## Mclust XXX (ellipsoidal multivariate normal) model with 1 component: 
    ## 
    ##  log-likelihood    n  df      BIC      ICL
    ##        126989.2 9823 104 253022.3 253022.3
    ## 
    ## Clustering table:
    ##    1 
    ## 9823

Como se puede ver, al realizar el análisis en este orden no se logra
encontrar nueva información, ya que el algoritmo GMM no es capaz de
discernir nuevos clusters dentro del cual fue entregado.

## Clusterización GMM

Debido a lo previamente mencionado es que se decició realizar el proceso
en orden inverso, esto es, primero realizar clusterización GMM y luego
elegir uno de los clusters resultantes para apricarle el proceso de
clusterización K-means. Este modelo será guardado en una nueva lista
llamada **modeloGMM**.

``` r
modeloGMM <- Mclust(songsScale)
plot(modeloGMM, what = 'classification')
```

![](README_files/figure-gfm/modelo%20GMM-1.png)<!-- -->

El gráfico de la clusterización no deja muy claras las cosas, aunque
ayuda a distinguir a muy bajo nivel la división entre los clusters en la
data de estudio. Por esto es que se decidió utilizar la función
**summary()** nuevamente, para obtener la cantidad de clusters así como
también cuántos elementos posee cada uno.

``` r
summary(modeloGMM)
```

    ## ---------------------------------------------------- 
    ## Gaussian finite mixture model fitted by EM algorithm 
    ## ---------------------------------------------------- 
    ## 
    ## Mclust VEV (ellipsoidal, equal shape) model with 5 components: 
    ## 
    ##  log-likelihood     n  df       BIC       ICL
    ##       -240911.3 25000 476 -486642.8 -487260.3
    ## 
    ## Clustering table:
    ##    1    2    3    4    5 
    ## 1654 3618 7428 3524 8776

Luego, es necesario asignar a cada canción el cluster al que pertenece,
para esto se crea un nuevo dataframe llamado **sampleSongsClusters**, en
donde se agregan todas las columnas del dataframe **sampleSongS**,
además de una extra correspondiente al cluster al que cada elemento
pertenece.

``` r
sampleSongsClusters <- NULL
clusters <- modeloGMM$classification

sampleSongsClusters <- cbind(sampleSongs, clusters)
```

Ahora es cuando se vuelve necesario escoger la canción para la creación
de la playlist solicitada. Para esto, se seleccionará una canción al
azar desde la muestra de datos, y luego se procederá a un análisis de
clustering con K-means para una mejor segmentación.

``` r
cancion <- sampleSongsClusters[sample(nrow(sampleSongsClusters), 1),]

print(cancion$artist_name)
```

    ## [1] "Electric Light Orchestra"

``` r
print(cancion$track_name)
```

    ## [1] "Need Her Love"

``` r
print(cancion$clusters)
```

    ## [1] 5

Como se puede ver la canción escogida pertenece al cluster N°3, por lo
que se trabajará con este. Sin embargo, se deja el código implementado
de manera tal que funcione con cualquier canción, independiente del
cluster al que pertenece. De esta manera, se guarda el cluster al que la
canción escogida pertenece en un nuevo dataframe llamado **cluster**.

``` r
cluster <- sampleSongsClusters %>% filter(sampleSongsClusters$clusters == cancion$clusters)
summary(cluster)
```

    ##  artist_name         danceability        energy            key        
    ##  Length:8776        Min.   :0.0000   Min.   :0.0025   Min.   : 0.000  
    ##  Class :character   1st Qu.:0.3570   1st Qu.:0.2550   1st Qu.: 2.000  
    ##  Mode  :character   Median :0.4870   Median :0.5460   Median : 5.000  
    ##                     Mean   :0.4909   Mean   :0.5244   Mean   : 5.145  
    ##                     3rd Qu.:0.6150   3rd Qu.:0.7940   3rd Qu.: 8.000  
    ##                     Max.   :0.9710   Max.   :1.0000   Max.   :11.000  
    ##     loudness            mode         speechiness       acousticness      
    ##  Min.   :-42.109   Min.   :0.0000   Min.   :0.00000   Min.   :0.0000012  
    ##  1st Qu.:-15.824   1st Qu.:0.0000   1st Qu.:0.03550   1st Qu.:0.0372000  
    ##  Median : -9.498   Median :1.0000   Median :0.04450   Median :0.3595000  
    ##  Mean   :-11.459   Mean   :0.7231   Mean   :0.05419   Mean   :0.4382847  
    ##  3rd Qu.: -6.316   3rd Qu.:1.0000   3rd Qu.:0.06050   3rd Qu.:0.8532500  
    ##  Max.   : -0.555   Max.   :1.0000   Max.   :0.25100   Max.   :0.9960000  
    ##  instrumentalness       liveness         valence           tempo       
    ##  Min.   :0.0000000   Min.   :0.0000   Min.   :0.0000   Min.   :  0.00  
    ##  1st Qu.:0.0000032   1st Qu.:0.1020   1st Qu.:0.2410   1st Qu.: 96.24  
    ##  Median :0.0006025   Median :0.1440   Median :0.4190   Median :118.98  
    ##  Mean   :0.1069276   Mean   :0.2501   Mean   :0.4472   Mean   :119.07  
    ##  3rd Qu.:0.0540250   3rd Qu.:0.3060   3rd Qu.:0.6440   3rd Qu.:136.92  
    ##  Max.   :0.9880000   Max.   :1.0000   Max.   :0.9820   Max.   :216.38  
    ##    track_id          duration_ms      track_name           clusters
    ##  Length:8776        Min.   :  4893   Length:8776        Min.   :5  
    ##  Class :character   1st Qu.:173349   Class :character   1st Qu.:5  
    ##  Mode  :character   Median :221793   Mode  :character   Median :5  
    ##                     Mean   :231244                      Mean   :5  
    ##                     3rd Qu.:278440                      3rd Qu.:5  
    ##                     Max.   :865866                      Max.   :5

Luego, es necesario realizar un análisis de la cantidad de clusters que
entreguen las mejores métricas al momento de aplicar el método k-means,
por lo que se realiza un análisis de coeficiente de silueta, utilizando
las variables numéricas de **cluster**, las cuales fueron guardadas en
un uevo dataframe llamado **clusterNum**.

``` r
clusterNum <- cluster %>% select(c("key", "danceability", "energy", "loudness", "mode", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo", "duration_ms"))

coefSil=numeric(30)

for (k in 2:30){
  modelo <- kmeans(clusterNum, centers = k)
  temp <- silhouette(modelo$cluster,dist(clusterNum))
  coefSil[k] <- mean(temp[,3])
}
```

    ## Warning: Quick-TRANSfer stage steps exceeded maximum (= 438800)

    ## Warning: Quick-TRANSfer stage steps exceeded maximum (= 438800)

    ## Warning: did not converge in 10 iterations

    ## Warning: Quick-TRANSfer stage steps exceeded maximum (= 438800)

``` r
tempDF=data.frame(CS=coefSil,K=c(1:30))

ggplot(tempDF, aes(x=K, y=CS)) + 
  geom_line() +
  scale_x_continuous(breaks=c(1:30))
```

![](README_files/figure-gfm/coeficiente%20silueta%20cluster-1.png)<!-- -->

De acuerdo a lo observado en el gráfico, se decidió utilizar k = 5 ya
que entrega un mayor nivel de segmentación, a la vez que no afecta en
gran manera el valor del coeficiente de silueta, que se logra mantener
en valores bastante cercanos al máximo obtenido con K = 2.

Para confirmar la teoría es que se realizó un análisis de la suma de
cuadrados, desde 1 hasta 30 clusters, para obtener una mejor
visualización se graficaron estos valores, mostrados a continuación.

``` r
kmeansSumaCuadradoscluster <- numeric(30)

for(k in 1:30){
  modelo <- kmeans(clusterNum, centers = k)
  kmeansSumaCuadradoscluster[k] <- modelo$tot.withinss
}
```

    ## Warning: Quick-TRANSfer stage steps exceeded maximum (= 438800)

    ## Warning: Quick-TRANSfer stage steps exceeded maximum (= 438800)

    ## Warning: Quick-TRANSfer stage steps exceeded maximum (= 438800)

    ## Warning: Quick-TRANSfer stage steps exceeded maximum (= 438800)

    ## Warning: Quick-TRANSfer stage steps exceeded maximum (= 438800)

``` r
plot(kmeansSumaCuadradoscluster)
```

![](README_files/figure-gfm/suma%20cuadrados%203-1.png)<!-- -->

Como se puede ver, el “codo” o punto de quiebre del gráfico se encuentra
entre K = 5 y K = 6, lo que refuerza lo propuesto anteriormente. Así, se
decidió por realizar un análisis K-means con K = 5 en el cluster de la
canción escogida.

``` r
modeloKmeansCluster <- kmeans(clusterNum, centers = 5)

cluster$clusters <- NULL
cluster$clusters <- modeloKmeansCluster$cluster

cancionNuevoCluster <- cluster %>% filter(cluster$track_id == cancion$track_id)

KmeansCluster <- cluster %>% filter(cluster$clusters == cancionNuevoCluster$clusters)
```

Ahora es necesario encontrar el cluster al que fue asignada la canción
seleccionada, para luego comenzar a elegir canciones de ese mismo
cluster y agregarlas a la playlist, que será almacenada en un nuevo
dataframe llamado **Playlist**, a la cual se dejarán de agregar
canciones una vez que la variable **contadorTiempo** sea mayor a
10800000 milisegundos. Al final de este proceso se muestra el tiempo
total en milisegundos que dura la playlist, meramente para confirmar que
se cumpla el requisito de extensión mínima de 3 horas, equivalente a
10800000 milisegundos.

``` r
cancionNuevoCluster <- cluster %>% filter(cluster$track_id == cancion$track_id)

Playlist <- NULL

contadorTiempo <- cancion$duration_ms
tiempoMax <- 10800000

Playlist <- rbind.data.frame(Playlist, cancion)

while (contadorTiempo <= tiempoMax) {
  
  cancionElegida <- KmeansCluster[sample(nrow(KmeansCluster), 1),]
  
  if(any(Playlist$track_id == cancionElegida$track_id)){
    next
  }
  
  Playlist <- rbind.data.frame(Playlist, cancionElegida)
  
  contadorTiempo = contadorTiempo + as.numeric(cancionElegida$duration_ms) 
}

print(contadorTiempo)
```

    ## [1] 10834039

La lista de reprodución creada se muestra a continuación.

``` r
PlaylistPrint <- Playlist %>% select(c("track_name"))


head(as.matrix.data.frame(PlaylistPrint), nrow(PlaylistPrint))
```

    ##       track_name                                                                                             
    ## 61581 "Need Her Love"                                                                                        
    ## 1764  "El Peor De Mis Fracasos - En Vivo en Puerto Rico - Teatro Bellas Artes /2000"                         
    ## 1884  "I Was Born To Love You"                                                                               
    ## 705   "All Is Full of Love"                                                                                  
    ## 921   "Monkberry Moon Delight - Remastered 2012"                                                             
    ## 261   "Come Rain or Come Shine"                                                                              
    ## 950   "Enas Mythos"                                                                                          
    ## 927   "Dancing"                                                                                              
    ## 595   "Variationen über 10 Volksweisen, Op.107 - für Violine und Fortepiano: 6. Peggy's daughter (Walisisch)"
    ## 561   "Nada Es Para Siempre"                                                                                 
    ## 317   "Truth Be Known"                                                                                       
    ## 1712  "El Telefono"                                                                                          
    ## 1325  "Duerme - Acústico"                                                                                    
    ## 1976  "Guns for Hands"                                                                                       
    ## 2478  "The Girl With The Midnight Hair"                                                                      
    ## 1939  "El Reino del Tiempo"                                                                                  
    ## 2134  "Vrexi Stin Ftoxo Gitonia"                                                                             
    ## 1681  "The Bay"                                                                                              
    ## 1084  "Strangeness And Charm"                                                                                
    ## 576   "Every Little Thing She Does Is Magic"                                                                 
    ## 1210  "Until the Sun Needs to Rise"                                                                          
    ## 219   "LAMBO LIFE"                                                                                           
    ## 934   "Smooth Dancer"                                                                                        
    ## 177   "Cosi fan tutte, K. 588 (Sung in German): Act II: Holl' und Tod! (Ferrando, Guglielmo)"                
    ## 2076  "5 Pieces for Musical Clock, WoO 33: 4. Allegro. Allegro non più molto in C Major"                     
    ## 1797  "Give Me Your Love"                                                                                    
    ## 1104  "Wachet auf, ruft uns die Stimme, BWV 140: VI. Mein Freund ist mein! Und ich bin sein! (Aria)"         
    ## 2177  "Palmares 1999 - Ao Vivo"                                                                              
    ## 1732  "Made For You"                                                                                         
    ## 2514  "Goldberg Variations, BWV 988 (Arr. F. Meïmoun for String Quartet): Var. 13"                           
    ## 1993  "Cuanto Poder"                                                                                         
    ## 637   "New York City - Remastered 2010"                                                                      
    ## 1947  "Nature Boy"                                                                                           
    ## 1509  "So Close (Unplugged)"                                                                                 
    ## 1058  "Innocent - Instrumental With Background Vocals"                                                       
    ## 2001  "Con Melodía de Adolescente"                                                                           
    ## 59    "Talk More Talk - 1993 Digital Remaster"                                                               
    ## 104   "See You (Ao Vivo)"                                                                                    
    ## 2289  "<U+7A7A><U+60F3><U+30C8><U+30E9><U+30A4><U+30A2><U+30F3><U+30B0><U+30EB>"
