Proyecto N°1 Minería de Datos
================
Tomás Reyes, Juan Lisboa
26-03-2021

### Librerías utilizadas

Para este proyecto se utilizó la librería “tidiverse”, ya que
facilitaría de beuna manera la visualización del procesamiento de los
datos.

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.3     v purrr   0.3.4
    ## v tibble  3.1.0     v dplyr   1.0.5
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

### Cargando los datos

Lo primero que se debe hacer corresponde a cargar los datos de estudio
en el proyecto R, para que posteriormente puedan ser utilizados, esto se
realizó con el comando read.csv(), y luego se almacenó la tabla obtenida
en el Data Frame **data**. También se obtiene un resumen de este nuevo
Data Frame con el fin de conocer qué tipo de variables este contiene.

``` r
data <- read.csv("sanguchez.csv", sep = ";", header = TRUE)

summary(data)
```

    ##      url               Local            Direccion            Precio         
    ##  Length:410         Length:410         Length:410         Length:410        
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##  Ingredientes            nota          texto          
    ##  Length:410         Min.   :1.000   Length:410        
    ##  Class :character   1st Qu.:3.000   Class :character  
    ##  Mode  :character   Median :3.000   Mode  :character  
    ##                     Mean   :3.167                     
    ##                     3rd Qu.:4.000                     
    ##                     Max.   :5.000                     
    ##                     NA's   :8

### Limpieza y preparación de datos

A continuación, corresponde eliminar aquellas columnas de datos que no
aportarán información pertinente al estudio. Antes de esto, sin embargo,
se debe crear un nuevo Data Frame para guardar los datos y así no
afectar a los originales, en caso de necesitar un respaldo. Así, se crea
un nuevo Data Frame llamado **sandwich**, que inicialmente corresponde a
una copia del original. Luego, se eliminan las columnas **url**, ya que
la dirección en internet de la crítica al sandwich no nos aporta ningún
valor ni información, **local**, ya que se considera que un sandwich
debe ser ideal, sin importan dónde sea hecho, **dirección**, por el
mismo motivo recién expuesto, y finalmente la varibale **texto**,
correspondiente a los comentarios de quien realiza la crítica. Esta
última varibale es considerada como una que aporta valor, sin embargo,
quienes escriben no poseen el conocimiento requerido para procesarla,
debido a la alta complejidad de esta tarea.

``` r
sandwich <- data
sandwich$url <- NULL
sandwich$Local <- NULL
sandwich$Direccion <- NULL
sandwich$texto <- NULL
```

Luego de haber estandarizado los ingredientes de los sandwiches, es
necesario eliminar aquellos que tengan alguno de sus datos incompletos,
ya que si esto ocurre, no podrá ser utilizado en el estudio posterior,
para esto se hizo una copia de los datos sin icluir aquellos
incompletos, utilizando el método **na.omit()**, la cual fue almacenada
en un nuevo Data Frame llamdo **sandwich2**.

``` r
sandwich2 <- na.omit(sandwich)
```

El siguiente paso corresponde a normalizar los datos restantes, esto es,
dejarlos todos en el mismo formato. Para esto se utilizó el método
**tolower()**, que lleva todos los carácteres del valor de input a
minúsculas. Por último, se imprime la dimensión del Data Frame
resultante, para tener una idea acerca de la contidad de filas
restantes.

``` r
sandwich2$Ingredientes <- tolower(sandwich2$Ingredientes)

dim(sandwich2)
```

    ## [1] 402   3

Luego, se realiza una limpieza de caracteres no deseados en el precio de
los sandwiches, que hasta este punto aún se creía como una fuente de
infromación relevante. Los carácteres eliminados corresponden a letras
**\[a-zA-Z\]**, signo peso **$**, puntos **.** y espacios de texto
vacíos. Todos estos fueron removidos, utilizando el método **gsub**. Por
último, se imprimió por pantalla la dimensión del Data Frame resultante
para verificar la integridad de los datos.

``` r
sandwich2$Precio = gsub("[a-zA-Z]", "", sandwich2$Precio)
sandwich2$Precio = gsub("\\$", "", sandwich2$Precio)
sandwich2$Precio = gsub("\\.", "", sandwich2$Precio)
sandwich2$Precio = gsub(" ", "", sandwich2$Precio)
dim(sandwich2)
```

    ## [1] 402   3

Luego de haber removido los carácteres especiales del precio, se
prosiguió a convertirlo de tipo de dato **character** a un **int**, con
los cuales se podrían realizar operaciones matemáticas.

``` r
sandwich2 <- transform(sandwich2, Precio = as.numeric(Precio))
```

    ## Warning in eval(substitute(list(...)), `_data`, parent.frame()): NAs
    ## introducidos por coerción

Luego de haber realizado la limpieza y estandarización de los precios de
los sandwiches, correpondía hacer lo mismo con la lista de ingredientes
de cada una de estas filas o sandwiches. Para esto se hizo una copia de
los datos, almacenada en el nuevo Data Frame **sandwich3**, y se imprime
por pantalla un resumen de este para verificar que los datos no han
sufrido modificaciones indeseadas.

``` r
sandwich3 <- sandwich2 

summary(sandwich3)
```

    ##      Precio         Ingredientes            nota      
    ##  Min.   :       7   Length:402         Min.   :1.000  
    ##  1st Qu.:    4460   Class :character   1st Qu.:3.000  
    ##  Median :    5900   Mode  :character   Median :3.000  
    ##  Mean   :   94179                      Mean   :3.167  
    ##  3rd Qu.:    6990                      3rd Qu.:4.000  
    ##  Max.   :35005500                      Max.   :5.000  
    ##  NA's   :7

Para eliminar carácteres indeseados de las listas de ingredientes, y
poder separarlos para su posterior análisis individual, se utilizó el
método **gsub()** anteriormente mencionado. Los carácteres removidos
correpsonden a **y** y comas sin espacios laterales, ambos remmplazados
por **“,”**, mientras que símbolos de paréntesis, números, puntos y
símbolos de porcentaje, fueron removidos.

``` r
sandwich3$Ingredientes = gsub(" y ", ", ", sandwich3$Ingredientes)
sandwich3$Ingredientes = gsub(",", ", ", sandwich3$Ingredientes)
sandwich3$Ingredientes = gsub(" \\(", ", ", sandwich3$Ingredientes)
sandwich3$Ingredientes = gsub("\\) ", ", ",sandwich3$Ingredientes)
sandwich3$Ingredientes = gsub("\\)", ", ", sandwich3$Ingredientes)
sandwich3$Ingredientes = gsub("[0-9]", "", sandwich3$Ingredientes)
sandwich3$Ingredientes = gsub("\\.", "", sandwich3$Ingredientes)
sandwich3$Ingredientes = gsub("\\%", "", sandwich3$Ingredientes)
```

Luego de haber removido todos los carácteres no deseados de las listas
de ingredientes de cada sandwich, comienza la preparación de los datos.
Este proceso consiste en la separación de los ingredientes en elementos
atómicos, cada ingrediente corresponde a un elemento en una lista. Para
esto, se utilizó el método **strsplit()**, el cual permite separar un
string de carácteres de acuerdo a un separador que debe ser
especificado, el cual en este caso corresponde a una coma seguida de un
espacio, **“,”**. Luego, se imprime por pantalla una vista resumida del
Data Frame resultante, con el comando **glimpse()**, para poder
verificar la correcta separación de las lsiats de ingredientes.

``` r
sandwich3$ingredientes_split <- NULL
sandwich3$ingredientes_split <- strsplit(sandwich3$Ingredientes, ", ")
glimpse(sandwich3)
```

    ## Rows: 402
    ## Columns: 4
    ## $ Precio             <dbl> 5210, 7000, 7290, 8690, 4900, 6500, 5500, 8000, 890~
    ## $ Ingredientes       <chr> "suprema de pollo dulce,  espinaca,  crema ácida,  ~
    ## $ nota               <int> 3, 3, 4, 4, 4, 3, 3, 3, 3, 3, 4, 3, 3, 2, 4, 2, 4, ~
    ## $ ingredientes_split <list> <"suprema de pollo dulce", " espinaca", " crema ác~

A continuación, se crean 3 listas de datos, una que contiene a todos los
ingredientes llamada **ingredientes\_solos**, otra con sus respectivas
notas llamada **notas**, y una última en la que más adelante se
guardarán los promedios de notas de cada ingrediente llamada
**promedios**.

``` r
contador_listas = 0
contador = 1 
notas <- 0
ingredientes_solos <- 0
promedios <- 0
list(notas)
list(ingredientes_solos)
list(promedios)

for(i in sandwich3$ingredientes_split) {  # i itera sobre las listas de ingredientes separados de cada sandwich
  
  contador_listas = contador_listas + 1
  nota_i <- sandwich3$nota[contador_listas]
  
  for (ing in unlist(i)) {                # ing itera sobre los ingredientes de cada lista i
    
    notas[contador] <- nota_i
    ingredientes_solos[contador] <- ing
    promedios[contador] = 0
    
    contador = contador + 1
    
  }
  
}
```

Una vez obtenidas las listas mencionadas anteriormente, se deben unir en
un solo Data Frame, para poder realizar cálculos con mayor facilidad. En
este caso se almacenaron en un Data Frame llamado
**ingredientes\_nota**, y a continuación se eliminaron los espacios de
texto vacíos de cada ingrediente.

``` r
ingredientes_nota <- data.frame(ingredientes_solos, notas, promedios)
ingredientes_nota$ingredientes_solos = gsub("  ", "", ingredientes_nota$ingredientes_solos)
```

### Procesamiento de datos

El siguiente paso corresponde a obtener el promedio de notas de cada
ingrediente, lo cual fue realizado con la ayuda de un Data Frame
auxiliar llamadp **aux**, en el cual se guardan todas las filas de un
mismo ingrediente con sus respectivas notas, y se calcula su promedio
para luego guardarlo en la varibale **promedios** del Data Frame
**ingredientes\_nota** para su uso futuro.

``` r
contador = 1

for (ingrediente in ingredientes_nota$ingredientes_solos) {
  
  aux <- ingredientes_nota %>% filter(ingredientes_nota$ingredientes_solos == ingrediente)  #crear un subset temporal del ingrediente en el que estoy
  
  aux$promedios <-round(mean(aux$notas), 2)                       #calcular promedio de nota de ese ingrediente

  
  ingredientes_nota$promedios[contador] =  unique(aux$promedios) #guardar el promedio en dataframe de ingredientes
  contador = contador + 1
}
```

Ahora prosigue eliminar los ingredientes duplicados, ya que ahora todos
los ingredientes iguales tienen la misma nota promedio. Este nuevo Data
Frame fue llamado **ingredientes\_unicos**, y contiene todos los
ingredientes úncios con su respectiva nota proemdio.

``` r
ingredientes_unicos <- unique.data.frame(ingredientes_nota)
```

Luego, debemos determinar la cantidad de ingredientes promedio de los
mejores sandwiches, que serán considerados como aquellos cuya nota de
calificación sea igual a 5. Estos se guardan en un nuevo Data Frame
llamado **mejores\_sandwiches** y se guarda la cantidad de ingredientes
de cada uno en una nueva lista llamada **n\_ingredientes**.

``` r
glimpse(sandwich3)
```

    ## Rows: 402
    ## Columns: 4
    ## $ Precio             <dbl> 5210, 7000, 7290, 8690, 4900, 6500, 5500, 8000, 890~
    ## $ Ingredientes       <chr> "suprema de pollo dulce,  espinaca,  crema ácida,  ~
    ## $ nota               <int> 3, 3, 4, 4, 4, 3, 3, 3, 3, 3, 4, 3, 3, 2, 4, 2, 4, ~
    ## $ ingredientes_split <list> <"suprema de pollo dulce", " espinaca", " crema ác~

``` r
mejores_sandwiches <- sandwich3 %>% filter(sandwich3$nota == 5)

n_ingredientes <- 0
list(n_ingredientes)
```

    ## [[1]]
    ## [1] 0

``` r
contador = 1


for (i in mejores_sandwiches$ingredientes_split) {    # i recorre las listas de ingredientes de los mejores sandwiches
  n_ingredientes[contador] <- as.numeric(length(i))   # se guarda la cantidad de ingredientes de cada sandwich en una lista
  contador = contador + 1
}
```

Luego se obtiene el promedio de ingredientes de los mejores sandwiches
seleccionados, que es lo mismo que el promedio de los elementos de la
lista **n\_ingredientes**, y se guarda en una nueva variable denominada
**promedio\_ingredientes**.

``` r
promedio_ingredientes = mean(n_ingredientes)
promedio_ingredientes
```

    ## [1] 5.803571

Ahora sabemos que los mejores sandwiches tienen en promedio 5,8
ingredientes, lo que resulta en sandwiches de 6 ingredientes, ya que son
variables discretas.Por otro lado, es necesario escoger los mejores
ingredientes, en este caso se considerará como de los mejores a aquellos
ingredientes con una nota promedio igual o mayor a 4.5 y serán guardados
en un nuevo Data Frame llamado **ingredientes\_elegidos**.

``` r
ingredientes_elegidos <- ingredientes_nota %>% filter(promedios >= 4.5)
```

También es necesario tener en consideración que un sandwich debe incluir
algún tipo de pan, por lo que para asegurar que esto ocurra se crea otro
Data Frame denominado **pan**, en donde se almacena una copia de los
ingredientes elegidos que contengan la palabra “pan”.

``` r
pan <- ingredientes_elegidos[grep("pan ", ingredientes_elegidos$ingredientes_solos),]
```

Luego, se ordena el Data Frame **ingredientes\_elegidos** en orden
descendiente respecto a la nota, con ayuda del método **order()**, para
luego escoger los primeros 6 indicados por el método **ceilling()**,
exceptuando el último ya que este será escogido de aquellos ingredientes
en el Data Frame **pan**, específicamente el primero de estos.

``` r
ingredientes_elegidos <- ingredientes_elegidos[order(ingredientes_elegidos$promedios, decreasing = TRUE),]

ingredientes_elegidos <- ingredientes_elegidos[1:ceiling(promedio_ingredientes)-1,]
```

Finalmente, se unen las filas del Data Frame **ingredientes\_elegidos**
con el primer ingrediente del Data Frame **pan**, para obtener aquellos
6 ingredientes que confromarán el sadwich con la mejor calificación de
acuerdo a los datos originalmente entregados.

``` r
ingredientes_elegidos <- rbind.data.frame(ingredientes_elegidos, pan[1,])

print.data.frame(ingredientes_elegidos)
```

    ##                   ingredientes_solos notas promedios
    ## 1                               pato     5         5
    ## 2                  mayonesa habanero     5         5
    ## 3                    queso americano     5         5
    ## 4  tocino glaseado con miel de maple     5         5
    ## 5           alioli con miel de maple     5         5
    ## 7                       pan de leche     5         5
