# las distribución normal se asocia principalmente a medidas de ingenieria, 
# mecanica, materiales y biológica

mu <- 95.3
sigma <- 5.7

# N <- N(mu, sigma^2)

curve(dnorm(x, mean=mu, sd=sigma), xlim=c(80,120), col="red") 
# dnorm es funcion de densidad


# APARTADO a)

# set.seed(semilla)
rnorm(4, mean=mu, sd=sigma)
# el rnom coge una muestra aleatoria de 4 cables, en este caso, nos coge 
# resistencias aleatorias de 4 cables, porque si no ponemo semilla, cada vez 
# nos da 4 resistencias diferentes, lo que quiere decir que son 4 experimentos
# aleatorios

# un ESTADÍSTICO es una variable aleatoria que se forma a partir de cualquier 
# función de la muestra, en este caso, Y será el estadístico que se define
# a partir de la suma de de las rnorm

Y <- function(i){sum(rnorm(4, mean=mu, sd=sigma))}
Y(1) # repito 1 vez el experimento Y, como es aleatorio, nos da un valor 
# diferente cada vez

Y100000 <- sapply(1:10000, Y) # repito Y 100000 veces
mean(Y100000) # el valor medio convergerá 

# TEOREMA SUMA MUESTRAL. Si X -> N(mu, sigma^2), sum(Xi)=Y donde 
# Y -> N(n*mu, n*sigma^2) y n es el número de veces que se repite, 
# el experimento aleatorio, osea la cantidad de muestras

hist(Y100000, freq=FALSE)
curve(dnorm(x, 4*mu, 2*sigma), col="red", add=TRUE) 
# para ver que converge a la media a lo que nos dice el teorema

# por tanto, la respuesta es simplemente 4*mu


# APARTADO b)

# Por el teorema sabemos que la varianza es 100*sigma^2.


# APARTADO c)

1-pnorm(103, mean=95.3, sd=5.7)


# APARTADO d)

# TEOREMA DE LA MEDIA MUESTRAL. ahora la variable aleatoria nueva, X\, es la 
# media muestral, cuya esperanza, como hemos visto en teoría es mu, y la 
# varianza es (sigma^2)/n

# aún sabiendo el teorema, vamos a comprobarlo cuando repito muchas veces la
# variable aleatoria de la media

xbar <- function(i){mean(rnorm(4, mean=mu, sd=sigma))}
xbar100000 <- sapply(1:100000, xbar)

hist(xbar100000)
mean(xbar100000<98) # frecuencia relativa del vector TRUE FALSE de 
# que sea mayor a 98

# cuando repito el experimento de coger 4 cables y calcular 
# la media de las resistencia sde la muestra infinitas veces, la media de los 
# resultados tiende a la esperanza

hist(xbar100000, freq=FALSE)
curve(dnorm(x,mu,sigma/sqrt(4)),add=TRUE, col="red")
pnorm(98, mu, sigma/sqrt(4)) # coinciden los resultados


# APARTADO e)

# TEOREMA DE LA VARIANZA MUESTRAL. ahora la variable aleatoria nueva es S^2, 
# pero como tiene sesgo, lo corrregimos aplicando W, que es la variable 
# aleatoria que se distribuye como una chi^2 de Pearson con n-1 grados de 
# libertad. W=(n-1)*S^2/sigma^2

ssq <- function(i){var(rnorm(100, mean=mu, sd=sigma))}
ssq100000 <- sapply(1:100000, ssq)

hist(ssq100000, freq=FALSE)
mean(ssq100000>98)

hist(ssq100000*(100-1)/sigma^2, freq=FALSE)
curve(dchisq(x,100-1),add=TRUE, col="red")
1-pchisq(98, 100-1) # coinciden los resultados


