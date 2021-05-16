H<- Hoja_de_cálculo_sin_título_2_
datos<-H$numero
datos<-as.numeric(H$numero)
datos
Multi<-datos*datos
Multi
datos<- 1/Multi
datos.ts<-ts(datos, start = 1971, freq=12)
datos.ts
plot(datos.ts)
plot(acf(datos.ts, type="correlation", plot=T, lag.max = 50))

serie1<-diff(datos.ts)
plot(serie1)
plot(acf(serie1, type="correlation", plot=T, lag.max = 50))

serie2<-diff(serie1)
plot(serie2)
plot(acf(serie2, type="correlation", plot=T))

serie3<-diff(serie2)
plot(serie3)
plot(acf(serie3, type="correlation", plot=T))

var(datos.ts)
var(serie1)
var(serie2)
var(serie3)

library(tseries)
adf.test(datos.ts)
adf.test(serie1)
adf.test(serie2)
adf.test(serie3)

durango<-Durango_Confirmados$DURANGO
durango.ts<-ts(durango, start = 2020)
durango.ts = durango.ts+1
durango.ts
plot(durango.ts)
plot(acf(durango.ts, type="correlation", plot=T, lag.max = 50))

durango1<-log(durango.ts)
durango1
plot(durango1)
plot(acf(durango1, type="correlation", plot=T, lag.max = 100))

durango2<-1/(durango.ts*durango.ts)
durango2
plot(durango2)
plot(acf(durango2, type="correlation", plot=T, lag.max = 100))

durango3<-diff(durango2)
plot(durango3)
plot(acf(durango3, type="correlation", plot=T, lag.max = 50))

durango4<-diff(durango3)
plot(durango4)
plot(acf(durango4, type="correlation", plot=T, lag.max = 50))

library(tseries)
adf.test(durango2)
adf.test(durango3)
adf.test(durango4)
adf.test(serie3)

var(durango2)
var(durango3)
var(durango4)


length(durango2)
d1<-durango2[1:50]
d2<-durango2[51:100]
d3<-durango2[101:150]
d4<-durango2[151:200]
d5<-durango2[201:250]
d6<-durango2[251:300]
d7<-durango2[301:350]
d8<-durango2[351:400]
d9<-durango2[401:450]
d10<-durango2[451:500]
length(d1)
length(d2)
length(d3)
length(d4)
length(d5)
length(d6)
length(d7)
length(d8)
length(d9)
length(d10)

var(d1)
var(d2)
var(d3)
var(d4)
var(d5)
var(d6)
var(d7)
var(d8)
var(d9)
var(d10)


