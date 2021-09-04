


library("DataEditR")
library(readxl)
library("tidyverse")
library("cluster")
library("factoextra")
library("NbClust")
library("vegan")
library("textshape")
library("tibble")
library("tidyverse")
library(Rilostat)
library(ggpubr)
library(ggplot2)





##############################################################################################################
############################################## metodo simplificado ########################################################


## extremos los datos de empleos por sexo y ocupacion economica, con los parametros 
## peru- que el año minimo sea del 2010 y el maximo del 2020, tambien que solo extraiga de ambos sexos y el type
## sea "both" para que venga bien datallado

dat <- get_ilostat(id = c("EMP_TEMP_SEX_EC2_NB_A"), 
                   filters = list(	ref_area = "PER",
                                   timefrom='2010',
                                   timeto='2020',
                                   sex = c("T")),
                   lang='es',
                   detail='full',
                   type='both'
                   
)

##convertimos nuestro extracto "dat" en un dataframe "d"
d<- as.data.frame(dat) 

view(d)
#creamos el arreglo de columnas que no usareamos y lo eliminaremos
borrar <- c("ref_area.label", "source", "source.label", "note_source","note_source.label","indicator","note_indicator","note_indicator.label", "indicator.label","obs_status","obs_status.label", "source", "source.label", "sex.label", "classif1")
d<- d[, !(names(d)%in% borrar)]

############ hacemos la limpieza de datos en el compo "classif1.label"
d$classif1.label<- str_replace(d$classif1.label,"Actividad económica","")
d$classif1.label<- str_replace(d$classif1.label,"CIIU-Rev.4","")
d$classif1.label<- str_replace(d$classif1.label,"a nivel de 2 dígitos:","")
d$classif1.label<- str_replace(d$classif1.label,",","")
d$classif1.label<- str_remove(d$classif1.label, "[(]")
d$classif1.label<- str_remove(d$classif1.label, "[)]")
d$classif1.label<- str_remove(d$classif1.label, "[-]")
d$classif1.label<- str_remove(d$classif1.label, "[0123456789]")
d$classif1.label<- str_remove(d$classif1.label, "[0123456789]")
d$classif1.label<-str_trim(d$classif1.label)


################################### borrramos los campos "ref_area" y "sex"
borrar <- c("ref_area", "sex")
d<- d[, !(names(d)%in% borrar)]

#################################### eliminamos los valores nulos de nuestro dataframe "d"
d<- na.omit(d)
view(d)
########################################################################graficamos las actividades##########################


#### creamos un dataframe "grafica" el cual recibirá los valores filtrados por nombre que esta en la casilla [1,1]
grafica<-d[d$classif1.label==d[49,1],]
view(grafica)
#### se ejecuta el plop con los parametros, titulo, subitutlo y el tipo de grafico
plot(grafica$time,grafica$obs_value, type = "l",
     main = d[49,1],
     col="red",
     fg="red",
     xlab = "Año",
     sub = "Hombres y mujeres",
     ylab = "Cantidad en miles")       


####opcional para abrir una ventana extra
windows()
###############################################Separamos todo por años para hallar su crecimiento####
###### creamos y guarfamos el dataframe j con el fitro "2015", obetenemos las actividades, y sus valores
###### de cierto año en especifico, y asi sucesivamente con los otros años
view(d)
j<-d[d$time=="2015",]
a<-d[d$time=="2016",]
i<-d[d$time=="2017",]
m<-d[d$time=="2018",]
e<-d[d$time=="2019",]
n<-d[d$time=="2020",]

################## renombramos los titulos de "obs_value" al año que corresponde
names(j)[names(j)=="obs_value"]="2015"
names(a)[names(a)=="obs_value"]="2016"
names(i)[names(i)=="obs_value"]="2017"
names(m)[names(m)=="obs_value"]="2018"
names(e)[names(e)=="obs_value"]="2019"
names(n)[names(n)=="obs_value"]="2020"


################ Limpiamos los "time" para una mejor vista
borrar <- c("ref_area", "sex", "time")
j<- j[, !(names(j)%in% borrar)]
borrar <- c("ref_area", "sex", "time")
a<- a[, !(names(a)%in% borrar)]
borrar <- c("ref_area", "sex", "time")
i<- i[, !(names(i)%in% borrar)]
borrar <- c("ref_area", "sex", "time")
m<- m[, !(names(m)%in% borrar)]
borrar <- c("ref_area", "sex", "time")
e<- e[, !(names(e)%in% borrar)]
borrar <- c("ref_area", "sex", "time")
n<- n[, !(names(n)%in% borrar)]


################# juntamos las tablas filtradas por años
df01<-full_join(j, a, by="classif1.label")
df02<-full_join(df01,i, by="classif1.label")
df03<-full_join(df02,m, by="classif1.label")
df04<-full_join(df03,e, by="classif1.label")
df05<-full_join(df04,n, by="classif1.label")

#################### borramos el campo "time" ya no es util
borrar <- c("time")
df05<- df05[, !(names(df05)%in% borrar)]

view(df05)
############################3 eliminamos los valores nulos para evitar las divisiones sobre 0
df05<- na.omit(df05)

############# añadimos los compos variaciones, para el ver el porcenatje de aumento o disminucion años tras años
df05$variacion2016=round((df05$`2016`/df05$`2015`)*100-100,2)
df05$variacion2017=round((df05$`2017`/df05$`2016`)*100-100,2)
df05$variacion2018=round((df05$`2018`/df05$`2017`)*100-100,2)
df05$variacion2019=round((df05$`2019`/df05$`2018`)*100-100,2)
df05$variacion2020=round((df05$`2020`/df05$`2019`)*100-100,2)

############## añadimos el campo total, es una comparacion del 2015 al 2020, para ver el crecimiento o
############## dimisnucion en porcentaje de los empleios en estos 6 ultimos años
df05$total=round((df05$`2020`/df05$`2015`)*100-100,2)

############################# acabamos con una vista global del crecimiento o disminucion de empleos
view(df05)



##################################################################################### para mujeres##########################
#######################################################################################################################
###########################################################################################################################
#####################################################################################################################3

datMujer <- get_ilostat(id = c("EMP_TEMP_SEX_EC2_NB_A"), 
                        filters = list(	ref_area = "PER",
                                        
                                        timefrom='2010',
                                        timeto='2020',
                                        sex = c("SEX_F")),
                        lang='es',
                        detail='full',
                        type='both'
                        
)





mujer<- as.data.frame(datMujer) 


#creamos el arreglo de columnas a ignorar en el analisis
borrar <- c("ref_area.label", "source", "source.label", "note_source","note_source.label","indicator","note_indicator","note_indicator.label", "indicator.label","obs_status","obs_status.label", "source", "source.label", "sex.label", "classif1")
mujer<- mujer[, !(names(mujer)%in% borrar)]

view(mujer)
mujer$classif1.label<- str_replace(mujer$classif1.label,"Actividad económica","")
mujer$classif1.label<- str_replace(mujer$classif1.label,"CIIU-Rev.4","")
mujer$classif1.label<- str_replace(mujer$classif1.label,"a nivel de 2 dígitos:","")
mujer$classif1.label<- str_replace(mujer$classif1.label,",","")
mujer$classif1.label<- str_remove(mujer$classif1.label, "[(]")
mujer$classif1.label<- str_remove(mujer$classif1.label, "[)]")
mujer$classif1.label<- str_remove(mujer$classif1.label, "[-]")
mujer$classif1.label<- str_remove(mujer$classif1.label, "[0123456789]")
mujer$classif1.label<- str_remove(mujer$classif1.label, "[0123456789]")
mujer$classif1.label<-str_trim(mujer$classif1.label)



borrar <- c("ref_area", "sex")
mujer<- mujer[, !(names(mujer)%in% borrar)]
mujer<- na.omit(mujer)
view(mujer)
########################################################################graficamos las actividades##########################

graficaM<-mujer[mujer$classif1.label==mujer[39,1],]
view(graficaM)
plot(graficaM$time,graficaM$obs_value, type = "l",
     main = mujer[39,1],
     col="red",
     fg="red",
     xlab = "Año",
     sub="Mujeres",
     ylab = "Cantidad en miles")             

windows()
######################################################################Separamos todo por años para hallar su crecimiento####
view(mujer)
jm<-mujer[mujer$time=="2015",]
am<-mujer[mujer$time=="2016",]
im<-mujer[mujer$time=="2017",]
mm<-mujer[mujer$time=="2018",]
em<-mujer[mujer$time=="2019",]
nm<-mujer[mujer$time=="2020",]


names(jm)[names(jm)=="obs_value"]="2015"
names(am)[names(am)=="obs_value"]="2016"
names(im)[names(im)=="obs_value"]="2017"
names(mm)[names(mm)=="obs_value"]="2018"
names(em)[names(em)=="obs_value"]="2019"
names(nm)[names(nm)=="obs_value"]="2020"


####################################################no
borrar <- c("ref_area", "sex", "time")
jm<- jm[, !(names(jm)%in% borrar)]
borrar <- c("ref_area", "sex", "time")
am<- am[, !(names(am)%in% borrar)]
borrar <- c("ref_area", "sex", "time")
im<- im[, !(names(im)%in% borrar)]
borrar <- c("ref_area", "sex", "time")
mm<- mm[, !(names(mm)%in% borrar)]
borrar <- c("ref_area", "sex", "time")
em<- em[, !(names(em)%in% borrar)]
borrar <- c("ref_area", "sex", "time")
nm<- nm[, !(names(nm)%in% borrar)]


df01m<-full_join(jm, am, by="classif1.label")
df02m<-full_join(df01m,im, by="classif1.label")
df03m<-full_join(df02m,mm, by="classif1.label")
df04m<-full_join(df03m,em, by="classif1.label")
df05m<-full_join(df04m,nm, by="classif1.label")

borrar <- c("time")
df05m<- df05m[, !(names(df05m)%in% borrar)]

view(df05m)

df05m<- na.omit(df05m)


df05m$variacion2016=round((df05m$`2016`/df05m$`2015`)*100-100,2)
df05m$variacion2017=round((df05m$`2017`/df05m$`2016`)*100-100,2)
df05m$variacion2018=round((df05m$`2018`/df05m$`2017`)*100-100,2)
df05m$variacion2019=round((df05m$`2019`/df05m$`2018`)*100-100,2)
df05m$variacion2020=round((df05m$`2020`/df05m$`2019`)*100-100,2)
df05m$total=round((df05m$`2020`/df05m$`2015`)*100-100,2)
view(df05m)






##################################################################################### para hombre##########################
#######################################################################################################################
###########################################################################################################################
#####################################################################################################################3
dathombre <- get_ilostat(id = c("EMP_TEMP_SEX_EC2_NB_A"), 
                         filters = list(	ref_area = "PER",
                                         
                                         timefrom='2010',
                                         timeto='2020',
                                         sex = c("M")),
                         lang='es',
                         detail='full',
                         type='both'
                         
)





dfhombre<- as.data.frame(dathombre) 

#creamos el arreglo de columnas a ignorar en el analisis
borrar <- c("ref_area.label", "source", "source.label", "note_source","note_source.label","indicator","note_indicator","note_indicator.label", "indicator.label","obs_status","obs_status.label", "source", "source.label", "sex.label", "classif1")
dfhombre<- dfhombre[, !(names(dfhombre)%in% borrar)]


dfhombre$classif1.label<- str_replace(dfhombre$classif1.label,"Actividad económica","")
dfhombre$classif1.label<- str_replace(dfhombre$classif1.label,"CIIU-Rev.4","")
dfhombre$classif1.label<- str_replace(dfhombre$classif1.label,"a nivel de 2 dígitos:","")
dfhombre$classif1.label<- str_replace(dfhombre$classif1.label,",","")
dfhombre$classif1.label<- str_remove(dfhombre$classif1.label, "[(]")
dfhombre$classif1.label<- str_remove(dfhombre$classif1.label, "[)]")
dfhombre$classif1.label<- str_remove(dfhombre$classif1.label, "[-]")
dfhombre$classif1.label<- str_remove(dfhombre$classif1.label, "[0123456789]")
dfhombre$classif1.label<- str_remove(dfhombre$classif1.label, "[0123456789]")
dfhombre$classif1.label<-str_trim(dfhombre$classif1.label)


borrar <- c("ref_area", "sex")
dfhombre<- dfhombre[, !(names(dfhombre)%in% borrar)]
dfhombre<- na.omit(dfhombre)
view(dfhombre)
########################################################################graficamos las actividades##########################

graficaHombre<-dfhombre[dfhombre$classif1.label==dfhombre[49,1],]
view(grafica)
plot(graficaHombre$time,graficaHombre$obs_value, type = "l",
     main = dfhombre[49,1],
     col="red",
     fg="red",
     xlab = "Año",
     sub = "Hombres",
     ylab = "Cantidad en miles")             

windows()
######################################################################Separamos todo por años para hallar su crecimiento####
view(dfhombre)
jh<-dfhombre[dfhombre$time=="2015",]
ah<-dfhombre[dfhombre$time=="2016",]
ih<-dfhombre[dfhombre$time=="2017",]
mh<-dfhombre[dfhombre$time=="2018",]
eh<-dfhombre[dfhombre$time=="2019",]
nh<-dfhombre[dfhombre$time=="2020",]


names(jh)[names(jh)=="obs_value"]="2015"
names(ah)[names(ah)=="obs_value"]="2016"
names(ih)[names(ih)=="obs_value"]="2017"
names(mh)[names(mh)=="obs_value"]="2018"
names(eh)[names(eh)=="obs_value"]="2019"
names(nh)[names(nh)=="obs_value"]="2020"

borrar <- c("ref_area", "sex", "time")
jh<- jh[, !(names(jh)%in% borrar)]
borrar <- c("ref_area", "sex", "time")
ah<- ah[, !(names(ah)%in% borrar)]
borrar <- c("ref_area", "sex", "time")
ih<- ih[, !(names(ih)%in% borrar)]
borrar <- c("ref_area", "sex", "time")
mh<- mh[, !(names(mh)%in% borrar)]
borrar <- c("ref_area", "sex", "time")
eh<- eh[, !(names(eh)%in% borrar)]
borrar <- c("ref_area", "sex", "time")
nh<- nh[, !(names(nh)%in% borrar)]




df01h<-full_join(jh, ah, by="classif1.label")
df02h<-full_join(df01h,ih, by="classif1.label")
df03h<-full_join(df02h,mh, by="classif1.label")
df04h<-full_join(df03h,eh, by="classif1.label")
df05h<-full_join(df04h,nh, by="classif1.label")

borrar <- c("time")
df05h<- df05h[, !(names(df05h)%in% borrar)]

view(df05h)

df05h<- na.omit(df05h)


df05h$variacion2016=round((df05h$`2016`/df05h$`2015`)*100-100,2)
df05h$variacion2017=round((df05h$`2017`/df05h$`2016`)*100-100,2)
df05h$variacion2018=round((df05h$`2018`/df05h$`2017`)*100-100,2)
df05h$variacion2019=round((df05h$`2019`/df05h$`2018`)*100-100,2)
df05h$variacion2020=round((df05h$`2020`/df05h$`2019`)*100-100,2)
df05h$total=round((df05h$`2020`/df05h$`2015`)*100-100,2)
view(df05)










Conf2mas1 = matrix(c(2,1,3,1), nrow=2, byrow=F) # Creamos un matriz a partir de un vector con los valores c(1:3,3) que es igual que c(1,2,3,3)
Conf2mas1
layout(Conf2mas1)
layout.show(3)





###### hombres y mujeres
view(d)
grafica<-d[d$classif1.label==d[49,1],]
plot(grafica$time,grafica$obs_value, type = "l",
     main = d[49,1],
     col="red",
     fg="red",
     xlab = "Año",
     sub = "Hombres y mujeres",
     ylab = "Cantidad en miles")       



#######Mujeres
view(mujer)
graficaM<-mujer[mujer$classif1.label==mujer[39,1],]
plot(graficaM$time,graficaM$obs_value, type = "l",
     main = mujer[39,1],
     col="red",
     fg="red",
     xlab = "Año",
     sub="Mujeres",
     ylab = "Cantidad en miles")         



######hombre
view(dfhombre)
graficaHombre<-dfhombre[dfhombre$classif1.label==dfhombre[49,1],]
plot(graficaHombre$time,graficaHombre$obs_value, type = "l",
     main = dfhombre[49,1],
     col="red",
     fg="red",
     xlab = "Año",
     sub = "Hombres",
     ylab = "Cantidad en miles")   
