#paquetes necesarios
library(haven)
library(dplyr)
library(car)
library(Hmisc)
library(zoo)
library(lmtest)
library(sjPlot)

WV6_Stata_v_2016_01_01 <- read_dta("C:/Users/Pablo/Desktop/tarea 1/WV6_Stata_v_2016_01_01.dta") 
attach(WV6_Stata_v_2016_01_01)


pais=c(V2)
sexo=c(V240)


sexo=factor(sexo, levels=c(1,2), labels=c("Hombre", "Mujer"))
table(sexo)
pais=factor(pais, levels= c(32,152,724,752,792), labels=c("argentina","chile","españa","suecia","turquia"))
table(pais)

#Rol_genero4


p1=recode(V45, "1=0.5 ;else=0")
p2=recode(V47, "1=0.5;else=0")
p3=recode(V48, "1=0.5;else=0")
p4=recode(V50, "2=0.5;1=1;else=0")
p5=recode(V51, "2=0.5;1=1;else=0")
p6=recode(V52, "2=0.5;1=1;else=0")
p7=recode(V53, "2=0.5;1=1;else=0")
p8=recode(V54, "2=0.5;1=1;else=0")

rol_genero4=p1+p2+p3+p4+p5+p6+p7+p8

#Crear dataframe y filtrar por pais

junto=data.frame(pais,sexo,rol_genero4)

argentina=filter(junto, pais=="argentina")
chile=filter(junto, pais=="chile")
españa=filter(junto, pais=="españa")
suecia=filter(junto, pais=="suecia")
turquia=filter(junto, pais=="turquia")

#Regresion por pais lm(Y~X)

model_argentina=lm(argentina$rol_genero4~argentina$sexo)
model_chile=lm(chile$rol_genero4~chile$sexo)
model_españa=lm(españa$rol_genero4~españa$sexo)
model_suecia=lm(suecia$rol_genero4~suecia$sexo)
model_turquia=lm(turquia$rol_genero4~turquia$sexo)

model_argentina
model_chile
model_españa
model_suecia
model_turquia

edad=c(V242)
etnico=c(V245)
religion=factor(V25)
casado=factor(V57)
hijos=factor(V58)

dummyreligion=recode(religion, "2=1;else=0")
dummyhijos=recode(hijos, "0=0;1=1;2=2;3=3;4=4;5=5;6=6;7=7;8=8;else=NA")
dummycasado=recode(casado, "1=1;else=0")

junto2=data.frame(pais,edad,etnico,dummyreligion,dummyhijos,dummycasado, rol_genero4)


argentina2=filter(junto2, pais=="argentina")
chile2=filter(junto2, pais=="chile")
españa2=filter(junto2, pais=="españa")
suecia2=filter(junto2, pais=="suecia")
turquia2=filter(junto2, pais=="turquia")

paisesjuntos=rbind(argentina2,chile2,españa2,suecia2,turquia2)

attach(paisesjuntos)

#Regresiones rol_genero4 con respecto a cada variable

model_pais=lm(paisesjuntos$rol_genero4~paisesjuntos$pais)
model_edad=lm(paisesjuntos$rol_genero4~paisesjuntos$edad)
model_etnico=lm(paisesjuntos$rol_genero4~paisesjuntos$etnico)
model_religion=lm(paisesjuntos$rol_genero4~paisesjuntos$dummyreligion)
model_hijos=lm(paisesjuntos$rol_genero4~paisesjuntos$dummyhijos)
model_casado=lm(paisesjuntos$rol_genero4~paisesjuntos$dummycasado)


model_pais
Anova(model_pais)

model_edad
Anova(model_edad)

model_etnico
Anova(model_etnico)

model_religion
Anova(model_religion)

model_hijos
Anova(model_hijos)

model_casado
Anova(model_casado)

#Modelo Regresion multiple

model_rolgenero4=lm(rol_genero4~pais+edad+etnico+dummyreligion+dummyhijos+dummycasado, data=paisesjuntos)
model_rolgenero4
Anova(model_rolgenero4)
summary(model_rolgenero4)

#Modelo Regresion simple para Chile
attach(chile2)

model_edadchile=lm(rol_genero4~edad, data = chile2)
model_etnicochile=lm(rol_genero4~etnico, data = chile2)
model_religionchile=lm(rol_genero4~dummyreligion, data = chile2)
model_hijoschile=lm(rol_genero4~dummyhijos, data = chile2)
model_casadochile=lm(rol_genero4~dummycasado,data = chile2)

model_edadchile
model_etnicochile
model_religionchile
model_hijoschile
model_casadochile

#Modelo Regresion multiple para Chile
model_rolgenero4chile=lm(rol_genero4~edad+etnico+dummyreligion+dummyhijos+dummycasado, data=chile2)
model_rolgenero4chile
Anova(model_rolgenero4chile)
summary(model_rolgenero4chile)

#Heterocedasticidad pregunta 3 (model_rolgenero4)


summary(model_rolgenero4)

bptest(model_rolgenero4)

#Corrección heterocedasticidad

paisesjuntos= na.omit(paisesjuntos)

paisesjuntos$resi=model_rolgenero4$residuals

varfunc.ols= lm(log(resi^2) ~ pais+edad+etnico+dummyreligion+dummyhijos+dummycasado, data = paisesjuntos)

summary(varfunc.ols)

bptest(varfunc.ols)

detach(WV6_Stata_v_2016_01_01)

WV4_Data_stata_v_2015_04_18 <- read_dta("C:/Users/Pablo/Desktop/tarea 1/WV4_Data_stata_v_2015_04_18.dta") 
attach(WV4_Data_stata_v_2015_04_18)

#Rol genero 4 nueva base
g1=recode(V78, "1=0.5 ;else=0")
g2=recode(V119, "2=0.5;1=1;else=0")
g3=recode(V118, "2=0.5;1=1;else=0")
g4=recode(V116, "2=0.5;1=1;else=0")
g5=recode(V109, "3=0.5;4=1;else=0")

rol_genero42=g1+g2+g3+g4+g5

pais2=c(V2)
sexo2=c(V223)
edad2=c(V225)
etnico2=c(V242)
casado2=factor(V106)
hijos2=factor(V107)

sexo2=factor(sexo2, levels=c(1,2), labels=c("Hombre", "Mujer"))
table(sexo2)
pais2=factor(pais2, levels= c(32,152,724,752,792), labels=c("argentina","chile","españa","suecia","turquia"))
table(pais2)

dummyhijos2=recode(hijos2, "0=0;1=1;2=2;3=3;4=4;5=5;6=6;7=7;else=NA")
dummycasado2=recode(casado2, "1=1;else=0")

junto3=data.frame(pais2,sexo2,edad2,etnico2,dummyhijos2,dummycasado2,rol_genero42)

argentina3=filter(junto3, pais2=="argentina")
chile3=filter(junto3, pais2=="chile")
españa3=filter(junto3, pais2=="españa")
suecia3=filter(junto3, pais2=="suecia")
turquia3=filter(junto3, pais2=="turquia")

paisesjuntos2=rbind(argentina3,chile3,españa3,suecia3,turquia3)

attach(paisesjuntos2)

#Regresion multivariada rol_genero4(2)
model_rolgenero42=lm(rol_genero42~pais2+edad2+etnico2+dummyhijos2+dummycasado2, data=paisesjuntos2)
model_rolgenero42
summary(model_rolgenero42)

#tabla de ambos modelos
library(sjPlot)
tab_model(model_rolgenero4,model_rolgenero42, title = "model_rolgenero4_____________(1)____________________________(2)")

detach(WV4_Data_stata_v_2015_04_18)

#Obtencion de variables para ambas bases de datos
#1 base
WVS_Longitudinal_1981_2014_stata_v2015_04_18 <- read_dta("C:/Users/Pablo/Desktop/tarea_2/WVS_Longitudinal_1981_2014_stata_v2015_04_18.dta") 
attach(WVS_Longitudinal_1981_2014_stata_v2015_04_18)
edad3=c(X003)

edades=c(X003)

rol1=ifelse(X003<=64&X003>=24&C001==1,"1","0")
paises=c(S003)
escolaridad=c(X025)
situacion=c(C006)

A=data.frame(paises,rol1, edades, escolaridad, situacion)


detach(WVS_Longitudinal_1981_2014_stata_v2015_04_18)
#2 base

ZA4804_v3_0_0 <- read_dta("C:/Users/Pablo/Desktop/tarea_2/ZA4804_v3_0_0.dta") 
attach(ZA4804_v3_0_0)

edades1=c(X003)
escolaridad1=c(X025)
situacion1=c(C006)
rol2=ifelse(X003<=64&X003>=24&C001==1,"1","0")
pais3=c(S003)
B=data.frame(pais3,rol2,edades1,escolaridad1, situacion1)

#Unir ambas bases

names(B)=c("paises","rol1","edades","escolaridad", "situacion")

names(A)==names(B)

baserol1=rbind(A,B)

summary(baserol1)

detach(ZA4804_v3_0_0)

#plot participacion de mercado y rol genero 1

rolgenero1=c(0.07855,0.21702,0.17837,0.08573,0.02511,0.052275,0.12702,0.135905,0.17617,0.17717,0.39628,0.12438,0.186577,0.177111,0.07021,0.05707,0.05752,0.17392,0.23306,0.023189,0.10457,0.44029,0.114277,0.07126)

paises2=c("australia","austria","belgica","canada","dinamarca","finlandia","francia","alemania","grecia","hong kong","india","irlanda","italia","japon","holanda","nzelanda","noruega","portugal","singapur","suecia","suiza","turquia","uk","us")

mercado=c(0.404,0.056,0.5,0.25,0.28,0.187,0.15,0.089,0.102,0.138,0.033,0.17,0.07,0.297,0.14,0.310,0.21,0.145,0.083,0.22,0.176,0.012,0.3,0.260)

gini=c(0.272,0.263,0.343,0.276,0.253,0.293,0.295,0.343,0.351,0.295,0.331,0.379,0.269,0.339,0.473,0.276,0.415,0.315,0.415)

data1=data.frame(paises2,rolgenero1,mercado)

fit=glm(data1$mercado~data1$rolgenero1)

plot(data1$rolgenero1,data1$mercado, xlab = "Rol Genero 1", ylab = "Participacion mercado domestico", main = "Rol genero 1 y participacion en el mercado domestico",abline(fit, col="red"))

rolgenero1paises=factor(rolgenero1, levels= c(0.07855,0.21702,0.17837,0.08573,0.02511,0.052275,0.12702,0.135905,0.17617,0.17717,0.39628,0.12438,0.186577,0.177111,0.07021,0.05707,0.05752,0.17392,0.23306,0.023189,0.10457,0.44029,0.114277,0.07126), 
               labels=c("australia","austria","belgica","canada","dinamarca","finlandia","francia","alemania","grecia","hong kong","india","irlanda","italia","japon","holanda","nzelanda","noruega","portugal","singapur","suecia","suiza","turquia","uk","us"))

#Regresion univariada mercado~rolgenero1 por pais

model_regresion=lm(mercado~rolgenero1paises)
model_regresion
summary(model_regresion)

model_regresion1=lm(mercado~rolgenero1, data=data1)
model_regresion1
summary(model_regresion1)

#filtrar base por pais

austria=subset(baserol1, paises==40)
belgica=subset(baserol1, paises==56)
canada=subset(baserol1, paises==124)
dinamarca=subset(baserol1, paises==208)
finlandia=subset(baserol1, paises==246)
francia=subset(baserol1, paises==250)
alemania=subset(baserol1, paises==276)
grecia=subset(baserol1, paises==300)
india=subset(baserol1, paises==356)
irlanda=subset(baserol1, paises==372)
italia=subset(baserol1, paises==380)
japon=subset(baserol1, paises==392)
holanda=subset(baserol1, paises==528)
portugal=subset(baserol1, paises==620)
singapur=subset(baserol1, paises==702)
suecia=subset(baserol1, paises==752)
turquia=subset(baserol1, paises==792)
uk=subset(baserol1, paises==826)
us=subset(baserol1, paises==840)

#obtener medias de las variables de interes 

describe(austria$edades)
describe(belgica$edades)
describe(canada$edades)
describe(dinamarca$edades)
describe(finlandia$edades)
describe(francia$edades)
describe(alemania$edades)
describe(grecia$edades)
describe(india$edades)
describe(irlanda$edades)
describe(italia$edades)
describe(japon$edades)
describe(holanda$edades)
describe(portugal$edades)
describe(singapur$edades)
describe(suecia$edades)
describe(turquia$edades)
describe(uk$edades)
describe(us$edades)

describe(austria$escolaridad)
describe(belgica$escolaridad)
describe(canada$escolaridad)
describe(dinamarca$escolaridad)
describe(finlandia$escolaridad)
describe(francia$escolaridad)
describe(alemania$escolaridad)
describe(grecia$escolaridad)
describe(india$escolaridad)
describe(irlanda$escolaridad)
describe(italia$escolaridad)
describe(japon$escolaridad)
describe(holanda$escolaridad)
describe(portugal$escolaridad)
describe(singapur$escolaridad)
describe(suecia$escolaridad)
describe(turquia$escolaridad)
describe(uk$escolaridad)
describe(us$escolaridad)

describe(austria$edades)
describe(belgica$edades)
describe(canada$edades)
describe(dinamarca$edades)
describe(finlandia$edades)
describe(francia$edades)
describe(alemania$edades)
describe(grecia$edades)
describe(india$edades)
describe(irlanda$edades)
describe(italia$edades)
describe(japon$edades)
describe(holanda$edades)
describe(portugal$edades)
describe(singapur$edades)
describe(suecia$edades)
describe(turquia$edades)
describe(uk$edades)
describe(us$edades)

describe(austria$situacion)
describe(belgica$situacion)
describe(canada$situacion)
describe(dinamarca$situacion)
describe(finlandia$situacion)
describe(francia$situacion)
describe(alemania$situacion)
describe(grecia$situacion)
describe(india$situacion)
describe(irlanda$situacion)
describe(italia$situacion)
describe(japon$situacion)
describe(holanda$situacion)
describe(portugal$situacion)
describe(singapur$situacion)
describe(suecia$situacion)
describe(turquia$situacion)
describe(uk$situacion)
describe(us$situacion)

#creacion dataframe con todas las variables

paises4=c("austria","belgica","canada","dinamarca","finlandia","francia","alemania","grecia","india","irlanda","italia","japon","holanda","portugal","singapur","suecia","turquia","uk","us")
rolgeneros1=c(0.21702,0.17837,0.08573,0.02511,0.052275,0.12702,0.135905,0.17617,0.39628,0.12438,0.186577,0.177111,0.07021,0.17392,0.23306,0.023189,0.44029,0.114277,0.07126)
rolgenero1paises=factor(rolgeneros1, levels= c(0.21702,0.17837,0.08573,0.02511,0.052275,0.12702,0.135905,0.17617,0.39628,0.12438,0.186577,0.177111,0.07021,0.17392,0.23306,0.023189,0.44029,0.114277,0.07126), 
                        labels=c("austria","belgica","canada","dinamarca","finlandia","francia","alemania","grecia","india","irlanda","italia","japon","holanda","portugal","singapur","suecia","turquia","uk","us"))
mercado2=c(0.056,0.050,0.250,0.280,0.187,0.150,0.089,0.102,0.033,0.170,0.070,0.297,0.140,0.145,0.083,0.220,0.012,0.300,0.260)
gini=c(0.272,0.263,0.343,0.276,0.253,0.293,0.295,0.343,0.351,0.295,0.331,0.379,0.269,0.339,0.473,0.276,0.415,0.315,0.415)
edadmedia=c(46.67,46.39,46.33,45.76,42.58,45.12,48.86,35.89,40.03,47.06,45.28,46.85,46.38,48.4,33.58,44.19,36.59,44.12,42.36)
escolaridadmedia=c(3.703,5.219,4.932,4.345,3.794,3.916,4.026,6.157,2.529,3.876,4.478,5.562,4.938,3.916,3.706,4.936,3.113,4.096,5.645)
satisfaccion=c(NA,NA,0.852,NA,NA,NA,NA,NA,0.644,NA,NA,0.747,NA,NA,0.865,NA,0.513,NA,0.802)

data2=data.frame(paises4,rolgenero1paises,mercado2,gini,edadmedia, escolaridadmedia, satisfaccion, rolgeneros1)

#regresion multivariada mercado 3.3

modelo1=lm(mercado2~rolgeneros1+escolaridadmedia+edadmedia+gini,data = data2)
modelo1
summary(modelo1)

#regresion satisfaccion rolgenero1 por pais 3.4
modelo2=lm(satisfaccion~rolgenero1paises,data=data2)
modelo2
summary(modelo2)

modelo2_1=lm(satisfaccion~rolgeneros1, data=data2)
modelo2_1
summary(modelo2_1)

#regresion multivariada satisfaccion 3.4
modelo3=lm(satisfaccion~rolgeneros1+escolaridadmedia+edadmedia+gini,data = data2)
modelo3
summary(modelo3)
