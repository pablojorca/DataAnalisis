library(haven)
library(dplyr)
library(car)
library(lmtest)
library(mfx)


wave6 <- read_dta("C:/Users/Pablo/Desktop/tarea 1/WV6_Stata_v_2016_01_01.dta") 
attach(wave6)

#Parte II. Participación laboral: modelo probit

trabajo=c(V229)
dummytrabajo=recode(trabajo,"1=1;2=1;3=1;else=0")

p1=recode(V45, "1=0.5 ;else=0")
p2=recode(V47, "1=0.5;else=0")
p3=recode(V48, "1=0.5;else=0")
p4=recode(V50, "2=0.5;1=1;else=0")
p5=recode(V51, "2=0.5;1=1;else=0")
p6=recode(V52, "2=0.5;1=1;else=0")
p7=recode(V53, "2=0.5;1=1;else=0")
p8=recode(V54, "2=0.5;1=1;else=0")

rolgen=p1+p2+p3+p4+p5+p6+p7+p8

sexo=recode(V240, "1=0;2=1;else=NA") 
edad2=V242^2
edad=c(V242)
pais=c(V2)

junto=data.frame(sexo,dummytrabajo,rolgen,edad2,edad, pais)

#1. Regresion normal
model1<- lm(dummytrabajo~rolgen+sexo+edad+edad2, data=junto)
summary(model1)
model1


#2. Regresion modelo probit
modelprobit=glm(dummytrabajo~rolgen+sexo+edad+edad2,family = binomial(link = "probit"),data=junto)
summary(modelprobit)

probitmfx(formula = dummytrabajo~rolgen+sexo+edad+edad2, data=junto )


#3. Regresiones por sexo
mujer=filter(junto, sexo==1)
hombre=filter(junto, sexo==0)

#3.1 Mujeres
modelprobit_mujer= glm(dummytrabajo~rolgen+edad+edad2,family = binomial(link = "probit"), data=mujer)
modelprobit_mujer
summary(modelprobit_mujer)

probitmfx(formula = dummytrabajo~rolgen+edad+edad2, data=mujer )

#3.2 Hombres

modelprobit_hombre= glm(dummytrabajo~rolgen+edad+edad2,family = binomial(link = "probit"), data=hombre)
modelprobit_hombre
summary(modelprobit_hombre)

probitmfx(formula = dummytrabajo~rolgen+edad+edad2, data=hombre)



#4. Regresiones por paises

chile=filter(junto, pais==152)
usa=filter(junto, pais==840)
uruguay=filter(junto, pais==858)
espana=filter(junto, pais==724)
china=filter(junto, pais==156)

#4.1. Chile
modelprobit_chile=glm(dummytrabajo~rolgen+sexo+edad+edad2,family = binomial(link = "probit"), data=chile)
summary(modelprobit_chile)

probitmfx(formula = dummytrabajo~rolgen+sexo+edad+edad2, data=chile )

#4.2 USA
modelprobit_usa=glm(dummytrabajo~rolgen+sexo+edad+edad2,family = binomial(link = "probit"), data=usa)
summary(modelprobit_usa)

probitmfx(formula = dummytrabajo~rolgen+sexo+edad+edad2, data=usa ) 

#4.3 Uruguay
modelprobit_uruguay=glm(dummytrabajo~rolgen+sexo+edad+edad2,family = binomial(link = "probit"), data=uruguay)
summary(modelprobit_uruguay)

probitmfx(formula = dummytrabajo~rolgen+sexo+edad+edad2, data=uruguay )

#4.4 Espana
modelprobit_espana=glm(dummytrabajo~rolgen+sexo+edad+edad2,family = binomial(link = "probit"), data=espana)
summary(modelprobit_espana)

probitmfx(formula = dummytrabajo~rolgen+sexo+edad+edad2, data=espana )

#4.5 China
modelprobit_china=glm(dummytrabajo~rolgen+sexo+edad+edad2,family = binomial(link = "probit"), data=china)
summary(modelprobit_china)

probitmfx(formula = dummytrabajo~rolgen+sexo+edad+edad2, data=china )

detach(wave6)

# III.Roles de genero y escolaridad: Variables instrumentales

datos <- read_dta("C:/Users/Pablo/Desktop/tarea3/datos.dta") 
attach(datos)

sexo=recode(X001, "1=0;2=1;else=NA") 
edad=c(X003)
pais=c(S003)
esc=c(X025R)
rel=c(A006)

religion=recode(rel,"1=4;2=3;3=2;4=1;else=0")
escolaridad1=recode(esc, "1=8;2=12;3=17;else=0")
escolaridad=as.numeric(escolaridad1)

b1=recode(C001, "1=0.5 ;else=0")
b2=recode(D074, "5=1;4=0.5;else=0")
b3=recode(D072, "5=1;4=0.5=else=0")
b4=recode(E213, "2=0.5;1=1;else=0")
b5=recode(D059, "2=0.5;1=1;else=0")
b6=recode(D060, "2=0.5;1=1;else=0")
b7=recode(D078, "2=0.5;1=1;else=0")
b8=recode(D057, "2=0.5;1=1;else=0")

rolgen4=b1+b2+b3+b4+b5+b6+b7+b8
  
roles_sexo=sexo*rolgen4

junto2=data.frame(pais,escolaridad,sexo,edad,roles_sexo,rolgen4,religion)

alemania=filter(junto2, pais==276)
australia=filter(junto2, pais==36)
austria=filter(junto2, pais==40)
belgica=filter(junto2, pais==56)
canada=filter(junto2, pais==124)
chile=filter(junto2, pais==152)
coreanorte=filter(junto2, pais==408)
coreasur=filter(junto2, pais==410)
dinamarca=filter(junto2, pais==208)
eslovenia=filter(junto2, pais==705)
espana=filter(junto2, pais==724)
us=filter(junto2, pais==840)
estonia=filter(junto2, pais==233)
finlandia=filter(junto2, pais==246)
francia=filter(junto2, pais==250)
grecia=filter(junto2, pais==300)
hungaria=filter(junto2, pais==348)
irlanda=filter(junto2, pais==372)
islandia=filter(junto2, pais==352)
israel=filter(junto2, pais==376)
italia=filter(junto2, pais==380)
japon=filter(junto2, pais==392)
luxemburgo=filter(junto2, pais==442)
mexico=filter(junto2, pais==484)
noruega=filter(junto2, pais==578)
nzelanda=filter(junto2, pais==554)
holanda=filter(junto2, pais==528)
polonia=filter(junto2, pais==616)
portugal=filter(junto2, pais==620)
granbretana=filter(junto2, pais==826)
republicache=filter(junto2, pais==203)
eslovaquia=filter(junto2, pais==703)
turquia=filter(junto2, pais==792)
suecia=filter(junto2, pais==752)
suiza=filter(junto2, pais==756)

ocde=rbind(alemania, australia,austria,belgica,canada,chile,coreanorte,coreasur
           ,dinamarca,eslovenia,espana,us,estonia,finlandia,francia,grecia,hungaria
           ,irlanda,islandia,israel,italia,japon,luxemburgo,mexico,noruega,nzelanda
           ,holanda,polonia,portugal,granbretana,republicache,eslovaquia,turquia,suecia,suiza)

ocde_na=na.omit(ocde)

#1. Modelo General

model_ocde=lm(escolaridad~edad+rolgen4+sexo+roles_sexo,data=ocde_na)
model_ocde
summary(model_ocde)

#2. Modelo separado por sexo
ocde_mujer=filter(ocde_na, sexo==1)
ocde_hombre=filter(ocde_na, sexo==0)

#2.1 Modelo para mujer
model_ocde_mujer=lm(escolaridad~edad+rolgen4,data=ocde_mujer)
model_ocde_mujer
summary(model_ocde_mujer)

#2.2 Modelo para hombre
model_ocde_hombre=lm(escolaridad~edad+rolgen4,data=ocde_hombre)
model_ocde_hombre
summary(model_ocde_hombre)


#4. Correlacion variable roles y religiosidad

cor(ocde_na$religion, ocde_na$rolgen4)

#5.1 Modelo variable instrumental general

ols=lm(formula=escolaridad~rolgen4+edad, data=ocde_na)
ols_ins=lm(rolgen4~religion, data=ocde_na)

summary(ols_ins)

estim=fitted.values(ols_ins)

model_instrumental=lm(ocde_na$escolaridad~ocde_na$edad+estim)

summary(model_instrumental)


ocdena_mujer=filter(ocde_na, sexo==1)
ocdena_hombre=filter(ocde_na, sexo==0)

#5.2 Modelo variable instrumental mujer
olsm=lm(formula=escolaridad~rolgen4+edad, data=ocdena_mujer)
olsm_ins=lm(rolgen4~religion, data=ocdena_mujer)

summary(olsm_ins)

estim_mujer=fitted.values(olsm_ins)

model_instru_mujer=lm(ocdena_mujer$escolaridad~ocdena_mujer$edad+estim_mujer)

summary(model_instru_mujer)

#5.3 Modelo variable instrumental hombre

olsh=lm(formula=escolaridad~rolgen4+edad, data=ocdena_hombre)
olsh_ins=lm(rolgen4~religion, data=ocdena_hombre)

summary(olsh_ins)

estim_hombre=fitted.values(olsh_ins)

model_instrumental_hombre=lm(ocdena_hombre$escolaridad~ocdena_hombre$edad+estim_hombre)

summary(model_instrumental_hombre)
