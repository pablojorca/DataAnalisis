**PREGUNTA 1**

clear all 
set more off

global usuario "Laura Ponce" 



if "$usuario"=="Laura Ponce" {
global direc "C:\Users\laura\Documents\Microeconomía\Microeconomía III\Problem Set 3\Bases"

global data "$direc\bbdd" // Base
global fig "$direc\graficos" // Graficos
}


cd "$direc"
cd "C:\Users\laura\Documents\Microeconomía\Microeconomía III\Problem Set 3\Bases"


use AutoaplicadaENCLA2008.dta

rename P216 NUMTRAB
rename P429 WTOTAL
rename FEMINIZACION FEMINIZADA 
rename RAMA ACTIVIDAD_1 
rename P4 EXPORTA

gen salario=(WTOTAL/NUMTRAB)
gen lnw=ln(salario)

gen SINDICATO=.
replace SINDICATO=0 if SINDICAT==2
replace SINDICATO=1 if SINDICAT==1

gen AÑO=.
replace AÑO=2008

keep SINDICATO NUMTRAB WTOTAL FEMINIZADA EXPORTA REGION TAMAÑO FOLIO salario lnw AÑO ACTIVIDAD_1 

save AutoaplicadaENCLA08.dta, replace 

clear all
use AutoaplicadaENCLA2011.dta

rename RAMA ACTIVIDAD_1
rename P4 EXPORTA
rename P549 WTOTAL

gen salario=(WTOTAL/NUMTRAB)
gen lnw=ln(salario)

gen AÑO=.
replace AÑO=2011

keep SINDICATO NUMTRAB WTOTAL FEMINIZADA EXPORTA REGION TAMAÑO FOLIO salario lnw AÑO ACTIVIDAD_1 

save AutoaplicadaENCLA11.dta, replace 

clear all
use AutoaplicadaENCLA2014.dta

gen WTOTAL=P48A1+P48A2+P48A3+P48A4+P48A5+P48A6+P48A7+P48A8+P48A9

rename P4 EXPORTA

gen salario=(WTOTAL/NUMTRAB)
gen lnw=ln(salario)

gen AÑO=.
replace AÑO=2014

encode ACTIVIDAD, generate(ACTIVIDAD_1)

keep SINDICATO NUMTRAB WTOTAL FEMINIZADA EXPORTA REGION TAMAÑO FOLIO salario lnw AÑO ACTIVIDAD_1 

save AutoaplicadaENCLA14.dta, replace 

clear all
use AutoaplicadaENCLA08.dta
append using AutoaplicadaENCLA11.dta
append using AutoaplicadaENCLA14.dta

save baseENCLA0814.dta, replace

//Parte d

**Para trabajadores sindicalizados

histogram lnw if SINDICATO==1, normal kdensity ytitle(Densidad) ytitle(, size(small)) xtitle(ln salarios) xtitle(, size(small)) title(Distribución de los Salarios de Trabajadores Sindicalizados, size(small))

** Para trabajadores no sindicalizados

histogram lnw if SINDICATO==0, normal kdensity ytitle(Densidad) ytitle(, size(small)) xtitle(ln salarios) xtitle(, size(small)) title(Distribución de los Salarios de Trabajadores No Sindicalizados, size(small))

//Parte g

*Minería
g mineria=0
replace mineria=1 if ACTIVIDAD_1==3

*Región
gen region1=0
replace region1=1 if REGION==1
gen region2=0
replace region2=1 if REGION==2
gen region3=0
replace region3=1 if REGION==3
gen region4=0
replace region4=1 if REGION==4
gen region5=0
replace region5=1 if REGION==5
gen region6=0
replace region6=1 if REGION==6
gen region7=0
replace region7=1 if REGION==7
gen region8=0
replace region8=1 if REGION==8
gen region9=0
replace region9=1 if REGION==9
gen region10=0
replace region10=1 if REGION==10
gen region11=0
replace region11=1 if REGION==11
gen region12=0
replace region12=1 if REGION==12
gen region13=0
replace region13=1 if REGION==13
gen region14=0
replace region14=1 if REGION==14
gen region15=0
replace region15=1 if REGION==15


g tratados=.
replace tratados=1 if AÑO==2011 
replace tratados=0 if AÑO==2008

g interaccion=(SINDICATO*tratados)

reg lnw interaccion FEMINIZADA EXPORTA TAMAÑO mineria region2 region3 region4 region5 region6 region7 region8 region9 region10 region11 region12 region13 region14 region15

outreg2 using difsindicato.tex, replace dec(3)

clear all
set more off

global usuario "pablo"
global direc "D:\Data\pablo\Documentos\Universidad\Micro\PS3"
global data "$direc\bawe"
global fig "$direc\graficos"
global tablas "$direc\tablas" 

import delimited "D:\Data\pablo\Documentos\Universidad\Micro\PS3\3%\1_afiliados.csv", encoding(Big5) clear 

rename v1 id
rename v2 sexo
rename v3 fechanaci
rename v4 educ
rename v5 anos
rename v6 civil
rename v7 comuna
rename v8 institucion


save $direc\afiliados.dta, replace

clear all 
import delimited "D:\Data\pablo\Documentos\Universidad\Micro\PS3\3%\5_rentas_imponibles.csv", encoding(Big5) clear 

rename v1 id
rename v2 fechadeven
rename v3 contrato
rename v4 subsidio
rename v5 actividad
rename v6 comuna_empl
rename v7 ingreso
rename v8 n_trab
rename v9 rentra_prom
rename v10 desvi_renta
rename v11 ind_0
rename v12 ind_tope
rename v13 ind_min
rename v14 ind_emp


save $direc\rentas.dta, replace

clear all
use $direc\rentas.dta

gen f_rem= ym(floor(fechadeven/100), mod(fechadeven,100)) 
format f_rem  %tm 

gen yr=floor(fechadeven/100)
rename id algo
gen id=floor(algo)

gen mes=mod(fechadeven,100)

gen ing=floor(ingreso)

drop if yr<2010

egen meses=count(mes), by(id yr ind_emp)

egen ingr=sum(ing), by(id yr ind_emp)

save $direc\ingresos.dta, replace

clear all
use $direc\ingresos.dta

gen ing_mensual=ingr/meses

gen dummi1=0
replace dummi1=1 if ingr==0
replace dummi1=1 if meses==0


gen trabajado=0
replace trabajado=1 if meses<=14
keep if trabajado==1

gen contr=0
replace contr=1 if contrato==1

gen menos6=0
replace menos6=1 if meses<=6 & meses>0

drop if actividad==0



collapse (mean) ing_mensual dummi1 trabajado menos6 me_contr=contr yr (first) contr contr_yr=yr (last) actividad libre=dummi1 xao_yr=yr xao_mes=mes, by(id)

save $direc\colapsada.dta, replace
*Generar Variables

clear all
use $direc\afiliados.dta
merge 1:1 id using $direc\colapsada.dta 
keep if _merge==3
drop _merge

gen year=floor(fechanaci/100)
drop if year<1953
drop if year>1997
gen edad=yr-year


gen region=floor(comuna/1000)
drop if region==0

label define region1 1 "Tarapaca" 2 "Antofagasta" 3 "Atacama" 4 "Coquimbo" 5 "Valparaiso" 6 "Ohiggins" 7 "Maule" 8 "Biobio" 9 "Araucania" 10 "Los Lagos" 11 "Ibanezdelcampo" 12 "Magallanes" 13 "RM" 14 "Los Rios" 15 "Arica y Par" 16 "Nuble"

label values region region1

label define actividad1 1 "Agricultura" 2 "Mineria" 3 "Manufacturera" 4 "SSBB" 5 "Suministro" 6 "Construccion" 7 "Comercio" 8 "Transporte" 9 "Turismo" 10 "Comunicaciones" 11 "Financiera" 12 "Inmobiliaria" 13 "Ciencia/tecnico" 14 "Administra" 15 "Ad. Publica" 16 "Enseñanza" 17 "Salud" 18 "Artes" 19 "Servicios" 20 "Domestico" 21 "Organizaciones"

label values actividad actividad1

gen casade=0
replace casade=1 if civil==2

drop if civil==99

gen esc=.
replace esc=0 if educ==0
replace esc=6 if educ==1
replace esc=8 if educ==2
replace esc=8 if educ==3
replace esc=10 if educ==4
replace esc=12 if educ==5
replace esc=10 if educ==6
replace esc=12 if educ==7
replace esc=12 if educ==8
replace esc=15 if educ==9
replace esc=13 if educ==10
replace esc=17 if educ==11
replace esc=17 if educ==12
replace esc=19 if educ==13
replace esc=4 if educ==14
replace esc=6 if educ==15
replace esc=8 if educ==16
replace esc=12 if educ==17

drop if educ==99

gen exp=edad-esc-6
drop if exp<0

gen ano=floor(yr)
*Pregunta b
bysort sexo: outreg2 using $tablas\tab1.tex, replace sum(log) eqkeep(mean sd N) keep(esc exp ing_mensual menos6) 

ttest esc, by(sexo)

ttest exp, by(sexo)

ttest ing_mensual, by(sexo)

gen ocupado=.
replace ocupado=1 if dummi1==0
replace ocupado=0 if dummi1>0
drop if ocupado==.

gen ingxhora=ing_mensual/40
gen log_ing=log(ingxhora)
gen exp2=exp^2

gen mujer=.
replace mujer=1 if sexo=="F"
replace mujer=0 if sexo=="M"

drop if mujer==.

label define mujer1 0 "Hombre" 1 "Mujer"

label values mujer mujer1


save $direc\ultima.dta, replace

clear all
use $direc\ultima.dta
*Pregunta C
keep if ocupado==1

bysort sexo: outreg2 using $tablas\tab2.tex, replace sum(log) eqkeep(mean sd N) keep(esc exp ing_mensual menos6) 

ttest esc, by(sexo) 

ttest exp, by(sexo)

ttest ing_mensual, by(sexo)


xtile quintil=ing_mensual, nq(5)

egen prom_esc=mean(esc), by(quintil sexo)

twoway bar prom_esc quintil, by(sexo) xsc(noextend)
graph export $tablas\distribucion.png, replace


*Estadística Descriptiva
tab sexo quintil, sum(esc)

tab region sexo, sum(ing_mensual)

tab ano sexo, sum(ing_mensual)

tab region sexo, sum(esc)

tab ano sexo, sum(esc) 

tab actividad sexo, sum(ing_mensual)

tab actividad sexo, sum(esc)


*Pregunta D
reg log_ing esc exp exp2, r
outreg2 using $tablas\Mincer1.tex, replace ctitle(Log W/H) dec(4)

reg log_ing esc exp exp2 casade menos6 i.region i.ano i.actividad, r
outreg2 using $tablas\Mincer1.tex, append ctitle(C/controles) dec(4)
*Pregunta f
reg log_ing esc exp exp2 mujer casade menos6 i.region i.ano i.actividad, r
outreg2 using $tablas\Mincer1.tex, append ctitle(C/género) dec(4)

*Pregunta g
reg log_ing esc exp exp2 casade menos6 i.region i.ano i.actividad if mujer==1, r
outreg2 using $tablas\Mincer2.tex, replace ctitle(Mujeres) dec(4)

reg log_ing esc exp exp2 casade menos6 i.region i.ano i.actividad if mujer==0, r
outreg2 using $tablas\Mincer2.tex, append ctitle(Hombres) dec(4)

clear all
use $direc\ingresos.dta

gen dummi1=0
replace dummi1=1 if ingr==0
replace dummi1=1 if meses==0

gen trabajado=0
replace trabajado=1 if meses<=14
keep if trabajado==1

gen contr=1
replace contr=2 if contrato==1

gen menos6=0
replace menos6=1 if meses<=6 & meses>0


gen ing_mensual=ingr/meses if yr==2018

keep if yr==2017|yr==2018

collapse (first) ant=contr (mean) ing_mensual (last) dummi1 dps=contr menos6 actividad, by(id) cw

gen diferencia=ant-dps

gen cont=. 
replace cont=1 if diferencia==-1
replace cont=0 if diferencia!=-1
drop if cont==.


save $direc\contratado.dta, replace

clear all
use $direc\afiliados.dta
merge 1:1 id using $direc\contratado.dta 
keep if _merge==3
drop _merge

gen year=floor(fechanaci/100)
drop if year<1953
drop if year>1997
gen edad=2018-year

gen region=floor(comuna/1000)
drop if region==0

gen mujer=.
replace mujer=1 if sexo=="F"
replace mujer=0 if sexo=="M"

drop if mujer==.

label define region1 1 "Tarapaca" 2 "Antofagasta" 3 "Atacama" 4 "Coquimbo" 5 "Valparaiso" 6 "Ohiggins" 7 "Maule" 8 "Biobio" 9 "Araucania" 10 "Los Lagos" 11 "Ibanezdelcampo" 12 "Magallanes" 13 "RM" 14 "Los Rios" 15 "Arica y Par" 16 "Nuble"

label values region region1

label define actividad1 1 "Agricultura" 2 "Mineria" 3 "Manufacturera" 4 "SSBB" 5 "Suministro" 6 "Construccion" 7 "Comercio" 8 "Transporte" 9 "Turismo" 10 "Comunicaciones" 11 "Financiera" 12 "Inmobiliaria" 13 "Ciencia/tecnico" 14 "Administra" 15 "Ad. Publica" 16 "Enseñanza" 17 "Salud" 18 "Artes" 19 "Servicios" 20 "Domestico" 21 "Organizaciones"

label values actividad actividad1

label define mujer1 0 "Hombre" 1 "Mujer"

label values mujer mujer1

gen esc=.
replace esc=0 if educ==0
replace esc=6 if educ==1
replace esc=8 if educ==2
replace esc=8 if educ==3
replace esc=10 if educ==4
replace esc=12 if educ==5
replace esc=10 if educ==6
replace esc=12 if educ==7
replace esc=12 if educ==8
replace esc=15 if educ==9
replace esc=13 if educ==10
replace esc=17 if educ==11
replace esc=17 if educ==12
replace esc=19 if educ==13
replace esc=4 if educ==14
replace esc=6 if educ==15
replace esc=8 if educ==16
replace esc=12 if educ==17

drop if educ==99

gen exp=edad-esc-6
drop if exp<0

gen casade=0
replace casade=1 if civil==2

drop if civil==99

gen ingxhora=ing_mensual/40
gen log_ing=log(ingxhora)
gen exp2=exp^2

drop if ing_mensual==0

bysort sexo: outreg2 using $tablas\tab2.tex, replace sum(log) eqkeep(mean sd N) keep(esc exp ing_mensual) 

*pregunta h
reg log_ing esc exp exp2 mujer casade cont menos6 i.region i.actividad, r
outreg2 using $tablas\Mincer3.tex, replace ctitle(Contratadx) dec(4)

reg log_ing esc exp exp2 cont casade menos6 i.region i.actividad if mujer==1, r
outreg2 using $tablas\Mincer3.tex, append ctitle(Contratada) dec(4)

reg log_ing esc exp exp2 cont casade menos6 i.region i.actividad if mujer==0, r
outreg2 using $tablas\Mincer3.tex, append ctitle(Contratado) dec(4)

*Pregunta J
clear all
use $direc\ultima.dta

gen lning=log_ing*ocupado
gen edad2=edad^2

heckman lning esc exp exp2 menos6 i.region i.ano if mujer==1, select(ocupado= edad edad2 casade esc i.actividad) twostep
outreg2 using $tablas\Heckman1.tex, replace ctitle(Modelo Selección Heckman) dec(4)

heckman lning esc exp exp2 menos6 i.region i.ano if mujer==0, select(ocupado= edad edad2 casade esc i.actividad) twostep
outreg2 using $tablas\Heckman2.tex, replace ctitle(Modelo Selección Heckman) dec(4)


