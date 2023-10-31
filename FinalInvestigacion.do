clear all
set more off

global usuario "pablo"
global direc "D:\Data\pablo\Documentos\Universidad\paper"
global data "$direc\bases"
global fig "$direc\graficos"
global tablas "$direc\tablas" 

***************************
******Para PteALto*********
***************************
***************************

use $data\casen2003.dta, clear

gen ing_ocup03=yopraj
gen ing_ocuph03=yoprhaj
rename yopraj ingreso
rename yoprhaj ingreso_hogar

gen hijo=0
replace hijo=1 if pco1==3
replace hijo=. if hijo==0
egen num_hijos=count(hijo), by(segmento f)

gen hombre=0
replace hombre=1 if sexo==1
replace hombre=. if hombre==0
egen num_hombres=count(hombre), by(segmento f)

gen mujer=0
replace mujer=1 if sexo==2
replace mujer=. if mujer==0
egen num_mujeres=count(mujer), by(segmento f)

gen contrato=0
replace contrato=1 if o11==1| o11==2

gen n_dormitorio=v3a
gen n_bano=v3g

gen propietario=0
replace propietario=1 if v17<9 

gen subsidio=0
replace subsidio=1 if v18<17

gen credito=0
replace credito=1 if v20<5

gen casade=0
replace casade=1 if ecivil==1

gen hrs=o19_hrs
rename comu comuna
keep if r==13
keep if pco1==1

keep segmento f comuna numper casade expc expr subsidio propietario hrs credito num_hijos num_hombres rama num_mujeres n_bano n_dormitorio contrato ingreso ingreso_hogar hrs esc educ qaut dau daur sexo edad ecivil ing_ocup03 ing_ocuph03 

keep if comuna==13117|comuna==13401|comuna==13126|comuna==13301|comuna==13123|comuna==13132

gen tratado4=. 
replace tratado4=1 if comuna==13301
replace tratado4=0 if comuna==.


save $direc\pre1.dta, replace

clear all
use $data\casen2006.dta, clear

gen ing_ocup06=yopraj
gen ing_ocuph06=yoprhaj

rename yopraj ingreso
rename yoprhaj ingreso_hogar

gen hijo=0
replace hijo=1 if pco1==3
replace hijo=. if hijo==0
egen num_hijos=count(hijo), by(seg f)

gen hombre=0
replace hombre=1 if sexo==1
replace hombre=. if hombre==0
egen num_hombres=count(hombre), by(seg f)

gen mujer=0
replace mujer=1 if sexo==2
replace mujer=. if mujer==0
egen num_mujeres=count(mujer), by(seg f)

gen contrato=0
replace contrato=1 if o20==1| o20==2

gen n_dormitorio=v3a
gen n_bano=v3g

gen propietario=0
replace propietario=1 if v17<9 

gen subsidio=0
replace subsidio=1 if v19<17

gen credito=0
replace credito=1 if v20<5

gen casade=0
replace casade=1 if ecivil==1

gen hrs=o15
keep if pco1==1
keep if r_15==15
rename seg segmento



keep  segmento f comuna numper subsidio  casade rama expr expc propietario hrs credito num_hijos num_hombres num_mujeres n_bano n_dormitorio contrato ingreso ingreso_hogar hrs esc educ qaut dau daur sexo edad ecivil ing_ocup06 ing_ocuph06 

keep if comuna==13401|comuna==13112|comuna==13201|comuna==13119|comuna==13116|comuna==13125

gen tratado4=. 
replace tratado4=1 if comuna==13201
replace tratado4=0 if comuna==.

save $direc\pre2.dta, replace 


use $direc\pre1.dta, clear

gen year=2003

append using $direc\pre2.dta, force


replace year=2006 if year==.

save $data\basepaper1.dta, replace


*Generar resto de variables para ambas

*Generar variable tratado para Puente Alto

*****GENERAMOS LA VARIABLE DE TIEMPO****

gen t=1 if year==2006
replace t=0 if year==2003 

save $data\basepaper1.dta, replace

keep if ingreso>0
keep if edad>15
keep if edad<65
**Generar Sectores**


*mincer

gen exp=edad-esc-6
gen exp2=exp^2
gen esc2=esc^2
gen hombre=1 if sexo==1
replace hombre=0 if sexo==2
gen hijo=0
replace hijo=1 if num_hijos>0

gen log_ingreso=log(ingreso_hogar)

tab tratado4 year

gen tratado=.
replace tratado=tratado4 if tratado4==1
replace tratado=0 if tratado==.

tab tratado year



**GENERAMOS LA VARIABLE DIF

gen dif=tratado*t

*LA pintana, san bernardo, puente alto, maipu

reg log_ingreso tratado t dif [aw=expc], r
outreg2 using $tablas\ptaltomco1.tex, replace ctitle(DIFnDIF) dec(4)

reg log_ingreso tratado t dif exp esc esc2 [aw=expc]
outreg2 using $tablas\ptaltomco1.tex, append ctitle(Mincer) dec(4)

reg log_ingreso tratado t dif exp esc esc2 hombre  [aw=expc]
outreg2 using $tablas\ptaltomco1.tex, append ctitle(x Género) dec(4)

reg log_ingreso tratado t dif exp esc esc2 hombre hijo casade propietario subsidio credito [aw=expc]
outreg2 using $tablas\ptaltomco1.tex, append ctitle(Controles) dec(4)


reg hrs tratado t dif [aw=expc], r
outreg2 using $tablas\ptaltomco_hrs.tex, replace ctitle(DIFnDIF HRS)

reg hrs tratado t dif exp esc esc2 [aw=expc]
outreg2 using $tablas\ptaltomco_hrs.tex, append ctitle(Mincer) dec(4)

reg hrs tratado t dif exp esc esc2 hombre  [aw=expc]
outreg2 using $tablas\ptaltomco_hrs.tex, append ctitle(x Género) dec(4)

reg hrs tratado t dif exp esc esc2 hombre hijo casade propietario subsidio credito [aw=expc]
outreg2 using $tablas\ptaltomco_hrs.tex, append ctitle(Controles) dec(4)





***************************
******Para maipu********
***************************
***************************

clear all
*Generar Variables 2009

use $data\casen2009.dta, clear
gen ing_ocup09=yopraj
gen ing_ocuph09=yoprhaj

rename yopraj ingreso
rename yoprhaj ingreso_hogar

gen hijo=0
replace hijo=1 if pco1==3
replace hijo=. if hijo==0
egen num_hijos=count(hijo), by(folio)

gen hombre=0
replace hombre=1 if sexo==1
replace hombre=. if hombre==0
egen num_hombres=count(hombre), by(folio)

gen mujer=0
replace mujer=1 if sexo==2
replace mujer=. if mujer==0
egen num_mujeres=count(mujer), by(folio)

gen contrato=0
replace contrato=1 if o25==1| o25==2

gen n_dormitorio=v7a
gen n_bano=v7g

gen propietario=0
replace propietario=1 if v24<9 

gen subsidio=0
replace subsidio=1 if v27==1

gen credito=0
replace credito=1 if v28<5


gen hrs=o16

keep if region==13
keep if pco1==1


keep folio comuna numper expc subsidio propietario hrs credito num_hijos num_hombres num_mujeres n_bano n_dormitorio contrato ingreso ingreso_hogar hrs esc educ qaut dau daur sexo edad ecivil ing_ocup09 ing_ocuph09

save $direc\pre3.dta, replace


*Generar Variables 2013

use $data\casen2013.dta, clear
gen ing_ocup13=yoprcor
gen ing_ocuph13=yoprcorh
rename yoprcor ingreso
rename yoprcorh ingreso_hogar

gen hijo=0
replace hijo=1 if pco1==3
replace hijo=. if hijo==0
egen num_hijos=count(hijo), by(folio)

gen hombre=0
replace hombre=1 if sexo==1
replace hombre=. if hombre==0
egen num_hombres=count(hombre), by(folio)

gen mujer=0
replace mujer=1 if sexo==2
replace mujer=. if mujer==0
egen num_mujeres=count(mujer), by(folio)

gen contrato=0
replace contrato=1 if o17==1| o17==2

gen n_dormitorio=v29a
gen n_bano=v29d

gen propietario=0
replace propietario=1 if v14<9 

gen subsidio=0
replace subsidio=1 if v16<3

gen credito=0
replace credito=1 if v17<8

gen hrs=o10

rename qaut_mn qaut
rename dau_mn dau
rename dautr_mn daur

*dejamos a los de la RM y Jefes de Hogar
keep if region==13
keep if pco1==1

keep folio comuna numper expc subsidio propietario hrs credito num_hijos num_hombres num_mujeres n_bano n_dormitorio contrato ingreso ingreso_hogar hrs esc educ qaut dau daur sexo edad ecivil ing_ocup13 ing_ocuph13

save $direc\pre4.dta, replace

*Unimos bases 2009-2013
use $direc\pre3.dta, clear
gen year=2009

append using $direc\pre4.dta, force
replace year=2013 if year==.

save $direc\basepaper2.dta, replace


gen casade=0
replace casade=1 if ecivil==1
*Generar variable tratado para Puente Alto

*****GENERAMOS LA VARIABLE DE TIEMPO****

gen t=1 if year==2013
replace t=0 if year==2009 

save $data\basepaper1.dta, replace

keep if ingreso>0
keep if edad>15
keep if edad<65
**Generar Sectores**

*mincer

gen exp=edad-esc-6
gen exp2=exp^2
gen esc2=esc^2
gen hombre=1 if sexo==1
replace hombre=0 if sexo==2
gen hijo=0
replace hijo=1 if num_hijos>0

gen log_ingreso=log(ingreso_hogar)



**GENERAMOS LA VARIABLE DIF

gen tratado=1 if comuna==13119
replace tratado=0 if comuna!=13119

gen dif=tratado*t

*LA pintana, san bernardo, maipu, lo espejo, quilicura
keep if comuna==13401|comuna==13112|tratado==1|comuna==13116|comuna==13125

reg log_ingreso tratado t dif [aw=expc], r
outreg2 using $tablas\meipumco1.tex, replace ctitle(DIFnDIF) dec(4)

reg log_ingreso tratado t dif exp esc esc2 [aw=expc]
outreg2 using $tablas\meipumco1.tex, append ctitle(Mincer) dec(4)

reg log_ingreso tratado t dif exp esc esc2 hombre  [aw=expc]
outreg2 using $tablas\meipumco1.tex, append ctitle(x Género) dec(4)

reg log_ingreso tratado t dif exp esc esc2 hombre hijo casade propietario subsidio credito [aw=expc]
outreg2 using $tablas\meipumco1.tex, append ctitle(Controles) dec(4)


reg hrs tratado t dif [aw=expc], r
outreg2 using $tablas\meipumco_hrs.tex, replace ctitle(DIFnDIF HRS)

reg hrs tratado t dif exp esc esc2 [aw=expc]
outreg2 using $tablas\meipumco_hrs.tex, append ctitle(Mincer) dec(4)

reg hrs tratado t dif exp esc esc2 hombre  [aw=expc]
outreg2 using $tablas\meipumco_hrs.tex, append ctitle(x Género) dec(4)

reg hrs tratado t dif exp esc esc2 hombre hijo casade propietario subsidio credito [aw=expc]
outreg2 using $tablas\meipumco_hrs.tex, append ctitle(Controles) dec(4)




