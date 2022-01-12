****
*TAREA DE REPRODUCIBILIDAD (PARTE II) - B
*ECONOMETRÍA APLICADA

*CAMILO TORRES ALMUNA


***********************************************************************************************
***********************************************************************************************
*One in a Million: Field Experiments on Perceived Closeness of the Election and Voter Turnout
***********************************************************************************************

*Defino el directorio principal:
global main "C:\Users\camilo.torres\Documents\ECONOMIA\MAGISTER EN ANALISIS ECONOMICO (U. de Chile)\CURSOS\3. ECONOMETRÍA APLICADA I\Evaluaciones\Tarea de replicación\Paper asignado"


/*==================================================================
 Programa: Main_e2014.do
 Propósito: Análisis de datos principales para el RCT 2014.
==================================================================*/

clear
set more off

/*==================================================================
ESTADÍSTICAS DE BALANCE Y ANTECEDENTES DE COVARIABLES
==================================================================*/

/*================================================
Apéndice Tabla C14: Medias muestrales
================================================*/
set more off
use "$main/2014_RCT.dta", clear
matrix input M = (1,2,3,4,5,6,7,8,9)
matrix input N = (1\2\3\4\5\6\7\8\9\10\11)
matrix H=N*M
matrix rownames H = Male Black Hispanic Other_race Age Democrat Republican Other_party vote2008? vote2010? vote2012?
matrix colnames H = control close+big close+small notclose+big  notclose+small close notclose big small
matrix list H

foreach yX in t_close t_fewvoters{
	assert `yX'==0 | `yX'==1 | mi(`yX')
}

global CharList = "male black hisp race_other age reg_dem reg_rep reg_otherparty vote2008 vote2010 vote2012"
local Counter = 0
foreach yX of global CharList{
	local Counter = `Counter'+1
	mean `yX', over(treatment)
	matrix H[`Counter',1]=e(b)
	mean `yX', over(t_close)
	matrix J=e(b)
	matrix H[`Counter',6]=J[1,2]
	matrix H[`Counter',7]=J[1,1]
	mean `yX', over(t_fewvoters)
	matrix J=e(b)
	matrix H[`Counter',8]=J[1,2]
	matrix H[`Counter',9]=J[1,1]
}
* H1 es la versión redondeada de H.
matrix H1 = H
forvalues iX=1/11{
	forvalues kX=1/9{
		matrix H1[`iX',`kX']=round(H[`iX',`kX'],.001)
	}
}
outtable using "$main/Tab-AllTreat-Means-$spec", mat(H1) nobox replace center

* Tasas de faltantes discutidas en las notas de la Tabla C14.
set more off
use "$main/2014_RCT.dta", clear
sum mi_gender 
assert r(mean)<.01
sum mi_race 
assert r(mean)<.01
sum reg_unknown
assert round(r(mean),.01)==0.42

/*================================================
Apéndice Tabla C15: valores p.
================================================*/
set more off
use "$main/2014_RCT.dta", clear
matrix input M = (1,2,3)
matrix input N = (1\2\3\4\5\6\7\8\9\10\11)
matrix H=N*M
matrix rownames H = Male Black Hispanic Other_race Age Democrat Republican Other_party vote2008? vote2010? vote2012?
matrix colnames H = close/notclose close/control control/notclose
matrix list H
global CharList = "male black hisp race_other age reg_dem reg_rep reg_otherparty vote2008 vote2010 vote2012"
local Counter = 0
foreach yX of global CharList{
	local Counter = `Counter'+1
	* Comparar Cerrar vs. No cerrar. Excluido Control.
	ttest `yX', by(t_close)
		matrix H[`Counter',1]=round(r(p),.001)
	* Comparar Cerrar vs. Controlar. Excluir No cerrar.
	ttest `yX' if t_close!=0, by(t_close_vscontrol)
		matrix H[`Counter',2]=round(r(p),.001)
	* Comparar No Cerrar vs. Controlar. Excluir Cerrar.
	ttest `yX' if t_close!=1, by(t_notclose_vscontrol)
		matrix H[`Counter',3]=round(r(p),.001)		
}
outtable using "$main/Tab-Expt2014-pVals-$spec", mat(H) nobox replace center


/*================================================
Discutido en el texto en la Sección 3: Tasas de votación por grupo de tratamiento.
================================================*/
set more off
use "$main/2014_RCT.dta", clear
gen t_manyvoters = 1-t_fewvoters

matrix SS_VoteRate = [0]
foreach yX in t_close_vscontrol t_notclose_vscontrol t_fewvoters t_manyvoters{
	sum vote100 if `yX'==1
		matrix SS_VoteRate = [SS_VoteRate\ round(r(mean),.1)]
}	
matrix list SS_VoteRate
scalar zz = rowsof(SS_VoteRate)
matrix SS_VoteRate = SS_VoteRate[2..zz,1]
matrix rowname SS_VoteRate = Close Not_Close Few_Voters Many_Voters
outtable using "$main/Table-Expt2014-VoteRates_$spec", mat(SS_VoteRate) nobox replace center

/*================================================
TABLA C2, PANEL B: ENCUESTAS 
================================================*/
set more off
use "$main/2014_RCT.dta", clear
gen closedem = poll1 if party1=="Democrat" & t_close==1
gen closerep = poll1 if party1=="Republican" & t_close==1
gen nclosedem = poll1 if party1=="Democrat" & t_close==0
gen ncloserep = poll1 if party1=="Republican" & t_close==0
gen avgrep = 100-avgdem
collapse closedem closerep nclosedem ncloserep avgdem avgrep, by(state)
foreach yX in closedem closerep nclosedem ncloserep avgdem avgrep{
	gen `yX'_perc=string(`yX')+"\%"
	drop `yX'
	rename `yX'_perc `yX'
}
dataout, save("$main/Tab-2014PollList-$spec") tex replace


/*================================================
TABLA C2, PANEL C: INFORMACIÓN DE EXPERTOS 
================================================*/
set more off
use "$main/2014_RCT.dta", clear
destring to, force replace ignore(",")
matrix input M = (1,2)
matrix input N = (1\2\3\4\5\6\7)
matrix H=N*M
local Counter = 0
foreach yX in AR FL GA KS MA MI WI{
	local Counter = `Counter'+1
	mean to if state=="`yX'", over(t_fewvoters)
	matrix J=e(b)
	matrix H[`Counter',1]=J[1,2]
	matrix H[`Counter',2]=J[1,1]
}
matrix rownames H = AR FL GA KS MA MI WI
matrix colnames H = Small_Electorate Large_Electorate
outtable using "$main/Tab-ExpertInfo-$spec", mat(H) nobox replace center


/*================================================
Márgenes de encuesta promedio por tratamiento
================================================*/
set more off
use "$main/2014_RCT.dta", clear
drop if mi(t_close)
collapse poll_margin, by(t_close)
sum poll_margin if t_close==1
sum poll_margin if t_close==0

/*================================================
Diferencia promedio en el tamaño del electorado previsto por tratamiento.
================================================*/
set more off
use "$main/2014_RCT.dta", clear
destring to, force replace ignore(",")
drop if mi(to)
foreach yX in min max{
	bysort state: egen `yX'_to = `yX'(to)
}
gen ratio = max_to/min_to
sum ratio




/*==================================================================
PRINCIPALES IMPACTOS EN LA PARTICIPACIÓN ELECTORAL

~ Resultados basales (TABLA 6 en el texto principal). Utilice "todos".
~ Tabla C20: Excluir personas que siempre votaron previamente en los datos. Usa "~votar_siempre".
~ FN 43: Excluir a las personas que siempre votaron o nunca votaron. Usa "votar_soloalgunos".
~ Cuadro C28: Restringido al jefe de hogar (persona a quien se dirige el sobre). Utilice "hh_cabeza".
~ Tabla C26: Restricción a las elecciones que están por debajo del tamaño medio. Utilice "pequeño_estado".
~ Discutido en el texto en la Sección 4A restringido a estados sin carrera por el Senado.
Utilice "no_senado".
~ Discutido en el texto en la Sección 4A: restringido a los estados donde usamos
una encuesta cerrada 50/50. Utilice "estado_cerrar50".
~ No utilizado: Restringido a personas con estatus de votante activo. Utilice "activo".

Uno necesita recorrer todas las opciones a continuación para producir las diferentes tablas.

yX = Restricción muestral o submuestra
   ~ foreach yX en todos ~vote_always vote_onlysome hh_head small_state no_senate state_close50 activo{
==================================================================*/	
set more off
qui do "$main/zz_globals_e2014.do"
foreach yX in all ~vote_always vote_onlysome hh_head small_state no_senate state_close50 active {
use "$main/2014_RCT.dta", clear
keep if `yX'==1
areg vote100 t_close $BaseVars_V2014, absorb(strata) cluster(hhid_unique)
	capture drop zz zz1
	egen zz1 = mean(vote100) if e(sample)==1 & t_close==0
	egen zz = max(zz1)	
	outreg2 t_close using "$main/Table-Expt2014-`yX'-$spec", $OR2_nodec dec(2) replace addstat(Mean dv if not close poll=1, zz)	
areg vote100 t_close $FullCont_V2014, absorb(strata) cluster(hhid_unique)
	capture drop zz zz1
	egen zz1 = mean(vote100) if e(sample)==1 & t_close==0
	egen zz = max(zz1)	
	outreg2 t_close using "$main/Table-Expt2014-`yX'-$spec", $OR2_nodec dec(2) append addstat(Mean dv if not close poll=1, zz) 
areg vote100 t_close_vscontrol t_notclose_vscontrol $FullCont_V2014, absorb(strata) cluster(hhid_unique)
	capture drop zz zz1
	egen zz1 = mean(vote100) if e(sample)==1 & mi(t_close)
	egen zz = max(zz1)	
	test t_close_vscontrol=t_notclose_vscontrol
	outreg2 t_close_vscontrol t_notclose_vscontrol using "$main/Table-Expt2014-`yX'-$spec", $OR2_nodec dec(2) append addstat(Mean dv if control=1,zz,F(Close_vs_NotClose),r(p))
areg vote100 t_close t_fewvoters $FullCont_V2014, absorb(strata) cluster(hhid_unique)
	capture drop zz zz1
	egen zz1 = mean(vote100) if e(sample)==1 & t_close==0
	egen zz = max(zz1)	
	outreg2 t_close t_fewvoters using "$main/Table-Expt2014-`yX'-$spec", $OR2_nodec dec(2) append	addstat(Mean dv if not close poll=1, zz)	
areg vote100 t_closeXt_fewvoters t_closeXt_manyvoters t_notcloseXt_fewvoters $FullCont_V2014, absorb(strata) cluster(hhid_unique)	
	capture drop zz zz1
	egen zz1 = mean(vote100) if e(sample)==1 & t_close==0
	egen zz = max(zz1)	
	outreg2 t_closeXt_fewvoters t_closeXt_manyvoters t_notcloseXt_fewvoters using "$main/Table-Expt2014-`yX'-$spec", $OR2_nodec dec(2) append	addstat(Mean dv if not close poll=1, zz)	
}
	
