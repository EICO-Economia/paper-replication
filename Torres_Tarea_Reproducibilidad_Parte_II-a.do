****
*TAREA DE REPRODUCIBILIDAD (PARTE II) - A
*ECONOMETRÍA APLICADA

*CAMILO TORRES ALMUNA


***********************************************************************************************
***********************************************************************************************
*One in a Million: Field Experiments on Perceived Closeness of the Election and Voter Turnout
***********************************************************************************************

*Defino el directorio principal:
global main "C:\Users\camilo.torres\Documents\ECONOMIA\MAGISTER EN ANALISIS ECONOMICO (U. de Chile)\CURSOS\3. ECONOMETRÍA APLICADA I\Evaluaciones\Tarea de replicación\Paper asignado"


/*==================================================================
 Programa: Main_e2010.do
 Propósito: Realiza análisis de datos del RCT de 2010.
==================================================================*/ 
  
/*==================================================================
PARTES DEL CÓDIGO:
- COMPROBACIÓN DE LA ALEATORIZACIÓN
- CIFRAS SOBRE CREENCIAS
- CORRELATOS DE CREENCIAS
- PRIMERA ETAPA: ¿EL TRATAMIENTO CERRAR/NO CERRAR AFECTA LAS CREENCIAS?
- RELACIÓN OBSERVACIONAL ENTRE CERCANÍA Y PARTICIPACIÓN
- OLS: CREENCIAS SOBRE LA CERCANÍA ELECTORAL Y LA PARTICIPACIÓN ELECTORAL
- IV: EFECTOS DE LAS CREENCIAS SOBRE LA CERCANÍA ELECTORAL SOBRE LA PARTICIPACIÓN
- ANÁLISIS DE VALENCIA: ¿CÓMO AFECTAN LAS PREFERENCIAS DE LAS ENCUESTAS?

Las variables de tiempo de la encuesta en 2010_RCT.dta son para la encuesta preelectoral.
==================================================================*/

clear
qui do "$main/zz_globals_e2010.do"	
set more off
set matsize 3000

* Agregué lo siguiente para tratar con outreg2 diciendo que los archivos son de solo lectura
winexec taskkill /F /IM Notepad.exe /T


/*==================================================================
VERIFICACIÓN DE ALEATORIZACIÓN: Hacemos esto de dos maneras:
(1) Entre quienes realizaron la encuesta de creencias preelectorales.
(2) Entre todos (basado en la asignación)

El enfoque principal está en (1), lo que refleja el fuerte enfoque del documento en la comprensión de las creencias de cercanía.
==================================================================*/

global Demogs_SS = 		"male black hisp other mixed ppage dropout hs somecoll bachelors mastersphd inc_2550 inc_5075 inc_75100 inc_100p"
global PartyReg_SS = 	"democrat_reg republican_reg npa_dts_indep_reg otherparty_reg"
global PolitVars_SS = 	"pelosi int_govt_scale dem_1to7 liberal_1to7"
global VoteBeliefs_SS = "votemarg_pre prob_100votes_pre prob_1000votes_pre pvote_pre prob_votedem_pre prob_voterep_pre prob_voteunderdog_pre"
global VoteVars_SS = 	"$PastVoteVars vote_admin_past"

global VarNames = 		"$Demogs_SS $PartyReg_SS $PolitVars_SS $VoteBeliefs_SS $VoteVars_SS"

* Número de variables en las estadísticas de resumen. 
matrix input N = (1\2\3\4\5\6\7\8\9\10\11\12\13\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28\29\30\31\32\33\34\35\36)

local Vars_In_SS = rowsof(N)

* Close, NotClose, valor p de la prueba t
matrix input M3 = (1,2,3)

* Close, NotClose, Control, p:1v2, p:1v3, p:2v3
matrix input M6= (1,2,3,4,5,6)

/*================================================
Tabla C2, Panel A: Encuestas proporcionadas y promedios de encuestas
=================================================*/
set more off
use "$main/2010_RCT.dta", clear
gen avgrep = 100-avgdem
collapse closedem closerep nclosedem ncloserep avgdem avgrep, by(state_st_assign)
drop if mi(state_st_assign)
foreach yX in closedem closerep nclosedem ncloserep avgdem avgrep{
	gen `yX'_perc=string(`yX')+"\%"
	drop `yX'
	rename `yX'_perc `yX'
}
dataout, save("$main/Tab-PollList-$spec") tex replace

/*================================================
Márgenes de encuesta promedio por tratamiento
=================================================*/
set more off
use "$main/2010_RCT.dta", clear
drop if mi(t_close) | mi(state_st_assign)
collapse poll_margin, by(t_close)
sum poll_margin if t_close==1
sum poll_margin if t_close==0



/*================================================
Apéndice Tabla C3: Estadísticas resumidas simples
=================================================*/
set more off
use "$main/2010_RCT.dta", clear
keep if t_close==0 | t_close==1
sutex $Demogs_SS $PartyReg_SS $PolitVars_SS ///
	votemarg_pre votemarg_post prob_100votes_pre prob_100votes_post /// 
	prob_1000votes_pre prob_1000votes_post pvote_pre pvote_post prob_votedem_pre prob_voterep_pre prob_votedem_post prob_voterep_post ///
	prob_voteunderdog_pre prob_voteunderdog_post vote_sr vote_admin vote_admin_past, /// 
	minmax labels digits(2) 
	
		
/*================================================
Tabla 1: Comparación de medias por tratamiento
ENTRE LOS QUE HICIERON LA ENCUESTA PREELECTORAL.
Estas son las personas que fueron asignadas a los tratamientos Cerrar o No Cerrar
Y quién contestó la encuesta. 
=================================================*/
set more off
use "$main/2010_RCT.dta", clear
matrix H=N*M3
matrix colnames H = close notclose pval
matrix rownames H = $Demogs_SS $PartyReg_SS $PolitVars_SS $VoteBeliefs_SS $VoteVars_SS
local Counter = 0
foreach yX of global VarNames{
	local Counter = `Counter'+1
	qui mean `yX', over(t_close)
	matrix J=e(b)
	matrix H[`Counter',1]=J[1,2]
	matrix H[`Counter',2]=J[1,1]	
	qui ttest `yX', by(t_close)
	matrix H[`Counter',3]=round(r(p),.01)	
}
matrix H1 = H
forvalues iX=1/`Vars_In_SS'{
	forvalues kX=1/3{
		matrix H1[`iX',`kX']=round(H[`iX',`kX'],.01)
	}
}
outtable using "$main/Tab-2010Means-CloseNotclose-$spec", mat(H1) nobox replace center

/*================================================
APÉNDICE TABLA C5
Comparación entre los que no respondieron a la encuesta.
================================================*/

set more off
use "$main/2010_RCT.dta", clear
keep if t_assign_close==1 | t_assign_notclose==1
drop if t_close==0 | t_close==1

matrix input N2 = (1\2\3\4\5\6\7)
local Vars_In_SS2 = rowsof(N2)
matrix H=N2*M3
matrix colnames H = close notclose pval
matrix rownames H = $PartyReg_SS dem_1to7 liberal_1to7 vote_admin_past
global VarNames2 "$PartyReg_SS dem_1to7 liberal_1to7 vote_admin_past"

local Counter = 0
foreach yX of global VarNames2{
	local Counter = `Counter'+1
	qui mean `yX', over(t_assign_close)
	matrix J=e(b)
	matrix H[`Counter',1]=J[1,2]
	matrix H[`Counter',2]=J[1,1]	
	qui ttest `yX', by(t_assign_close)
	matrix H[`Counter',3]=round(r(p),.01)	
}
matrix H1 = H
forvalues iX=1/`Vars_In_SS2'{
	forvalues kX=1/3{
		matrix H1[`iX',`kX']=round(H[`iX',`kX'],.01)
	}
}
outtable using "$main/Tab-2010Means-NonResponders-$spec", mat(H1) nobox replace center

/*================================================
ANALIZADO EN EL APÉNDICE B1
Comparación entre todos los votantes de la muestra
================================================*/
set more off
use "$main/2010_RCT.dta", clear
matrix H=N*M3
matrix colnames H = close notclose control
matrix rownames H = $Demogs_SS $PartyReg_SS $PolitVars_SS $VoteBeliefs_SS $VoteVars_SS
local Counter = 0
foreach yX of global VarNames{
	local Counter = `Counter'+1
	qui mean `yX', over(xpivotal_3groups)
	matrix J=e(b)
	matrix H[`Counter',1]=J[1,1]
	matrix H[`Counter',2]=J[1,2]	
	matrix H[`Counter',3]=J[1,3]	
}
matrix H1 = H
forvalues iX=1/`Vars_In_SS'{
	forvalues kX=1/3{
		matrix H1[`iX',`kX']=round(H[`iX',`kX'],.01)
	}
}
outtable using "$main/Tab-2010Means-3Groups-$spec", mat(H1) nobox replace center

/*================================================
ANALIZADO EN EL APÉNDICE B1
Comparación entre todos los votantes de la muestra, pero excluye California.
================================================*/
set more off
use "$main/2010_RCT.dta", clear
drop if state_st_assign=="CA"
matrix H=N*M3
matrix colnames H = close notclose control
matrix rownames H = $Demogs_SS $PartyReg_SS $PolitVars_SS $VoteBeliefs_SS $VoteVars_SS
local Counter = 0
foreach yX of global VarNames{
	local Counter = `Counter'+1
	qui mean `yX', over(xpivotal_3groups)
	matrix J=e(b)
	matrix H[`Counter',1]=J[1,1]
	matrix H[`Counter',2]=J[1,2]	
	matrix H[`Counter',3]=J[1,3]	
}
matrix H1 = H
forvalues iX=1/`Vars_In_SS'{
	forvalues kX=1/3{
		matrix H1[`iX',`kX']=round(H[`iX',`kX'],.01)
	}
}
outtable using "$main/Tab-2010Means-3Groups-NoCA-$spec", mat(H1) nobox replace center


* Agregué lo siguiente para tratar con outreg2 diciendo que los archivos son de solo lectura
winexec taskkill /F /IM Notepad.exe /T

/*==================================================================
CIFRAS SOBRE CREENCIAS:
- ¿CUÁL ES LA DISTRIBUCIÓN DE LAS CREENCIAS DE LOS VOTANTES?
- ¿CAMBIAN REGULARMENTE LAS CREENCIAS O EL COMPORTAMIENTO DE VOTACIÓN PREVISTO EN RESPUESTA A LAS ENCUESTAS?
==================================================================*/


/*================================================
FIGURA 1: DISTRIBUCIÓN DE LOS MÁRGENES CREIDOS ANTES DEL TRATAMIENTO
================================================*/

** Pretratamiento margen de voto creído en la población completa 
** Siempre debe ser uniforme.
set more off
use "$main/2010_RCT.dta", clear
assert mod(votemarg_pre,2)==0 if ~mi(votemarg_pre)
assert votemarg_pre>=0 &  votemarg_pre<=100 if ~mi(votemarg_pre)
sum votemarg_pre, detail
global Median = r(p50)
global P25 = r(p25)
global P75 = r(p75)
histogram votemarg_pre if votemarg_pre>=0, discrete width(1) start(0) xlabel(0(10)100) xmtick(0(5)100) ///
	ylabel(0(.05).2) xtitle(Predicted vote margin) ///
	note("Median = $Median, 25th Percentile = $P25, 75th Percentile = $P75") ///
	graphr(fc(white)) 
graph export "$main/Fig-PredMarginPre.pdf", as(pdf) replace


/*================================================
FIGURA 2: DISTRIBUCIÓN DE CREENCIAS
Probabilidades subjetivas de que la elección de gobernador se decida por menos de 100 votos o 1000 votos, antes del tratamiento
================================================*/

** Figura 2: Gráficos iniciales usando población completa 
set more off
use "$main/2010_RCT.dta", clear
foreach iX in 100 1000{ 
assert prob_`iX'votes_pre>=0 & prob_`iX'votes_pre<=100 if ~mi(prob_`iX'votes_pre)
sum prob_`iX'votes_pre, detail
global Median = r(p50)
global P25 = r(p25)
global P75 = r(p75)
histogram prob_`iX'votes_pre , discrete width(1) start(0) xlabel(0(10)100) xmtick(0(5)100) ///
	ylabel(0(.05).2) xtitle(Prob margin < `iX' votes) ///
	note("Median = $Median, 25th Percentile = $P25, 75th Percentile = $P75") ///
	graphr(fc(white)) 
graph export "$main/Fig-Prob`iX'VotesPre.pdf", as(pdf) replace
}

** Apéndice Figura C2: Restringido a Maestría o Doctorado
set more off
use "$main/2010_RCT.dta", clear
keep if mastersphd==1
foreach iX in 100 1000{ 
assert prob_`iX'votes_pre>=0 & prob_`iX'votes_pre<=100 if ~mi(prob_`iX'votes_pre)
sum prob_`iX'votes_pre, detail
global Median = r(p50)
global P25 = r(p25)
global P75 = r(p75)
histogram prob_`iX'votes_pre, discrete width(1) start(0) xlabel(0(10)100) xmtick(0(5)100) ///
	ylabel(0(.05).2) xtitle(Prob margin < `iX' votes) ///
	note("Median = $Median, 25th Percentile = $P25, 75th Percentile = $P75") ///
	graphr(fc(white)) 
graph export "$main/Fig-Prob`iX'VotesPre-MasterPHD.pdf", as(pdf) replace
}


/*================================================
IMPACTO DE LAS ENCUESTAS EN LA DISTRIBUCIÓN DE CREENCIAS:
Apéndice Figura C3: Distribución de creencias sobre una elección muy reñida antes y después de los tratamientos cerrado y no cerrado
================================================*/
foreach kX in votemarg prob_100votes prob_1000votes {
use "$main/2010_RCT.dta", clear
keep if t_close==0 | t_close==1
gen insamp = ~mi(`kX'_pre) & ~mi(`kX'_post)
keep if insamp==1
foreach yX in 0 1 {
twoway (histogram `kX'_pre if t_close==`yX', discrete width(1) start(0) xlabel(0(10)100) xmtick(0(5)100) color(green)  ) ///
       (histogram `kX'_post if t_close==`yX', discrete width(1) start(0) xlabel(0(10)100) xmtick(0(5)100) fcolor(none) lcolor(black)) , ///
	   legend( label(1 "Pre-Treat") label(2 "Post-Treat") ) ///
	   ytitle("Density") ///
	   graphregion(fcolor(white) lcolor(white))
graph export "$main/Fig-DistPrePost-`kX'-tclose`yX'.pdf", as(pdf) replace
}		
}


* Agregué lo siguiente para tratar con outreg2 diciendo que los archivos son de solo lectura
winexec taskkill /F /IM Notepad.exe /T


/*================================================
FIGURA 4: GRADO DE ACTUALIZACIÓN DE CREENCIA EN RESPUESTA A ENCUESTAS.
================================================*/

** Figura 4a: Se incluyen a todos los sujetos.
set more off
use "$main/2010_RCT.dta", clear
gen index = .
gen coefficient =.
gen utail =.
gen ltail =.
local i 1
foreach yX in votemarg prob_100votes prob_1000votes {
	reg `yX'_change if t_close==1, robust
	replace utail = _b[_cons] + invttail(e(df_r),0.025)*_se[_cons] if _n== `i'
	replace ltail = _b[_cons] - invttail(e(df_r),0.025)*_se[_cons] if _n== `i'
	replace coefficient = _b[_cons] if _n== `i'
	replace index = `i' if _n ==`i'
	local ++i
	reg `yX'_change if t_close==0, robust
	
	replace utail = _b[_cons] + invttail(e(df_r),0.025)*_se[_cons] if _n== `i'
	replace ltail = _b[_cons] - invttail(e(df_r),0.025)*_se[_cons] if _n== `i'
	replace coefficient = _b[_cons] if _n== `i'
	replace index = `i' if _n == `i'
	local ++i
}
graph twoway (bar coefficient index if index==1|index==3|index==5) ///
	(bar coefficient index if index==2|index==4|index==6) ///
	(rcap utail ltail index, lcolor(black) blwidth(medthick) msize(large)), scheme(sj) ///
	xscale(off) ytitle(Change in Probability) graphr(fc(white)) legend(order(1 2) ///
	label(1 "Close") label(2 "Not Close")) text(-0.5 1.5 "Vote Margin") ///
	text(-0.5 3.5 "Less than 100 votes") text(-0.5 5.5 "Less than 1,000 votes")
graph export "$main/Fig-UpdateBeliefs.pdf", as(pdf) replace


** Figura 4b: Restringido a sujetos que cambian de creencias.
set more off
use "$main/2010_RCT.dta", clear
gen index = .
gen coefficient =.
gen utail =.
gen ltail =.
local i 1
foreach yX in votemarg prob_100votes prob_1000votes {
	reg `yX'_change if t_close==1 & `yX'_change!=0, robust
	replace utail = _b[_cons] + invttail(e(df_r),0.025)*_se[_cons] if _n== `i'
	replace ltail = _b[_cons] - invttail(e(df_r),0.025)*_se[_cons] if _n== `i'
	replace coefficient = _b[_cons] if _n== `i'
	replace index = `i' if _n ==`i'
	local ++i
	reg `yX'_change if t_close==0 & `yX'_change!=0, robust
	
	replace utail = _b[_cons] + invttail(e(df_r),0.025)*_se[_cons] if _n== `i'
	replace ltail = _b[_cons] - invttail(e(df_r),0.025)*_se[_cons] if _n== `i'
	replace coefficient = _b[_cons] if _n== `i'
	replace index = `i' if _n == `i'
	local ++i
}
graph twoway (bar coefficient index if index==1|index==3|index==5) ///
	(bar coefficient index if index==2|index==4|index==6) ///
	(rcap utail ltail index, lcolor(black) blwidth(medthick) msize(large)), scheme(sj) ///
	xscale(off) ytitle(Change in Probability) graphr(fc(white)) legend(order(1 2) ///
	label(1 "Close") label(2 "Not Close")) text(-0.5 1.5 "Vote Margin") ///
	text(-0.5 3.5 "Less than 100 votes") text(-0.5 5.5 "Less than 1,000 votes")
graph export "$main/Fig-UpdateBeliefs-Changers.pdf", as(pdf) replace



/*==================================================================
Apéndice Tabla C6: CORRELACIONES DE CREENCIAS
Los "BeliefControls" globales se definen en "zz_globals_e2010.do".
==================================================================*/	
set more off
qui do "$main/zz_globals_e2010.do"
foreach kX in votemarg prob_100votes prob_1000votes prob_xvotes {
use "$main/2010_RCT.dta", clear
reg `kX'_pre $BeliefControls, vce(bootstrap, cluster(ppstaten) reps($BBReps) seed(123))
	outreg2 using "$main/Tab-DetermineBeliefs-$spec", $OR2 append 
areg `kX'_pre $BeliefControls, vce(bootstrap, cluster(ppstaten) reps($BBReps) seed(123)) absorb(ppstaten)
	outreg2 using "$main/Tab-DetermineBeliefs-$spec", $OR2 append
}

	
/*==============================================================
PRIMERA ETAPA: ¿EL TRATAMIENTO CERRAR/NO CERRAR AFECTA LAS CREENCIAS?
==============================================================*/

* Útil para hacer outregging		
winexec taskkill /F /IM Notepad.exe /T

/*================================================
IMPACTOS NO PARAMÉTRICOS EN LAS CREENCIAS (Figura 3 en el Texto Principal)
=================================================*/
set more off
use "$main/2010_RCT.dta", clear
label define t_close_lbl 0 `"Not close"'
label define t_close_lbl 1 `"Close"', add
label values t_close t_close_lbl
foreach yX in votemarg prob_100votes prob_1000votes pvote prob_voteunderdog {
capture drop order
gen order = .
replace order=1 if `yX'_change_sign=="Decrease"
replace order=2 if `yX'_change_sign=="Same"
replace order=3 if `yX'_change_sign=="Increase"
catplot t_close `yX'_change_sign, percent(t_close) var2opts(sort(order) label(labsize(vlarge))) ///
	asyvars bar(1, color(gs12)) bar(2, color(gs8)) scheme(s1color) aspect(1) ///
	blabel(total, format(%8.1f) size(large))  yscale(range(0 100) noextend) ///
	ylabel(0(20)100, nogrid) ytitle("") legend(size(vlarge))	
graph export "$main/Fig-IncDecSame-`yX'.pdf", as(pdf) replace
}


/*================================================
RESULTADOS DE BASE SOBRE EL MARGEN DE VOTO (Tabla 2 en el Texto Principal)
zX = Restricción de muestra
    ~ foreach zX en todos {
kX = Variable de tratamiento analizada
    ~ foreach kX en t_close poll_margin {
Tabla 2 = Versión discreta con "t_close"
Tabla C7 = Versión continua con "poll_margin"	
=================================================*/
set more off
qui do "$main/zz_globals_e2010.do"
foreach zX in all {
foreach kX in t_close poll_margin {
use "$main/2010_RCT.dta", clear
keep if `zX'==1
reg votemarg_post `kX', robust 
	capture drop zz zz1 yy yy1
	egen zz1 = mean(votemarg_post) if e(sample)==1 & `kX'==0
	egen zz = max(zz1)
	egen yy1 = median(votemarg_post) if e(sample)==1 & `kX'==0
	egen yy = max(yy1)
	outreg2 `kX' using "$main/Tab-TreatBeliefs-`kX'-`zX'-$spec", $OR2_addtext addstat(Mean DV if `kX'=0, zz, Median DV if `kX'=0, yy) replace
areg votemarg_post `kX' $demogs, robust absorb(ppstaten) 
	capture drop zz zz1 yy yy1
	egen zz1 = mean(votemarg_post) if e(sample)==1 & `kX'==0
	egen zz = max(zz1)
	egen yy1 = median(votemarg_post) if e(sample)==1 & `kX'==0
	egen yy = max(yy1)
	outreg2 `kX' using "$main/Tab-TreatBeliefs-`kX'-`zX'-$spec", $OR2_addtext addstat(Mean DV if `kX'=0, zz, Median DV if `kX'=0, yy) append
areg votemarg_post votemarg_pre `kX' $demogs, robust absorb(ppstaten) 
	capture drop zz zz1 yy yy1
	egen zz1 = mean(votemarg_post) if e(sample)==1 & `kX'==0
	egen zz = max(zz1)
	egen yy1 = median(votemarg_post) if e(sample)==1 & `kX'==0
	egen yy = max(yy1)
	outreg2 votemarg_pre `kX' using "$main/Tab-TreatBeliefs-`kX'-`zX'-$spec", $OR2 addstat(Mean DV if `kX'=0, zz, Median DV if `kX'=0, yy) append
areg votemarg_change `kX' $demogs, robust absorb(ppstaten) 
	capture drop zz zz1 yy yy1
	egen zz1 = mean(votemarg_change) if e(sample)==1 & `kX'==0
	egen zz = max(zz1)	
	egen yy1 = median(votemarg_change) if e(sample)==1 & `kX'==0
	egen yy = max(yy1)	
	outreg2 `kX' using "$main/Tab-TreatBeliefs-`kX'-`zX'-$spec", $OR2_addtext addstat(Mean DV if `kX'=0, zz, Median DV if `kX'=0, yy) append
areg votemarg_post `kX' $Polit_Controls $demogs, robust absorb(ppstaten) 
	capture drop zz zz1 yy yy1
	egen zz1 = mean(votemarg_post) if e(sample)==1 & `kX'==0
	egen zz = max(zz1)
	egen yy1 = median(votemarg_post) if e(sample)==1 & `kX'==0
	egen yy = max(yy1)	
	outreg2 `kX' $Polit_Controls using "$main/Tab-TreatBeliefs-`kX'-`zX'-$spec", $OR2_addtext addstat(Mean DV if `kX'=0, zz, Median DV if `kX'=0, yy) append
areg votemarg_post `kX' `kX'Xint_govt_scale $Polit_Controls $demogs, robust absorb(ppstaten)
	capture drop zz zz1 yy yy1
	egen zz1 = mean(votemarg_post) if e(sample)==1 & `kX'==0
	egen zz = max(zz1)
	egen yy1 = median(votemarg_post) if e(sample)==1 & `kX'==0
	egen yy = max(yy1)
	outreg2 votemarg_post `kX' `kX'Xint_govt_scale $Polit_Controls using "$main/Tab-TreatBeliefs-`kX'-`zX'-$spec", $OR2_addtext addstat(Mean DV if `kX'=0, zz, Median DV if `kX'=0, yy) append
areg votemarg_post `kX' `kX'Xpelosi $Polit_Controls $demogs, robust absorb(ppstaten)
	capture drop zz zz1 yy yy1
	egen zz1 = mean(votemarg_post) if e(sample)==1 & `kX'==0
	egen zz = max(zz1)
	egen yy1 = median(votemarg_post) if e(sample)==1 & `kX'==0
	egen yy = max(yy1)	
	outreg2 `kX' `kX'Xpelosi $Polit_Controls using "$main/Tab-TreatBeliefs-`kX'-`zX'-$spec", $OR2_addtext addstat(Mean DV if `kX'=0, zz, Median DV if `kX'=0, yy) append
areg votemarg_post `kX' `kX'Xvote_admin_past $Polit_Controls $demogs, robust absorb(ppstaten)
	capture drop zz zz1 yy yy1
	egen zz1 = mean(votemarg_post) if e(sample)==1 & `kX'==0
	egen zz = max(zz1)
	egen yy1 = median(votemarg_post) if e(sample)==1 & `kX'==0
	egen yy = max(yy1)
	outreg2 `kX' `kX'Xvote_admin_past $Polit_Controls using "$main/Tab-TreatBeliefs-`kX'-`zX'-$spec", $OR2_addtext addstat(Mean DV if `kX'=0, zz, Median DV if `kX'=0, yy) append
}
}


/*================================================
TABLA 3: IMPACTO EN LA PROBABILIDAD PERCIBIDA DE ELECCIÓN
DECIDIÉNDOSE POR MENOS DE X VOTOS.
Tabla 3 = Versión discreta con "t_close"
Tabla C8 = Versión continua con "poll_margin"
================================================*/
set more off
qui do "$main/zz_globals_e2010.do"
foreach kX in t_close poll_margin {
use "$main/2010_RCT.dta", clear
foreach yX in prob_100votes prob_1000votes prob_xvotes {
reg `yX'_post `kX', robust
	capture drop zz zz1 yy yy1
	egen zz1 = mean(`yX'_post) if e(sample)==1 & `kX'==0
	egen zz = max(zz1)	
	egen yy1 = median(`yX'_post) if e(sample)==1 & `kX'==0
	egen yy = max(yy1)
	outreg2 `kX' using "$main/Tab-TreatBeliefs-XVotes-`kX'-$spec", $OR2_addtext addstat(Mean DV if `kX'=0, zz, Median DV if `kX'=0, yy) append
reg `yX'_post `kX' `yX'_pre, robust
	capture drop zz zz1 yy yy1
	egen zz1 = mean(`yX'_post) if e(sample)==1 & `kX'==0
	egen zz = max(zz1)	
	egen yy1 = median(`yX'_post) if e(sample)==1 & `kX'==0
	egen yy = max(yy1)
	outreg2 `kX' `yX'_pre using "$main/Tab-TreatBeliefs-XVotes-`kX'-$spec", $OR2_addtext addstat(Mean DV if `kX'=0, zz, Median DV if `kX'=0, yy) append
areg `yX'_post `kX' `yX'_pre $demogs, robust absorb(ppstaten) 
	capture drop zz zz1 yy yy1
	egen zz1 = mean(`yX'_post) if e(sample)==1 & `kX'==0
	egen zz = max(zz1)	
	egen yy1 = median(`yX'_post) if e(sample)==1 & `kX'==0
	egen yy = max(yy1)
	outreg2 `kX' `yX'_pre using "$main/Tab-TreatBeliefs-XVotes-`kX'-$spec", $OR2_addtext addstat(Mean DV if `kX'=0, zz, Median DV if `kX'=0, yy) append
}
}

/*================================================
Tabla C9: Igual que la Tabla 3, pero restringida a casos en los que las personas cambian de creencias.
================================================*/
set more off
qui do "$main/zz_globals_e2010.do"
foreach kX in t_close {
use "$main/2010_RCT.dta", clear
foreach yX in votemarg prob_100votes prob_1000votes prob_xvotes {
reg `yX'_post `kX' if `yX'_change!=0, robust
	capture drop zz zz1 yy yy1
	egen zz1 = mean(`yX'_post) if e(sample)==1 & `kX'==0
	egen zz = max(zz1)	
	egen yy1 = median(`yX'_post) if e(sample)==1 & `kX'==0
	egen yy = max(yy1)
	outreg2 `kX' using "$main/Tab-TreatBeliefs-Change-`kX'-$spec", $OR2_addtext addstat(Mean DV if `kX'=0, zz, Median DV if `kX'=0, yy) append
reg `yX'_post `kX' `yX'_pre if `yX'_change!=0, robust
	capture drop zz zz1 yy yy1
	egen zz1 = mean(`yX'_post) if e(sample)==1 & `kX'==0
	egen zz = max(zz1)	
	egen yy1 = median(`yX'_post) if e(sample)==1 & `kX'==0
	egen yy = max(yy1)
	outreg2 `kX' `yX'_pre using "$main/Tab-TreatBeliefs-Change-`kX'-$spec", $OR2_addtext addstat(Mean DV if `kX'=0, zz, Median DV if `kX'=0, yy) append
areg `yX'_post `kX' `yX'_pre $demogs if `yX'_change!=0, robust absorb(ppstaten) 
	capture drop zz zz1 yy yy1
	egen zz1 = mean(`yX'_post) if e(sample)==1 & `kX'==0
	egen zz = max(zz1)	
	egen yy1 = median(`yX'_post) if e(sample)==1 & `kX'==0
	egen yy = max(yy1)
	outreg2 `kX' `yX'_pre using "$main/Tab-TreatBeliefs-Change-`kX'-$spec", $OR2_addtext addstat(Mean DV if `kX'=0, zz, Median DV if `kX'=0, yy) append
}
}


/*==================================================================
RELACIÓN OBSERVACIONAL ENTRE CERCANÍA Y PARTICIPACIÓN
==================================================================*/

/*================================================
Apéndice Tabla C11 con SE agrupado por estado
=================================================*/
set more off
qui do "$main/zz_globals_e2010.do"
use "$main/2010_RCT.dta", clear
keep if t_close==0 | t_close==1
sort ppstaten
by ppstaten: gen st_count = _N
collapse vote_admin1 vote_admin_past margin_actual st_count, by(ppstaten)
reg vote_admin1 margin_actual, cluster(ppstaten)
	outreg2 margin_actual using "$main/Table-VoteOLSAdmin-LitReg-$spec", $OR2 replace
reg vote_admin1 margin_actual vote_admin_past, cluster(ppstaten)
	outreg2 margin_actual vote_admin_past using "$main/Table-VoteOLSAdmin-LitReg-$spec", $OR2 append


/*================================================
Apéndice Tabla C11 con arranque salvaje
=================================================*/	
* Columna 1
set more off
qui do "$main/zz_globals_e2010.do"
use "$main/2010_RCT.dta", clear
keep if t_close==0 | t_close==1
sort ppstaten
by ppstaten: gen st_count = _N
collapse vote_admin1 vote_admin_past margin_actual st_count, by(ppstaten)
global Col_1_regressors "margin_actual"
global Col_1_reg "reg vote_admin1 $Col_1_regressors, cluster(ppstaten)"
	$Col_1_reg
	bootwildct $Col_1_regressors, bootreps($BReps) numvars(1) seed(123)
	scalar zz = trace(e(t_wild_pval))
$Col_1_reg
	outreg2 margin_actual using "$main/Table-VoteOLSAdmin-LitReg-$spec", $OR2_addtext addstat(wild p value,zz) replace

* Columna 2	
global Col_2_regressors "margin_actual vote_admin_past"
global Col_2_reg "reg vote_admin1 $Col_2_regressors, cluster(ppstaten)"
	$Col_2_reg
	bootwildct $Col_2_regressors, bootreps($BReps) numvars(1) seed(123)
	scalar zz = trace(e(t_wild_pval))
$Col_2_reg
	outreg2 margin_actual using "$main/Table-VoteOLSAdmin-LitReg-$spec", $OR2_addtext addstat(wild p value,zz) append	
		
* Columna 3
set more off
qui do "$main/zz_globals_e2010.do"
use "$main/2010_RCT.dta", clear
global Col_3_regressors "margin_actual"
global Col_3_reg "reg vote_admin1 $Col_3_regressors, cluster(ppstaten)"
	$Col_3_reg
	bootwildct $Col_3_regressors, bootreps($BReps) numvars(1) seed(123)
	scalar zz = trace(e(t_wild_pval))
$Col_3_reg
	outreg2 margin_actual using "$main/Table-VoteOLSAdmin-LitReg-$spec", $OR2_addtext addstat(wild p value,zz) append

* Columna 4
global Col_4_regressors "margin_actual $demogs"
global Col_4_reg "reg vote_admin1 $Col_4_regressors, cluster(ppstaten)"
	$Col_4_reg
	bootwildct $Col_4_regressors, bootreps($BReps) numvars(1) seed(123)
	scalar zz = trace(e(t_wild_pval))
$Col_4_reg
	outreg2 margin_actual using "$main/Table-VoteOLSAdmin-LitReg-$spec", $OR2_addtext addstat(wild p value,zz) append

* Columna 5	
global Col_5_regressors "margin_actual $PastVoteVars $demogs"
global Col_5_reg "reg vote_admin1 $Col_5_regressors, cluster(ppstaten)"
	$Col_5_reg
	bootwildct $Col_5_regressors, bootreps($BReps) numvars(1) seed(123)
	scalar zz = trace(e(t_wild_pval))
$Col_5_reg
	outreg2 margin_actual using "$main/Table-VoteOLSAdmin-LitReg-$spec", $OR2_addtext addstat(wild p value,zz) append		
	

/*================================================
Apéndice Tabla C11 con bootstrap de bloque
=================================================*/		
set more off
qui do "$main/zz_globals_e2010.do"
use "$main/2010_RCT.dta", clear
keep if t_close==0 | t_close==1
sort ppstaten
by ppstaten: gen st_count = _N
collapse vote_admin1 vote_admin_past margin_actual st_count, by(ppstaten)
reg vote_admin1 margin_actual, vce(bootstrap, cluster(ppstaten) reps($BBReps) seed(123))
	outreg2 margin_actual using "$main/Table-VoteOLSAdmin-LitReg-BBStrap-$spec", $OR2 replace
reg vote_admin1 margin_actual vote_admin_past, vce(bootstrap, cluster(ppstaten) reps($BBReps) seed(123))
	outreg2 margin_actual vote_admin_past using "$main/Table-VoteOLSAdmin-LitReg-BBStrap-$spec", $OR2 append	
set more off
qui do "$main/zz_globals_e2010.do"
use "$main/2010_RCT.dta", clear	
reg vote_admin1 margin_actual, vce(bootstrap, cluster(ppstaten) reps($BBReps) seed(123))
	outreg2 margin_actual using "$main/Table-VoteOLSAdmin-LitReg-BBStrap-$spec", $OR2 append	
reg vote_admin1 margin_actual $demogs, vce(bootstrap, cluster(ppstaten) reps($BBReps) seed(123))	
	outreg2 margin_actual using "$main/Table-VoteOLSAdmin-LitReg-BBStrap-$spec", $OR2 append		
reg vote_admin1 margin_actual $demogs $PastVoteVars, vce(bootstrap, cluster(ppstaten) reps($BBReps) seed(123))	
	outreg2 margin_actual using "$main/Table-VoteOLSAdmin-LitReg-BBStrap-$spec", $OR2 append			


/*==================================================================
OLS: CREENCIAS SOBRE LA CERCANÍA ELECTORAL Y LA PARTICIPACIÓN DE VOTANTES
==================================================================*/
		
/*================================================
TABLA C10: Relación OLS de creencias y participación

Se muestran los resultados de tres maneras principales:
(1) Base: FE estatales y controles de votaciones pasadas.
(2) Agregar creencias previas al tratamiento.
(3) Agregar demostraciones.

kX = Regresor que se analiza
     ~ foreach kX en votemarg prob_100votes prob_1000votes prob_xvotes {
=================================================*/
set more off
qui do "$main/zz_globals_e2010.do"
use "$main/2010_RCT.dta", clear
foreach kX in votemarg prob_100votes prob_1000votes prob_xvotes {
xi: reg vote_admin1 `kX'_post $BaseVars_V2010, robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 `kX'_post using "$main/Table-VoteOLSAdmin-$spec", $OR2_addtext addstat(Mean DV, zz) append
xi: reg vote_admin1 `kX'_post `kX'_pre $BaseVars_V2010, robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 `kX'_post `kX'_pre using "$main/Table-VoteOLSAdmin-$spec", $OR2_addtext addstat(Mean DV, zz) append
xi: reg vote_admin1 `kX'_post `kX'_pre $FullCont_V2010, robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 `kX'_post `kX'_pre using "$main/Table-VoteOLSAdmin-$spec", $OR2_addtext addstat(Mean DV, zz) append
}


/*================================================
Tabla C12: DEMOGRAFÍA Y PARTICIPACIÓN.
=================================================*/
set more off
qui do "$main/zz_globals_e2010.do"
use "$main/2010_RCT.dta", clear
areg vote_admin1 votemarg_post $demogs, absorb(ppstaten) robust
	outreg2 using "$main/Table-VoteAdminOLS-Demogs", $OR2 bdec(3) replace
areg vote_admin1 votemarg_post $demogs $PastVoteVars, absorb(ppstaten) robust
	outreg2 using "$main/Table-VoteAdminOLS-Demogs", $OR2 bdec(3) append
	
	
/*==================================================================
IV: EFECTOS DE LAS CREENCIAS SOBRE LA CERCANÍA ELECTORAL EN LA PARTICIPACIÓN
==================================================================*/

/*================================================
Se muestran los resultados de tres maneras principales:
(1) Base: FE estatales y controles de votaciones pasadas.
(2) Agregar creencias previas al tratamiento.
(3) Agregar demostraciones.

~ TABLA 4: Resultados basales. Utilice "todos".
~ Tabla C19: Excluir personas que siempre votaron previamente en los datos. Usa "~votar_siempre".
~ FN 43: Excluir a las personas que siempre votaron o nunca votaron. Usa "votar_soloalgunos".
~ Tabla C27: Usar personas que no cambian prob de votar D. Usar "prob_votedem_change==0".
~ FN 45: Restringido a personas con poco interés en el gobierno. Utilice "int_govt_01==0".
~ Tabla C24: Restringir a personas con alto interés en el gobierno. Utilice "int_govt_01".
~ FN 45: Utilizar personas con ideología política media. Utilice "ideología_medio".
~ Tabla C23: Restringir a personas con fuerte ideología. Utilice "ideología_partidista".
~ Tabla C25: Restringir a personas en estados con electorados más pequeños. Utilice "pequeño_estado".
~ No utilizado: Restringido a personas con estatus de votante activo. Utilice "vs_activo".
~ Cuadro C18. Utilice la mitad de la encuesta de toma de muestra más cercana a la elección. Utilice "half_closest_to_election".
~ Discutido en el texto en la Sección 4A: restringido a los estados donde se usó
una encuesta cerrada 50/50. Utilice "estado_cerrar50".

Se necesita recorrer todas las opciones a continuación para producir las diferentes tablas.

yX = Restricción muestral o submuestra
   ~ foreach yX en todos ~vote_always vote_onlysome ~vote_never prob_votedem_change==0 int_govt_01==0 int_govt_01 ideologia_media ideologia_partidista estado_pequeno vs_medio_activo_mas_cercano_a_la_eleccion estado_cerrar50{
kX = Regresor que se analiza
   ~ foreach kX en votemarg prob_100votes prob_1000votes prob_xvotes {
================================================*/
set more off
qui do "$main/zz_globals_e2010.do"
foreach yX in all {
use "$main/2010_RCT.dta", clear
keep if `yX'==1
foreach kX in votemarg prob_100votes prob_1000votes prob_xvotes {
xi: ivreg2 vote_admin1 (`kX'_post=t_close) $BaseVars_V2010, first savefirst robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 `kX'_post using "$main/Table-VoteIVAdmin-`yX'-$spec", $OR2_fstatmdv append
	est restore _ivreg2_`kX'_post
	outreg2 t_close using "$main/Table-VoteIVAdmin-FS-`yX'-$spec", $OR2 append	
xi: ivreg2 vote_admin1 (`kX'_post=t_close) `kX'_pre $BaseVars_V2010,  first savefirst robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 `kX'_post `kX'_pre using "$main/Table-VoteIVAdmin-`yX'-$spec", $OR2_fstatmdv append
	est restore _ivreg2_`kX'_post
	outreg2 t_close using "$main/Table-VoteIVAdmin-FS-`yX'-$spec", $OR2 append	
xi: ivreg2 vote_admin1 (`kX'_post=t_close) `kX'_pre $FullCont_V2010,  first savefirst robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 `kX'_post `kX'_pre using "$main/Table-VoteIVAdmin-`yX'-$spec", $OR2_fstatmdv append
	est restore _ivreg2_`kX'_post
	outreg2 t_close using "$main/Table-VoteIVAdmin-FS-`yX'-$spec", $OR2 append		
}
}

	
/*================================================
Mencionado al final del Apéndice A2: Registro de creencias.
También se menciona al final de la Tabla C22.
yX = Restricción muestral o submuestra
   ~ foreach yX en total{
kX = Regresor que se analiza
   ~ foreach kX en votemarg prob_100votes {
   ~ foreach kX en prob_1000votos {
   ~ foreach kX en prob_xvotes {
================================================*/		
set more off
qui do "$main/zz_globals_e2010.do"
foreach yX in all{
use "$main/2010_RCT.dta", clear
keep if `yX'==1	
foreach kX in votemarg prob_100votes {
xi: ivreg2 vote_admin1 (l`kX'_post=t_close) $BaseVars_V2010, first savefirst robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 l`kX'_post using "$main/Table-VoteIVAdmin-Log-`yX'-$spec", $OR2_fstatmdv append
	est restore _ivreg2_l`kX'_post
	outreg2 t_close using "$main/Table-VoteIVAdmin-Log-FS-`yX'-$spec", $OR2 append	
xi: ivreg2 vote_admin1 (l`kX'_post=t_close) l`kX'_pre $BaseVars_V2010,  first savefirst robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 l`kX'_post l`kX'_pre using "$main/Table-VoteIVAdmin-Log-`yX'-$spec", $OR2_fstatmdv append
	est restore _ivreg2_l`kX'_post
	outreg2 t_close using "$main/Table-VoteIVAdmin-Log-FS-`yX'-$spec", $OR2 append	
xi: ivreg2 vote_admin1 (l`kX'_post=t_close) l`kX'_pre $FullCont_V2010,  first savefirst robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 l`kX'_post l`kX'_pre using "$main/Table-VoteIVAdmin-Log-`yX'-$spec", $OR2_fstatmdv append
	est restore _ivreg2_l`kX'_post
	outreg2 t_close using "$main/Table-VoteIVAdmin-Log-FS-`yX'-$spec", $OR2 append		
}
foreach kX in prob_1000votes {
xi: ivreg2 vote_admin1 (l`kX'_post=t_close) $BaseVars_V2010, first savefirst robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 l`kX'_post using "$main/Table-VoteIVAdmin-Log-`yX'-$spec", $OR2_fstatmdv append
	est restore _ivreg2_l`kX'_pos
	outreg2 t_close using "$main/Table-VoteIVAdmin-Log-FS-`yX'-$spec", $OR2 append	
xi: ivreg2 vote_admin1 (l`kX'_post=t_close) l`kX'_pre $BaseVars_V2010,  first savefirst robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 l`kX'_post l`kX'_pre using "$main/Table-VoteIVAdmin-Log-`yX'-$spec", $OR2_fstatmdv append
	est restore _ivreg2_l`kX'_pos
	outreg2 t_close using "$main/Table-VoteIVAdmin-Log-FS-`yX'-$spec", $OR2 append	
xi: ivreg2 vote_admin1 (l`kX'_post=t_close) l`kX'_pre $FullCont_V2010,  first savefirst robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 l`kX'_post l`kX'_pre using "$main/Table-VoteIVAdmin-Log-`yX'-$spec", $OR2_fstatmdv append
	est restore _ivreg2_l`kX'_pos
	outreg2 t_close using "$main/Table-VoteIVAdmin-Log-FS-`yX'-$spec", $OR2 append		
}
foreach kX in prob_xvotes {
xi: ivreg2 vote_admin1 (l`kX'_post=t_close) $BaseVars_V2010, first savefirst robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 l`kX'_post using "$main/Table-VoteIVAdmin-Log-`yX'-$spec", $OR2_fstatmdv append
	est restore _ivreg2_l`kX'_post
	outreg2 t_close using "$main/Table-VoteIVAdmin-Log-FS-`yX'-$spec", $OR2 append	
xi: ivreg2 vote_admin1 (l`kX'_post=t_close) l`kX'_pre $BaseVars_V2010,  first savefirst robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 l`kX'_post l`kX'_pre using "$main/Table-VoteIVAdmin-Log-`yX'-$spec", $OR2_fstatmdv append
	est restore _ivreg2_l`kX'_post
	outreg2 t_close using "$main/Table-VoteIVAdmin-Log-FS-`yX'-$spec", $OR2 append	
xi: ivreg2 vote_admin1 (l`kX'_post=t_close) l`kX'_pre $FullCont_V2010,  first savefirst robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 l`kX'_post l`kX'_pre using "$main/Table-VoteIVAdmin-Log-`yX'-$spec", $OR2_fstatmdv append
	est restore _ivreg2_l`kX'_post
	outreg2 t_close using "$main/Table-VoteIVAdmin-Log-FS-`yX'-$spec", $OR2 append		
}
}	
	
/*================================================
Apéndice Tabla C22: Registro de creencias y restricción a personas
que actualizan creencias

yX = Restricción muestral o submuestra
   ~ foreach yX en total{
kX = Regresor que se analiza
   ~ foreach kX en votemarg prob_100votes {
   ~ foreach kX en prob_1000votos {
   ~ foreach kX en prob_xvotes {
================================================*/		
set more off
qui do "$main/zz_globals_e2010.do"
foreach yX in all{
foreach kX in votemarg prob_100votes {
use "$main/2010_RCT.dta", clear
keep if `yX'==1	
keep if `kX'_change!=0
xi: ivreg2 vote_admin1 (l`kX'_post=t_close) $BaseVars_V2010, first savefirst robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 l`kX'_post using "$main/Table-VoteIVAdminCB-Log-`yX'-$spec", $OR2_fstatmdv append
	est restore _ivreg2_l`kX'_post
	outreg2 t_close using "$main/Table-VoteIVAdminCB-Log-FS-`yX'-$spec", $OR2 append	
xi: ivreg2 vote_admin1 (l`kX'_post=t_close) l`kX'_pre $BaseVars_V2010,  first savefirst robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 l`kX'_post l`kX'_pre using "$main/Table-VoteIVAdminCB-Log-`yX'-$spec", $OR2_fstatmdv append
	est restore _ivreg2_l`kX'_post
	outreg2 t_close using "$main/Table-VoteIVAdminCB-Log-FS-`yX'-$spec", $OR2 append	
xi: ivreg2 vote_admin1 (l`kX'_post=t_close) l`kX'_pre $FullCont_V2010,  first savefirst robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 l`kX'_post l`kX'_pre using "$main/Table-VoteIVAdminCB-Log-`yX'-$spec", $OR2_fstatmdv append
	est restore _ivreg2_l`kX'_post
	outreg2 t_close using "$main/Table-VoteIVAdminCB-Log-FS-`yX'-$spec", $OR2 append		
}
foreach kX in prob_1000votes {
use "$main/2010_RCT.dta", clear
keep if `kX'_change!=0
xi: ivreg2 vote_admin1 (l`kX'_post=t_close) $BaseVars_V2010, first savefirst robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 l`kX'_post using "$main/Table-VoteIVAdminCB-Log-`yX'-$spec", $OR2_fstatmdv append
	est restore _ivreg2_l`kX'_pos
	outreg2 t_close using "$main/Table-VoteIVAdminCB-Log-FS-`yX'-$spec", $OR2 append	
xi: ivreg2 vote_admin1 (l`kX'_post=t_close) l`kX'_pre $BaseVars_V2010,  first savefirst robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 l`kX'_post l`kX'_pre using "$main/Table-VoteIVAdminCB-Log-`yX'-$spec", $OR2_fstatmdv append
	est restore _ivreg2_l`kX'_pos
	outreg2 t_close using "$main/Table-VoteIVAdminCB-Log-FS-`yX'-$spec", $OR2 append	
xi: ivreg2 vote_admin1 (l`kX'_post=t_close) l`kX'_pre $FullCont_V2010,  first savefirst robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 l`kX'_post l`kX'_pre using "$main/Table-VoteIVAdminCB-Log-`yX'-$spec", $OR2_fstatmdv append
	est restore _ivreg2_l`kX'_pos
	outreg2 t_close using "$main/Table-VoteIVAdminCB-Log-FS-`yX'-$spec", $OR2 append		
}
foreach kX in prob_xvotes {
use "$main/2010_RCT.dta", clear
keep if `kX'_change!=0
xi: ivreg2 vote_admin1 (l`kX'_post=t_close) $BaseVars_V2010, first savefirst robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 l`kX'_post using "$main/Table-VoteIVAdminCB-Log-`yX'-$spec", $OR2_fstatmdv append
	est restore _ivreg2_l`kX'_post
	outreg2 t_close using "$main/Table-VoteIVAdminCB-Log-FS-`yX'-$spec", $OR2 append	
xi: ivreg2 vote_admin1 (l`kX'_post=t_close) l`kX'_pre $BaseVars_V2010,  first savefirst robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 l`kX'_post l`kX'_pre using "$main/Table-VoteIVAdminCB-Log-`yX'-$spec", $OR2_fstatmdv append
	est restore _ivreg2_l`kX'_post
	outreg2 t_close using "$main/Table-VoteIVAdminCB-Log-FS-`yX'-$spec", $OR2 append	
xi: ivreg2 vote_admin1 (l`kX'_post=t_close) l`kX'_pre $FullCont_V2010,  first savefirst robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 l`kX'_post l`kX'_pre using "$main/Table-VoteIVAdminCB-Log-`yX'-$spec", $OR2_fstatmdv append
	est restore _ivreg2_l`kX'_post
	outreg2 t_close using "$main/Table-VoteIVAdminCB-Log-FS-`yX'-$spec", $OR2 append		
}
}			
		
		
/*================================================
Tabla C21: Restringir a personas que actualicen sus creencias.
kX = Regresor que se analiza
   ~ foreach kX en votemarg prob_100votes prob_1000votes prob_xvotes {
================================================*/			
set more off
qui do "$main/zz_globals_e2010.do"
foreach kX in votemarg prob_100votes prob_1000votes prob_xvotes {
use "$main/2010_RCT.dta", clear
keep if `kX'_change!=0
xi: ivreg2 vote_admin1 (`kX'_post=t_close) $BaseVars_V2010,  first robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 `kX'_post using "$main/Table-VoteIVAdmin-ChangeBeliefs-$spec", $OR2_fstatmdv append
xi: ivreg2 vote_admin1 (`kX'_post=t_close) `kX'_pre $BaseVars_V2010,  first robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 `kX'_post `kX'_pre using "$main/Table-VoteIVAdmin-ChangeBeliefs-$spec", $OR2_fstatmdv append	
xi: ivreg2 vote_admin1 (`kX'_post=t_close) `kX'_pre $FullCont_V2010,  first robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 `kX'_post `kX'_pre using "$main/Table-VoteIVAdmin-ChangeBeliefs-$spec", $OR2_fstatmdv append
}	
	

/*================================================
Tabla C13: No controle el historial de votaciones pasadas.
kX = Regresor que se analiza
   ~ foreach kX en votemarg prob_100votes prob_1000votes prob_xvotes {
================================================*/
set more off
qui do "$main/zz_globals_e2010.do"
use "$main/2010_RCT.dta", clear
foreach kX in votemarg prob_100votes prob_1000votes prob_xvotes {
xi: ivreg2 vote_admin1 (`kX'_post=t_close) $BaseVars_V2010_NoPastV,  first robust 
	outreg2 `kX'_post using "$main/Table-VoteIVAdmin-NoPastVoting-$spec", $OR2_fstat append
xi: ivreg2 vote_admin1 (`kX'_post=t_close) `kX'_pre $BaseVars_V2010_NoPastV,  first robust 
	outreg2 `kX'_post `kX'_pre using "$main/Table-VoteIVAdmin-NoPastVoting-$spec", $OR2_fstat append	
xi: ivreg2 vote_admin1 (`kX'_post=t_close) `kX'_pre $FullCont_V2010_NoPastV,  first robust 
	outreg2 `kX'_post `kX'_pre using "$main/Table-VoteIVAdmin-NoPastVoting-$spec", $OR2_fstat append
}

/*================================================
Tabla C17: Peso por día de encuesta
================================================*/
set more off
qui do "$main/zz_globals_e2010.do"
use "$main/2010_RCT.dta", clear
foreach kX in votemarg prob_100votes prob_1000votes prob_xvotes {
xi: ivreg2 vote_admin1 (`kX'_post=t_close) $BaseVars_V2010 [aw=day_survey], first savefirst robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 `kX'_post using "$main/Table-VoteIVAdmin-AWDayOfSurvey-$spec", $OR2_fstatmdv append
	est restore _ivreg2_`kX'_post
	outreg2 t_close using "$main/Table-VoteIVAdmin-FS-AWDayOfSurvey-$spec", $OR2 append	
xi: ivreg2 vote_admin1 (`kX'_post=t_close) `kX'_pre $BaseVars_V2010 [aw=day_survey],  first savefirst robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 `kX'_post `kX'_pre using "$main/Table-VoteIVAdmin-AWDayOfSurvey-$spec", $OR2_fstatmdv append
	est restore _ivreg2_`kX'_post
	outreg2 t_close using "$main/Table-VoteIVAdmin-FS-AWDayOfSurvey-$spec", $OR2 append	
xi: ivreg2 vote_admin1 (`kX'_post=t_close) `kX'_pre $FullCont_V2010 [aw=day_survey],  first savefirst robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1
	egen zz = max(zz1)
	outreg2 `kX'_post `kX'_pre using "$main/Table-VoteIVAdmin-AWDayOfSurvey-$spec", $OR2_fstatmdv append
	est restore _ivreg2_`kX'_post
	outreg2 t_close using "$main/Table-VoteIVAdmin-FS-AWDayOfSurvey-$spec", $OR2 append		
}


/*================================================
OTRAS VARIABLES DE RESULTADO ADEMÁS DE LA PARTICIPACIÓN DEL ADMINISTRADOR.

~ TABLA 5: INTENCIÓN DE VOTO INMEDIATA DESPUÉS DEL TRATAMIENTO.
La intención de voto="pvote_post" está en una escala de 0 a 100.

~ Tabla C16: Adquisición de Información.
Para la adquisición de información, se preguntó a las personas si comenzaron a prestar más o menos atención a las campañas después de realizar la encuesta preelectoral. 
================================================*/
set more off
qui do "$main/zz_globals_e2010.do"
use "$main/2010_RCT.dta", clear
assert pvote_post>=0 & pvote_post<=100 if ~mi(pvote_post)
foreach yX in pvote_post attention_change1 {
foreach kX in votemarg prob_100votes prob_1000votes prob_xvotes {
xi: ivreg2 `yX' (`kX'_post=t_close) $BaseVars_V2010,  first robust 
	capture drop zz zz1
	egen zz1 = mean(`yX') if e(sample)==1
	egen zz = max(zz1)
	outreg2 `kX'_post using "$main/Table-VoteIV`yX'-$spec", $OR2_fstatmdv append
xi: ivreg2 `yX' (`kX'_post=t_close) `kX'_pre $BaseVars_V2010,  first robust 
	capture drop zz zz1
	egen zz1 = mean(`yX') if e(sample)==1
	egen zz = max(zz1)
	outreg2 `kX'_post `kX'_pre using "$main/Table-VoteIV`yX'-$spec", $OR2_fstatmdv append	
xi: ivreg2 `yX' (`kX'_post=t_close) `kX'_pre $FullCont_V2010,  first robust 
	capture drop zz zz1
	egen zz1 = mean(`yX') if e(sample)==1
	egen zz = max(zz1)
	outreg2 `kX'_post `kX'_pre using "$main/Table-VoteIV`yX'-$spec", $OR2_fstatmdv append
}	
}	

* En la Sección 2C, apartes sobre la correlación entre la probabilidad de voto prevista 
* y voto administrativo, y sobre los medios.
set more off
use "$main/2010_RCT.dta", clear
pwcorr pvote_post vote_admin
assert round(r(rho),.01)==0.46
sum pvote_post if t_close==1
sum pvote_post if t_close==0

		
/*================================================
Cálculo de estadísticas resumidas simples sobre cuánto tiempo le tomó a la gente responder los márgenes de voto y las preguntas de los problemas electorales muy cerrados, así como la cantidad de tiempo que pasó mirando las encuestas. 
================================================*/			

* En la Sección 4A, se señala que las personas se tomaron un tiempo significativo para responder las preguntas sobre creencias.
* Estos también se mencionan en el Apéndice A.3.
set more off
use "$main/2010_RCT.dta", clear
sum time_votemarg_pre, detail
assert r(p50)==35 & r(p10)==19 & r(p90)==78
sum time_prob_xvotes_pre, detail
assert r(p50)==16 & r(p10)==9 & r(p90)==36

* Tiempo dedicado a mirar la pantalla principal con las encuestas.
* Esto se discute en FN 26.
use "$main/2010_RCT.dta", clear
sum time_on_poll, detail
assert round(r(p50))==16
gen time_on_poll_overmin = (time_on_poll>60) if ~mi(time_on_poll)
sum time_on_poll_overmin
assert round(r(mean),.01)==.03

		
/*==================================================================
Tabla C29: ANÁLISIS DE FORMA REDUCIDA 2010.
==================================================================*/
set more off
qui do "$main/zz_globals_e2010.do"
use "$main/2010_RCT.dta", clear
xi: reg vote_admin1 t_close $BaseVars_V2010, robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1 & t_close==0
	egen zz = max(zz1)	
	outreg2 t_close using "$main/Table-VoteAdmin-RedForm-$spec", $OR2_addtext addstat(Mean DV if not close poll=1, zz) replace
xi: reg vote_admin1 t_close $FullCont_V2010, robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1 & t_close==0
	egen zz = max(zz1)
	outreg2 t_close using "$main/Table-VoteAdmin-RedForm-$spec", $OR2_addtext addstat(Mean DV if not close poll=1, zz) append
* Se controló por "estado" en lugar de "ppstaten" porque falta "ppstaten" para las personas que no respondieron la encuesta.
xi: reg vote_admin1 t_assign_close t_assign_notclose i.state $PastVoteVars, robust 
	capture drop zz zz1
	egen zz1 = mean(vote_admin1) if e(sample)==1 & t_assign_control==1
	egen zz = max(zz1)	
	outreg2 t_assign_close t_assign_notclose using "$main/Table-VoteAdmin-RedForm-$spec", $OR2_addtext addstat(Mean DV if assigned to control=1, zz) append	
	

/*================================================================== 
ANÁLISIS DE VALENCIA: ¿CÓMO AFECTAN LAS PREFERENCIAS LAS ENCUESTAS?
- ¿Cómo afecta ver que el demócrata es más favorecido si la gente vota por los demócratas (autoinformado en la encuesta postelectoral) o la probabilidad prevista de votar por los demócratas en la encuesta preelectoral?
==================================================================*/ 

/*================================================
FN 42: "La correlación entre el porcentaje de votos demócratas previsto posterior al tratamiento y el porcentaje de votos demócratas real es de 0,42".
================================================*/
set more off
qui do "$main/zz_globals_e2010.do"	
use "$main/2010_RCT.dta", clear
pwcorr demshare_post demshare
assert round(r(rho),.01)==0.42

/*================================================
Tabla C31: Votación real por el demócrata (basada en una encuesta posterior a la selección)
================================================*/
set more off
qui do "$main/zz_globals_e2010.do"	
use "$main/2010_RCT.dta", clear
reg demshare_post poll_dem $BaseDemVars, robust
	outreg2 poll_dem using "$main/Table-VoteDem-$spec", $OR2_fstat replace
xi: reg vote_govdem1 demshare_post $BaseDemVars, robust
	outreg2 demshare_post using "$main/Table-VoteDem-$spec", $OR2_fstat append
gen insamp = e(sample)==1
reg demshare_post poll_dem $BaseDemVars if insamp==1, robust
xi: ivreg2 vote_govdem1 (demshare_post= poll_dem) $BaseDemVars,  first robust
	outreg2 demshare_post using "$main/Table-VoteDem-$spec", $OR2_fstat append
xi: ivreg2 vote_govdem1 (demshare_post= poll_dem)  demshare_pre $BaseDemVars,  first robust
	outreg2 demshare_post demshare_pre using "$main/Table-VoteDem-$spec", $OR2_fstat append	
xi: ivreg2 vote_govdem1 (demshare_post= poll_dem)  demshare_pre $FullDemCont,  first robust
	outreg2 demshare_post demshare_pre using "$main/Table-VoteDem-$spec", $OR2_fstat append	
	
		
/*================================================
Tabla C32: Intención Probabilidad de Votar por Demócrata
================================================*/	
set more off
qui do "$main/zz_globals_e2010.do"	
use "$main/2010_RCT.dta", clear
xi: reg prob_votedem_post demshare_post $BaseDemVars, robust
	outreg2 demshare_post using "$main/Table-VoteDemIntent-$spec", $OR2_fstat replace
xi: ivreg2 prob_votedem_post (demshare_post= poll_dem) $BaseDemVars,  first robust
	outreg2 demshare_post using "$main/Table-VoteDemIntent-$spec", $OR2_fstat append
xi: ivreg2 prob_votedem_post (demshare_post= poll_dem)  demshare_pre $BaseDemVars,  first robust
	outreg2 demshare_post demshare_pre using "$main/Table-VoteDemIntent-$spec", $OR2_fstat append	
xi: ivreg2 prob_votedem_post (demshare_post= poll_dem)  demshare_pre $FullDemCont,  first robust
	outreg2 demshare_post demshare_pre using "$main/Table-VoteDemIntent-$spec", $OR2_fstat append	
	
		



