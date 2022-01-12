****
*TAREA DE REPRODUCIBILIDAD (PARTE II) - C
*ECONOMETRÍA APLICADA

*CAMILO TORRES ALMUNA


***********************************************************************************************
***********************************************************************************************
*One in a Million: Field Experiments on Perceived Closeness of the Election and Voter Turnout
***********************************************************************************************

clear all
set more off
winexec taskkill /F /IM Notepad.exe /T 

*Defino el directorio principal:
global main "C:\Users\camilo.torres\Documents\ECONOMIA\MAGISTER EN ANALISIS ECONOMICO (U. de Chile)\CURSOS\3. ECONOMETRÍA APLICADA I\Evaluaciones\Tarea de replicación\Paper asignado"


/*==================================================================
 Programa: Combo_Tables.do
 Propósito: análisis que combina las RCT de 2010 y 2014.
==================================================================*/

/*==================================================================
PARTES DEL CÓDIGO:
1. COMBINE LOS DATOS DE 2014 Y 2010.
2. UTILIZANDO DATOS DE 2014: HACER TSIV Y ESTIMAR "s".
3. UTILIZANDO DATOS COMBINADOS: REALICE TSIV Y ESTIME "s".
4. UTILIZANDO DATOS DE 2010: REPITA LOS RESULTADOS PRINCIPALES IV Y ESTIME "s".
5. ANÁLISIS VARIOS
==================================================================*/

* Supuesta relación observacional entre cercanía y participación
scalar B_ClosenessTurnout = .34

* Supuesto porcentaje de impacto en las creencias para el RCT de 2014 en relación con 
* el tamaño del impacto en el RCT de 2010. Nuestra especificación base asume Theta=1. 
scalar Theta = 1

* Coeficientes de correlación. Discutido en el Apéndice Web FN 12.
scalar Rho12 = 0.5
scalar Rho13 = 0.5
scalar Rho23 = 0.5


/*==================================================================
1. COMBINE LOS DATOS DE 2014 Y 2010.
==================================================================*/
set more off
use "$main/2014_RCT.dta", clear
gen year = 2014
* Deshágase de las observaciones en el grupo de control de 2014.
drop if mi(t_close)
append using "$main/2010_RCT.dta"
replace year = 2010 if mi(year)
* Deshágase de las personas que no obtuvieron la encuesta de 2010.
drop if mi(t_close)
save "$main/BothYears.dta", replace



/*==================================================================
2. USAR DATOS DE 2014: REALIZAR TSIV Y ESTIMAR "s".

Pasos:
- Obtenga estimaciones sobre cómo el margen real afecta las creencias utilizando los datos de 2010.
- Estimar cómo el tratamiento afecta la participación, columna 4 de datos de 2014 (formato reducido).
- Estimar cómo afecta el tratamiento a las creencias utilizando datos de 2010 (primera etapa).
- Elaborar las estimaciones del TSIV (TABLA 7).
- Calcular "s" con base en datos de 2014 (Panel B de la TABLA 8). La idea de estimar "s" nos la sugirió Jesse Shapiro.
==================================================================*/
set more off

matrix input B_F = (0 \ 0 \ 0 \ 0)
matrix SE_F = B_F
matrix V_F = B_F
matrix B_TSIV = B_F
matrix SE_TSIV = B_F
matrix DelBel = B_F	

* Obtenga estimaciones sobre los cambios en las creencias, DelBel.
local zX = 1
qui do "$main/zz_globals_e2010.do"
use "$main/BothYears.dta", clear
foreach kX in votemarg prob_100votes prob_1000votes prob_xvotes {
	reg `kX'_pre $BeliefControls, robust
	matrix DelBel[`zX',1]  = -10*_b[margin_actual]
	local ++zX
}

* Obtener los presupuestos de forma reducida. Columna 4 del ECA 2014 (Tabla 6).
qui do "$main/zz_globals_e2014.do"
use "$main/BothYears.dta", clear
areg vote100 t_close t_fewvoters $FullCont_V2014, absorb(strata) cluster(hhid_unique)
matrix B_R = _b[t_close]
matrix SE_R = _se[t_close]
matrix V_R = SE_R*SE_R

* Obtener las estimaciones de la primera etapa.
* Columna 2 de la Tabla 2. 
qui do "$main/zz_globals_e2010.do"
use "$main/BothYears.dta", clear
areg votemarg_post t_close $demogs, robust absorb(ppstaten) 
matrix B_F[1,1]  =_b[t_close] * Theta 
matrix SE_F[1,1] =_se[t_close] * Theta 
matrix V_F[1,1]  = SE_F[1,1]*SE_F[1,1] 

* Columnas 3, 6 y 9 de la Tabla 3.
local zX = 2
foreach yX in prob_100votes prob_1000votes prob_xvotes {
	di `zX'
	areg `yX'_post t_close `yX'_pre $demogs, robust absorb(ppstaten) 
	matrix B_F[`zX',1]=_b[t_close] * Theta 
	matrix SE_F[`zX',1]=_se[t_close] * Theta 
	matrix V_F[`zX',1] = SE_F[`zX',1]*SE_F[`zX',1]
	local ++zX
}

* B_TSIV calculado dividiendo la forma reducida por la primera etapa.
* SE_TSIV calculado usando el Método Delta.
forvalues iX = 1/4 {
	matrix B_TSIV[`iX',1] = B_R / B_F[`iX',1]
	matrix SE_TSIV[`iX',1] = abs(1/B_F[`iX',1])*sqrt( V_R[1,1] + V_F[`iX',1] * ((B_R[1,1]/B_F[`iX',1])^2) )
}
matrix CI_L = B_TSIV - 1.96*SE_TSIV
matrix CI_H = B_TSIV + 1.96*SE_TSIV

* Salida de los resultados de TSIV (ESTOS VAN EN LA TABLA 7 EN EL TEXTO PRINCIPAL)
matrix Block = ( .,.,.,. )		
matrix Regs_TSIV = ( Block 	\ Block \ Block \ Block \ Block \ Block \ Block \ Block)
matrix rowname Regs_TSIV = Predicted_vote_margin SE Pr(Marg_<100_votes) SE Pr(Marg_<1000_votes) SE <100_or_1,000_votes SE
forvalues iX=1/4{
	matrix Regs_TSIV[2*`iX'-1,`iX']=round(B_TSIV[`iX',1],.01)
	matrix Regs_TSIV[2*`iX',`iX']=round(SE_TSIV[`iX',1],.01)
}
outtable using "$main/Tab-TSIV14-$spec", mat(Regs_TSIV) nobox replace center 
	
* Intervalos de confianza para la Tabla 7. Discutidos en la Sección 3.
matrix Block = ( .,.,.,. )		
matrix CIs_TSIV = ( Block 	\ Block \ Block \ Block \ Block \ Block \ Block \ Block)
matrix rowname CIs_TSIV = Predicted_vote_margin SE Pr(Marg_<100_votes) SE Pr(Marg_<1000_votes) SE <100_or_1,000_votes SE
forvalues iX=1/4{
	matrix CIs_TSIV[2*`iX'-1,`iX']=round(CI_L[`iX',1],.01)
	matrix CIs_TSIV[2*`iX',`iX']=round(CI_H[`iX',1],.01)
}	
outtable using "$main/Tab-TSIV14-CIs-$spec", mat(CIs_TSIV) nobox replace center 	
	
		
* Envíe los resultados de la primera etapa en Theta (parte inferior de la TABLA 7 EN EL TEXTO PRINCIPAL).	
matrix Block = ( .,.,.,. )		
matrix Regs_F = ( Block \ Block )	
forvalues iX=1/4{
	matrix Regs_F[1,`iX']=round(B_F[`iX',1],.01)
	matrix Regs_F[2,`iX']=round(SE_F[`iX',1],.01)
}	
outtable using "$main/Tab-FS-TSIV14-$spec", mat(Regs_F) nobox replace center 	
	
matrix Max_Impact = ( CI_L[1,1]*DelBel[1,1] \ CI_H[2,1]*DelBel[2,1] \ CI_H[3,1]*DelBel[3,1] \ CI_H[4,1]*DelBel[4,1] )
matrix Avg_Impact = ( B_TSIV[1,1]*DelBel[1,1] \ B_TSIV[2,1]*DelBel[2,1] \ B_TSIV[3,1]*DelBel[3,1] \ B_TSIV[4,1]*DelBel[4,1] )
matrix Min_Impact = ( CI_H[1,1]*DelBel[1,1] \ CI_L[2,1]*DelBel[2,1] \ CI_L[3,1]*DelBel[3,1] \ CI_L[4,1]*DelBel[4,1] )

matrix S_Share_Avg = Avg_Impact / (10*B_ClosenessTurnout)
foreach yX in Min Max {
	matrix S_Share_`yX' = `yX'_Impact / (10*B_ClosenessTurnout)
}
matrix S_Avg14 = S_Share_Avg

matrix S_SE = (S_Share_Avg-S_Share_Min)/1.96
scalar h_marg = 1/(S_SE[1,1]^2)
scalar h_100 = 1/(S_SE[2,1]^2)
scalar h_1k = 1/(S_SE[3,1]^2)
scalar h_1001k = 1/(S_SE[4,1]^2)
scalar Denom_3 = h_marg + h_100 + h_1k
scalar Denom_2 = h_marg + h_1001k

* Usar margen de voto previsto, probabilidad de menos de 100, probabilidad de menos de 1000 votos.
scalar S_Comobo3 = (h_marg*S_Share_Avg[1,1] + h_100*S_Share_Avg[2,1] + h_1k*S_Share_Avg[3,1])/ Denom_3
scalar S_Comobo3_SE = sqrt(1/Denom_3)
scalar S_Combo3_Min = round(S_Comobo3-1.96*S_Comobo3_SE,.01)
scalar S_Combo3_Max = round(S_Comobo3+1.96*S_Comobo3_SE,.01)
scalar S_Combo3_n = round((h_marg*S_Share_Avg[1,1] + h_100*S_Share_Avg[2,1] + h_1k*S_Share_Avg[3,1])/ Denom_3,.01)
matrix S_Overall = [0,0,0,0,0,S_Combo3_n,S_Combo3_Min,S_Combo3_Max]

scalar S_Comobo3_SE_Rho = sqrt((1/Denom_3) + (2/(Denom_3^2))*(Rho12*sqrt(h_marg)*sqrt(h_100)+Rho13*sqrt(h_marg)*sqrt(h_1k)+Rho23*sqrt(h_100)*sqrt(h_1k)))
scalar S_Combo3_Min_Rho = round(S_Comobo3-1.96*S_Comobo3_SE_Rho,.01)
scalar S_Combo3_Max_Rho = round(S_Comobo3+1.96*S_Comobo3_SE_Rho,.01)
matrix S_Overall_Rho = [0,0,0,0,0,S_Combo3_n,S_Combo3_Min_Rho,S_Combo3_Max_Rho]


* Utilice el margen de voto previsto y la probabilidad de menos de 100/1000 votos.
scalar S_Comobo2 = (h_marg*S_Share_Avg[1,1] + h_1001k*S_Share_Avg[4,1])/ Denom_2
scalar S_Comobo2_SE = sqrt(1/Denom_2)
scalar S_Combo2_Min = round(S_Comobo2-1.96*S_Comobo2_SE,.01)
scalar S_Combo2_Max = round(S_Comobo2+1.96*S_Comobo2_SE,.01)
scalar S_Combo2_n = round((h_marg*S_Share_Avg[1,1] + h_1001k*S_Share_Avg[4,1])/ Denom_2,.01)
matrix S_Overall2 = [0,0,0,0,0,S_Combo2_n,S_Combo2_Min,S_Combo2_Max]

forvalues iX=1/4{
	matrix DelBel[`iX',1]=round(DelBel[`iX',1],.1)
	matrix CI_L[`iX',1]=round(CI_L[`iX',1],.01)
	matrix CI_H[`iX',1]=round(CI_H[`iX',1],.01)
	matrix Min_Impact[`iX',1]=round(Min_Impact[`iX',1],.1)
	matrix Max_Impact[`iX',1]=round(Max_Impact[`iX',1],.1)
	matrix S_Share_Avg[`iX',1]=round(S_Share_Avg[`iX',1],.01)
	matrix S_Share_Min[`iX',1]=round(S_Share_Min[`iX',1],.01)
	matrix S_Share_Max[`iX',1]=round(S_Share_Max[`iX',1],.01)
}

matrix Shapiro14 = (DelBel, CI_L, CI_H, Min_Impact, Max_Impact, S_Share_Avg, S_Share_Min, S_Share_Max)
matrix Shapiro14 = (Shapiro14 \ S_Overall \ S_Overall2 \ S_Overall_Rho)
matrix rowname Shapiro14 = Predicted_vote_margin Pr(Marg_<100_votes) Pr(Marg_<1000_votes) <100_or_1,000_votes Overall Overall_2Measures Overall_Rho
matrix colname Shapiro14 = DelBel CI-Low CI-Hi Min-impact Max-impact Point-estimate-s Min-s Max-s
outtable using "$main/Tab-Shapiro14-$spec", mat(Shapiro14) nobox replace center 

/*==================================================================
3. UTILIZANDO DATOS COMBINADOS: HACER TSIV Y ESTIMAR "s". 
Esto crea el Panel C de la TABLA 8.

¿Cuáles son los estados en común a lo largo de los dos años?
2010: CA, CT, FL, GA, IL, MD, NH, NY, OH, OR, PA, TX, and WI
2014: AR, FL, GA, KS, MA, MI, and WI
Hay 3 estados en común: FL, GA, and WI.
==================================================================*/
set more off
matrix input B_F = (0 \ 0 \ 0 \ 0)
matrix SE_F = B_F
matrix V_F = B_F
matrix B_TSIV = B_F
matrix SE_TSIV = B_F
matrix DelBel = B_F		/* Cambios en las creencias */


* Obtener estimaciones sobre los cambios en las creencias
local zX = 1
qui do "$main/zz_globals_e2010.do"
use "$main/BothYears.dta", clear
foreach kX in votemarg prob_100votes prob_1000votes prob_xvotes {
	reg `kX'_pre $BeliefControls, robust
	matrix DelBel[`zX',1]  = -10*_b[margin_actual]
	local ++zX
}


set more off
use "$main/BothYears.dta", clear
gen vote_new = . 
replace vote_new = vote100 if year==2014
replace vote_new = vote_admin1 if year==2010
label variable vote_new "Admin vote (0/1) multiplied by 100"

* Se usó 10^9 para la ID de 2010 para asegurar de que el número sea muy diferente.
gen hhid_new = .
replace hhid_new  = hhid_unique if year==2014
replace hhid_new  = (10^9) + caseid if year==2010
replace vote2008 = vote_admin2008 if year==2010
foreach yX in male black hisp race_other {
	replace `yX'1=`yX' if year==2010
}

gen state_id_combo = .
replace state_id_combo = ppstaten if year==2010
replace state_id_combo = state_new if year==2014
replace state_id_combo = 59 if state=="FL"
replace state_id_combo = 58 if state=="GA"
replace state_id_combo = 35 if state=="WI"

save "$main/BothYears_Combo.dta", replace

* Obtenga las estimaciones de forma reducida en los datos agrupados. 
* Esto se informa en la última fila del Panel C de la Tabla 8.
use "$main/BothYears_Combo.dta", clear
reg vote_new t_close vote_admin_past male1 black1 hisp1 $AgeVars i.year i.state_id_combo, cluster(hhid_new)
matrix B_R = _b[t_close]
matrix SE_R = _se[t_close]
matrix V_R = SE_R*SE_R

* Obtener las estimaciones de la primera etapa.
* Columna 2 de la Tabla 2. 
qui do "$main/zz_globals_e2010.do"
use "$main/BothYears.dta", clear
areg votemarg_post t_close $demogs, robust absorb(ppstaten) 
matrix B_F[1,1]  =_b[t_close] * Theta 
matrix SE_F[1,1] =_se[t_close] * Theta 
matrix V_F[1,1]  = SE_F[1,1]*SE_F[1,1] 

* Columnas 3, 6 y 9 de la Tabla 3.
local zX = 2
foreach yX in prob_100votes prob_1000votes prob_xvotes {
	di `zX'
	areg `yX'_post t_close `yX'_pre $demogs, robust absorb(ppstaten) 
	matrix B_F[`zX',1]=_b[t_close] * Theta 
	matrix SE_F[`zX',1]=_se[t_close] * Theta 
	matrix V_F[`zX',1] = SE_F[`zX',1]*SE_F[`zX',1]
	local ++zX
}
forvalues iX = 1/4 {
	matrix B_TSIV[`iX',1] = B_R / B_F[`iX',1]
	matrix SE_TSIV[`iX',1] = abs(1/B_F[`iX',1])*sqrt( V_R[1,1] + V_F[`iX',1] * ((B_R[1,1]/B_F[`iX',1])^2) )
}
matrix CI_L = B_TSIV - 1.96*SE_TSIV
matrix CI_H = B_TSIV + 1.96*SE_TSIV

* Salida de los resultados de TSIV
matrix Block = ( .,.,.,. )		
matrix Regs_TSIV = ( Block 	\ Block \ Block \ Block \ Block \ Block \ Block \ Block)
matrix rowname Regs_TSIV = Predicted_vote_margin SE Pr(Marg_<100_votes) SE Pr(Marg_<1000_votes) SE <100_or_1,000_votes SE
forvalues iX=1/4{
	matrix Regs_TSIV[2*`iX'-1,`iX']=round(B_TSIV[`iX',1],.01)
	matrix Regs_TSIV[2*`iX',`iX']=round(SE_TSIV[`iX',1],.01)
}
outtable using "$main/Tab-TSIV-Combo-$spec", mat(Regs_TSIV) nobox replace center 

matrix Max_Impact = ( CI_L[1,1]*DelBel[1,1] \ CI_H[2,1]*DelBel[2,1] \ CI_H[3,1]*DelBel[3,1] \ CI_H[4,1]*DelBel[4,1] )
matrix Avg_Impact = ( B_TSIV[1,1]*DelBel[1,1] \ B_TSIV[2,1]*DelBel[2,1] \ B_TSIV[3,1]*DelBel[3,1] \ B_TSIV[4,1]*DelBel[4,1] )
matrix Min_Impact = ( CI_H[1,1]*DelBel[1,1] \ CI_L[2,1]*DelBel[2,1] \ CI_L[3,1]*DelBel[3,1] \ CI_L[4,1]*DelBel[4,1] )

matrix S_Share_Avg = Avg_Impact / (10*B_ClosenessTurnout)
foreach yX in Min Max {
	matrix S_Share_`yX' = `yX'_Impact / (10*B_ClosenessTurnout)
}
matrix S_Avg14 = S_Share_Avg

matrix S_SE = (S_Share_Avg-S_Share_Min)/1.96
scalar h_marg = 1/(S_SE[1,1]^2)
scalar h_100 = 1/(S_SE[2,1]^2)
scalar h_1k = 1/(S_SE[3,1]^2)
scalar h_1001k = 1/(S_SE[4,1]^2)
scalar Denom_3 = h_marg + h_100 + h_1k
scalar Denom_2 = h_marg + h_1001k

* Usar margen de voto previsto, probabilidad de menos de 100, probabilidad de menos de 1000 votos.
scalar S_Comobo3 = (h_marg*S_Share_Avg[1,1] + h_100*S_Share_Avg[2,1] + h_1k*S_Share_Avg[3,1])/ Denom_3
scalar S_Comobo3_SE = sqrt(1/Denom_3)
scalar S_Combo3_Min = round(S_Comobo3-1.96*S_Comobo3_SE,.01)
scalar S_Combo3_Max = round(S_Comobo3+1.96*S_Comobo3_SE,.01)
scalar S_Combo3_n = round((h_marg*S_Share_Avg[1,1] + h_100*S_Share_Avg[2,1] + h_1k*S_Share_Avg[3,1])/ Denom_3,.01)
matrix S_Overall = [0,0,0,0,0,S_Combo3_n,S_Combo3_Min,S_Combo3_Max]

scalar S_Comobo3_SE_Rho = sqrt((1/Denom_3) + (2/(Denom_3^2))*(Rho12*sqrt(h_marg)*sqrt(h_100)+Rho13*sqrt(h_marg)*sqrt(h_1k)+Rho23*sqrt(h_100)*sqrt(h_1k)))
scalar S_Combo3_Min_Rho = round(S_Comobo3-1.96*S_Comobo3_SE_Rho,.01)
scalar S_Combo3_Max_Rho = round(S_Comobo3+1.96*S_Comobo3_SE_Rho,.01)
matrix S_Overall_Rho = [0,0,0,0,0,S_Combo3_n,S_Combo3_Min_Rho,S_Combo3_Max_Rho]

* Utilice el margen de voto previsto y la probabilidad de menos de 100/1000 votos.
scalar S_Comobo2 = (h_marg*S_Share_Avg[1,1] + h_1001k*S_Share_Avg[4,1])/ Denom_2
scalar S_Comobo2_SE = sqrt(1/Denom_2)
scalar S_Combo2_Min = round(S_Comobo2-1.96*S_Comobo2_SE,.01)
scalar S_Combo2_Max = round(S_Comobo2+1.96*S_Comobo2_SE,.01)
scalar S_Combo2_n = round((h_marg*S_Share_Avg[1,1] + h_1001k*S_Share_Avg[4,1])/ Denom_2,.01)
matrix S_Overall2 = [0,0,0,0,0,S_Combo2_n,S_Combo2_Min,S_Combo2_Max]


forvalues iX=1/4{
	matrix DelBel[`iX',1]=round(DelBel[`iX',1],.1)
	matrix CI_L[`iX',1]=round(CI_L[`iX',1],.01)
	matrix CI_H[`iX',1]=round(CI_H[`iX',1],.01)
	matrix Min_Impact[`iX',1]=round(Min_Impact[`iX',1],.1)
	matrix Max_Impact[`iX',1]=round(Max_Impact[`iX',1],.1)
	matrix S_Share_Avg[`iX',1]=round(S_Share_Avg[`iX',1],.01)
	matrix S_Share_Min[`iX',1]=round(S_Share_Min[`iX',1],.01)
	matrix S_Share_Max[`iX',1]=round(S_Share_Max[`iX',1],.01)
}

matrix ShapiroCombo = (DelBel, CI_L, CI_H, Min_Impact, Max_Impact, S_Share_Avg, S_Share_Min, S_Share_Max)
matrix ShapiroCombo = (ShapiroCombo \ S_Overall \ S_Overall2 \ S_Overall_Rho)
matrix rowname ShapiroCombo = Predicted_vote_margin Pr(Marg_<100_votes) Pr(Marg_<1000_votes) <100_or_1,000_votes Overall Overall_2Measures Overall_Rho
matrix colname ShapiroCombo = DelBel CI-Low CI-Hi Min-impact Max-impact Point-estimate-s Min-s Max-s
outtable using "$main/Tab-ShapiroCombo-$spec", mat(ShapiroCombo) nobox replace center 



/*==================================================================
4. UTILIZANDO DATOS DE 2010: REPETIR LOS RESULTADOS PRINCIPALES IV Y ESTIMAR "s".
Esto crea el Panel A de la TABLA 8.
==================================================================*/
set more off
matrix input B_F = (0 \ 0 \ 0 \ 0)
matrix B_IV = B_F
matrix SE_IV = B_F
matrix DelBel = B_F

* Obtener estimaciones sobre los cambios en las creencias
local zX = 1
qui do "$main/zz_globals_e2010.do"
use "$main/BothYears.dta", clear
foreach kX in votemarg prob_100votes prob_1000votes prob_xvotes {
	reg `kX'_pre $BeliefControls, robust
	matrix DelBel[`zX',1]  = -10*_b[margin_actual]
	local ++zX
}

* Obtenga las estimaciones IV de cómo las creencias afectan la participación.
set more off
qui do "$main/zz_globals_e2010.do"
use "$main/2010_RCT.dta", clear

local zX = 1
foreach kX in votemarg prob_100votes prob_1000votes prob_xvotes {
xi: ivreg2 vote_admin1 (`kX'_post=t_close) `kX'_pre $FullCont_V2010,  first robust  
	matrix B_IV[`zX',1] = _b[`kX'_post]
	matrix SE_IV[`zX',1] = _se[`kX'_post]
	local ++zX
}

matrix CI_L = B_IV - 1.96*SE_IV
matrix CI_H = B_IV + 1.96*SE_IV
matrix Max_Impact = ( CI_L[1,1]*DelBel[1,1] \ CI_H[2,1]*DelBel[2,1] \ CI_H[3,1]*DelBel[3,1] \ CI_H[4,1]*DelBel[4,1] )
matrix Avg_Impact = ( B_IV[1,1]*DelBel[1,1] \ B_IV[2,1]*DelBel[2,1] \ B_IV[3,1]*DelBel[3,1] \ B_IV[4,1]*DelBel[4,1] )
matrix Min_Impact = ( CI_H[1,1]*DelBel[1,1] \ CI_L[2,1]*DelBel[2,1] \ CI_L[3,1]*DelBel[3,1] \ CI_L[4,1]*DelBel[4,1] )

matrix S_Share_Avg = Avg_Impact / (10*B_ClosenessTurnout)
foreach yX in Min Max {
	matrix S_Share_`yX' = `yX'_Impact / (10*B_ClosenessTurnout)
}
matrix S_Avg10 = S_Share_Avg

matrix list S_Share_Avg
matrix list S_Share_Min

matrix S_SE = (S_Share_Avg-S_Share_Min)/1.96
scalar h_marg = 1/(S_SE[1,1]^2)
scalar h_100 = 1/(S_SE[2,1]^2)
scalar h_1k = 1/(S_SE[3,1]^2)
scalar h_1001k = 1/(S_SE[4,1]^2)
scalar Denom_3 = h_marg + h_100 + h_1k
scalar Denom_2 = h_marg + h_1001k

* Usar margen de voto previsto, probabilidad de menos de 100, probabilidad de menos de 1000 votos.
scalar S_Comobo3 = (h_marg*S_Share_Avg[1,1] + h_100*S_Share_Avg[2,1] + h_1k*S_Share_Avg[3,1])/ Denom_3
scalar S_Comobo3_SE = sqrt(1/Denom_3)
scalar S_Combo3_Min = round(S_Comobo3-1.96*S_Comobo3_SE,.01)
scalar S_Combo3_Max = round(S_Comobo3+1.96*S_Comobo3_SE,.01)
scalar S_Combo3_n = round((h_marg*S_Share_Avg[1,1] + h_100*S_Share_Avg[2,1] + h_1k*S_Share_Avg[3,1])/ Denom_3,.001)
matrix S_Overall = [0,0,0,0,0,S_Combo3_n,S_Combo3_Min,S_Combo3_Max]

scalar S_Comobo3_SE_Rho = sqrt((1/Denom_3) + (2/(Denom_3^2))*(Rho12*sqrt(h_marg)*sqrt(h_100)+Rho13*sqrt(h_marg)*sqrt(h_1k)+Rho23*sqrt(h_100)*sqrt(h_1k)))
scalar S_Combo3_Min_Rho = round(S_Comobo3-1.96*S_Comobo3_SE_Rho,.01)
scalar S_Combo3_Max_Rho = round(S_Comobo3+1.96*S_Comobo3_SE_Rho,.01)
matrix S_Overall_Rho = [0,0,0,0,0,S_Combo3_n,S_Combo3_Min_Rho,S_Combo3_Max_Rho]

* Utilice el margen de voto previsto y la probabilidad de menos de 100/1000 votos.
scalar S_Comobo2 = (h_marg*S_Share_Avg[1,1] + h_1001k*S_Share_Avg[4,1])/ Denom_2
scalar S_Comobo2_SE = sqrt(1/Denom_2)
scalar S_Combo2_Min = round(S_Comobo2-1.96*S_Comobo2_SE,.01)
scalar S_Combo2_Max = round(S_Comobo2+1.96*S_Comobo2_SE,.01)
scalar S_Combo2_n = round((h_marg*S_Share_Avg[1,1] + h_1001k*S_Share_Avg[4,1])/ Denom_2,.01)
matrix S_Overall2 = [0,0,0,0,0,S_Combo2_n,S_Combo2_Min,S_Combo2_Max]

forvalues iX=1/4{
	matrix DelBel[`iX',1]=round(DelBel[`iX',1],.1)
	matrix CI_L[`iX',1]=round(CI_L[`iX',1],.01)
	matrix CI_H[`iX',1]=round(CI_H[`iX',1],.01)
	matrix Min_Impact[`iX',1]=round(Min_Impact[`iX',1],.1)
	matrix Max_Impact[`iX',1]=round(Max_Impact[`iX',1],.1)
	matrix S_Share_Avg[`iX',1]=round(S_Share_Avg[`iX',1],.01)
	matrix S_Share_Min[`iX',1]=round(S_Share_Min[`iX',1],.01)
	matrix S_Share_Max[`iX',1]=round(S_Share_Max[`iX',1],.01)
}

matrix Shapiro10 = (DelBel, CI_L, CI_H, Min_Impact, Max_Impact, S_Share_Avg, S_Share_Min, S_Share_Max)
matrix Shapiro10 = (Shapiro10 \ S_Overall \ S_Overall2 \ S_Overall_Rho)
matrix rowname Shapiro10 = Predicted_vote_margin Pr(Marg_<100_votes) Pr(Marg_<1000_votes) <100_or_1,000_votes Overall Overall_2Measures Overall_Rho
matrix colname Shapiro10 = DelBel CI-Low CI-Hi Min-impact Max-impact Point-estimate-s Min-s Max-s
outtable using "$main/Tab-Shapiro10-$spec", mat(Shapiro10) nobox replace center 

/*==================================================================
5. ANÁLISIS VARIOS
===================================================================*/

/*================================================
En la Sección 4B, se informa que los valores estimados de "s" son pequeños y no diferentes de 0.
También se calcula el promedio de las 8 filas completas.
================================================*/
matrix S_Avg_Combo = [S_Avg10 \ S_Avg14]
mat U = J(rowsof(S_Avg_Combo),1,1)
mat S_Avg = (U'*S_Avg_Combo)/rowsof(S_Avg_Combo)
outtable using "$main/Tab-Shapiro-AvgS-$spec", mat(S_Avg) nobox replace center 


/*================================================
FN 34: COMPARAR LAS TASAS DE VOTACIÓN ENTRE 2010 Y 2014. 

state_id_combo = 59 corresponde a "FL".
state_id_combo = 58 corresponde a "GA".
state_id_combo = 35 corresponde a "WI".
================================================*/
use "$main/BothYears_Combo.dta", clear

foreach kX in 59 58 35 {
sum vote_admin1 if state_id_combo==`kX'
assert year==2010 if ~mi(vote_admin1) & state_id_combo==`kX'
sum vote2010 if state_id_combo==`kX' & age>=22
assert year==2014 if ~mi(vote2010) & state_id_combo==`kX'
}

gen both_years = (state_id_combo==59 | state_id_combo==58 | state_id_combo==35)
sum vote_admin1 if both_years==1
sum vote2010 if both_years==1 & age>=22