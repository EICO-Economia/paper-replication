/*==================================================================
 Program: zz_globals_e2010.do
 Purpose: Creates globals used in analyzing the 2010 RCT.
==================================================================*/ 


/*================================================
Miscellaneous scalars
================================================*/
* This is the time we add for outreg2, to avoid errors.
global Slow_T = 650


/*================================================
Building Block Control variables.
================================================*/ 

* Age: omitted category is age 18-24.
global AgeVars "age_2534 age_3544 age_4554 age_5564 age_6574 age_75pl"

* Education: omitted category is high school graduate
global EducVars "dropout somecoll bachelors mastersphd"

* Combined variables
global demogs_minuseduc "male black hisp other mixed $AgeVars inc_2550 inc_5075 inc_75100 inc_100p"
global demogs "$demogs_minuseduc $EducVars"

* Past voting variables
global PastVoteVars "vote_admin2000 vote_admin2002 vote_admin2004 vote_admin2006 vote_admin2008"

* Party registration variables
global PartyRegVars "democrat_reg republican_reg otherparty_reg"

* Political interest controls
global Polit_Controls "int_govt_scale pelosi vote_admin_past"


/*================================================
Outregging globals.
================================================*/
global OR2 "se dec(2) nocons nonotes noaster label ctitle(" ") addtext("") tex(frag) slow($Slow_T)"
global OR2_fstat "se dec(2) nocons nonotes noaster label  ctitle(" ") addtext("") tex(frag) addstat(F-stat on excl instrument, e(widstat)) slow($Slow_T)"
global OR2_addtext "se dec(2) nocons nonotes noaster label ctitle(" ") tex(frag) slow($Slow_T)"
global OR2_dec3 "se dec(3) nocons nonotes noaster label ctitle(" ") addtext("") tex(frag) slow($Slow_T)"
global OR2_fstatmdv "se dec(2) nocons nonotes noaster label ctitle(" ") addtext("") tex(frag) addstat(F-stat on excl instrument,e(widstat),Mean DV,zz) slow($Slow_T)"



/*================================================
Belief analysis control variables:
================================================*/
global BeliefControls "margin_actual coins_fringe logelecsize dem_1to7 int_govt_scale  $demogs"


/*================================================
Turnout control variables
================================================*/
global BaseVars_V2010_NoPastV "i.ppstaten"
global FullCont_V2010_NoPastV "$demogs $BaseVars_V2010_NoPastV"

global BaseVars_V2010 "$BaseVars_V2010_NoPastV $PastVoteVars"
global FullCont_V2010 "$demogs $BaseVars_V2010"

global BaseVars_V2010_NoSFE "$PastVoteVars"
global FullCont_V2010_NoSFE "$demogs $BaseVars_V2010_NoSFE"


/*================================================
Democrat vote share controls
================================================*/
global BaseDemVars "prob_votedem_pre i.ppstaten"
global FullDemCont "$demogs $BaseDemVars"


/*================================================
Bootstrap replications.
================================================*/
set seed 1234

* Wild bootstrap reps
global BReps "2000"

* Block bootstrap reps
global BBReps "500"

