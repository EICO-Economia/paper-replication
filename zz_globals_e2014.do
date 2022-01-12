/*==================================================================
 Program: zz_globals_e2014.do
 Purpose: Creates globals used in analyzing the 2014 RCT.
==================================================================*/ 
 
/*================================================
Building Block Control variables.
================================================*/

* Past voting variables
global PastVoteVars "vote2008 vote2010 vote2012"

* The age variable is never missing
global AgeVars "age_2534 age_3544 age_4554 age_5564 age_6574 age_75pl"

* Demographic controls: gender, race, age, party registration.
global DemogVars "male1 black1 hisp1 race_other1 $AgeVars reg_dem1 reg_rep1 reg_unknown"

**** Outregging variables.
global OR2_nodec "se nocons nonotes noaster label ctitle(" ") addtext("") tex(frag) slow(50)"
global OR2_nodec_nomdv "se nocons nonotes noaster label ctitle(" ") addtext("") tex(frag) slow(50)"

/*================================================
Turnout control variables
================================================*/
global BaseVars_V2014 "$PastVoteVars"
global FullCont_V2014 "$BaseVars_V2014 $DemogVars"
