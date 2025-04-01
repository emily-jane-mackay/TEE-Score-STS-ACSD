
log using "`path'`file_name'.txt", text replace 



// Load the .dta raw data
use "`path'`file_name'.dta", clear 

// Understand Dataset
duplicates report acsdid 

// admit source
tab admitsrc, miss
generate admit_source = " " 
replace admit_source = "elective" if admitsrc == 1
replace admit_source = "ER" if admitsrc == 2
replace admit_source = "transfer" if admitsrc == 3
replace admit_source = "other" if admitsrc == 4
replace admit_source = "unknown" if admitsrc == . 
tab admit_source, miss

// dialysis 
tab dialysis, miss 
generate dialysis_pre = cond((dialysis == 1), 1, 0)
tab dialysis_pre, miss 

// select important pre-surgical variables
tab prcvint, miss

// pocpci 
tab pocpci, miss 
generate prev_pci = cond((pocpci == 1), 1, 0)
tab prev_pci, miss 

// pocpcin
tab pocpcin, miss 
tab pocpciin if pocpci == 1, miss  
generate pci_6_hrs_less = cond((pocpciin == 1 & pocpci == 1), 1, 0)
tab pci_6_hrs_less, miss 

// pocpcist
tab pocpcist, miss
tab pocpcist if pocpci == 1, miss 
generate pci_stent = cond((pocpcist == 1 & pocpci == 1), 1, 0)
tab pci_stent, miss 

// pocpciwhen
tab pocpciwhen, miss
generate pci_osh = cond((pocpciwhen == 2 & pocpci == 1), 1, 0) 
tab pci_osh pocpciwhen, miss
tab pci_osh datavrsn, miss
* note: osh pci is evenly distributed across versions 
generate pci_in_house = cond((pocpciwhen == 1), 1, 0)
tab pci_in_house pocpciwhen, miss
tab pci_in_house datavrsn, miss
* note: fewer in house PCIs in version 4.20.2 than 2.9 or 2.81 

// pocpcindsurg
tab pocpcindsurg, miss 
// pci/CABG hybrid cases - reason 
generate pci_ind_surgery = " " 
replace pci_ind_surgery = "complic-deterioration" if (pocpcindsurg == 1 | pocpcindsurg == 5) & (pocpciwhen == 1 | pocpciwhen == 2)
replace pci_ind_surgery = "complic-no-deterioration" if (pocpcindsurg == 2) & (pocpciwhen == 1 | pocpciwhen == 2)
replace pci_ind_surgery = "planned-staged-STEMI" if (pocpcindsurg == 4) & (pocpciwhen == 1 | pocpciwhen == 2)
replace pci_ind_surgery = "planned-staged-no-STEMI" if (pocpcindsurg == 3) & (pocpciwhen == 1 | pocpciwhen == 2)
replace pci_ind_surgery = "other-reason" if (pocpcindsurg == 9) 

// previous CABG 
generate prev_cabg = cond((prcab == 1), 1, 0)

// previous valve 
generate prev_valve = cond((prvalve == 1), 1, 0)

// previous other cardiac surgery
generate prev_cardiac_srg = cond((poc == 1), 1, 0)
tab prev_cardiac_srg, miss

// previous mi
tab prevmi, miss 
generate prev_mi = cond((prevmi == 1), 1, 0) 

// cardiac symptoms at the time of admission
tab cardsymptimeofadm, miss
generate cardiac_sympt_adm = " " 
replace cardiac_sympt_adm = "no_sympt" if cardsymptimeofadm == 1
replace cardiac_sympt_adm = "angina_stable" if cardsymptimeofadm == 2
replace cardiac_sympt_adm = "angina_unstable" if cardsymptimeofadm == 3
replace cardiac_sympt_adm = "NSTEMI" if cardsymptimeofadm == 4 
replace cardiac_sympt_adm = "STEMI" if cardsymptimeofadm == 5
replace cardiac_sympt_adm = "angina_equiv" if cardsymptimeofadm == 6
replace cardiac_sympt_adm = "other_sympt" if cardsymptimeofadm == 7
replace cardiac_sympt_adm = "unknown" if cardsymptimeofadm == . 

// consolidate 
generate stemi_nstemi = cond((cardiac_sympt_adm == "NSTEMI" | cardiac_sympt_adm == "STEMI"), 1, 0)

// chronic lung disease
tab chrlungd, miss
generate chronic_lung_p = " " 
replace chronic_lung_p = "no_disease" if chrlungd == 1 
replace chronic_lung_p = "mild" if chrlungd == 2
replace chronic_lung_p = "moderate" if chrlungd == 3
replace chronic_lung_p = "severe" if chrlungd == 4 
replace chronic_lung_p = "unclassified" if chrlungd == 5
replace chronic_lung_p = "unknown" if (chrlungd == 6 | chrlungd == .)  
tab chronic_lung_p, miss 

// obstructive sleep apnea
tab slpapn, miss
generate osa_p = cond((slpapn == 1), 1, 0)

// liver disease
tab liverdis, miss
generate liver_p = cond((liverdis == 1), 1, 0)

// cvd pre-surgery
tab cvd, miss 
generate cvd_p = cond((cvd == 1), 1, 0)

// cva pre-surgery 
tab cvawhen, miss
gen cva_p = cond((cvawhen == 3 | cvawhen == 4), 1, 0)
tab cva_p, miss 

// arrhythmia 
tab arrhythmia, miss
gen arrhythmia_p = cond((arrhythmia == 1 | arrhythmia == 3), 1, 0)

// arrhythmia - afib
tab arrhythafib, miss 
// generate paroxysmal afib for all years
gen afib_parox = cond((arrhythafib == 2), 1, 0)
tab afib_parox surgyear, miss 
// generate persisten afib for all years 
gen afib_persist = cond((arrhythafib == 3 | arrhythafib == 4 | arrhythafib == 5 | arrhythafib == 6), 1, 0)
tab afib_persist surgyear, miss 

// nyha class 
tab classnyh, miss 
generate nyha = " " 
replace nyha = "1" if classnyh == 1
replace nyha = "2" if classnyh == 2 
replace nyha = "3" if classnyh == 3
replace nyha = "4" if classnyh == 4 
tab classnyh, miss 

// generate chf
tab nyha, miss 
generate chf_p = cond((nyha == "1" | nyha == "2" | nyha == "3" | nyha == "4"), 1, 0)
tab chf_p, miss 

// pvd 
tab pvd, miss
generate pvd_p = cond((pvd == 1), 1, 0)
tab pvd_p, miss 

// cardiogenic shock pre-surgery 
tab carshock, miss
generate cardio_shock = cond((carshock == 3 | carshock == 4), 1, 0)
tab cardio_shock, miss 

// coronary dominance
tab dominance, miss 
generate dominance_cor = " " 
replace dominance_cor = "left" if dominance == 1
replace dominance_cor = "right" if dominance == 2
replace dominance_cor = "co-dominant" if dominance == 3
replace dominance_cor = "not_documented" if dominance == 4
replace dominance_cor = "unknown" if dominance == 4

// number of diseased vessels
tab numdisv, miss 
generate num_diseased_v = " " 
replace num_diseased_v = "zero" if numdisv == 1 
replace num_diseased_v = "one" if numdisv == 2
replace num_diseased_v = "two" if numdisv == 3 
replace num_diseased_v = "three" if numdisv == 4
replace num_diseased_v = "not_documented" if numdisv == . 
tab num_diseased_v, miss 

// ace inhibitor within 48 hours of surgery 
tab medacei48, miss
generate ace_i_48 = cond((medacei48 == 1), 1, 0)

// beta blocker within 24 hours of surgery 
tab medbeta, miss 
generate beta_24 = cond((medbeta == 1), 1, 0) 

// inotropic medications pre-surgery 
tab medinotr, miss 
generate inotrop_48 = cond((medinotr == 1), 1, 0)

// % stenosis left main (for years 2014 - 2020)
summarize pctstenlmain, detail 
// stenleftmain (for years 2021 - 2022)
tab stenleftmain, miss 
// generate variable that captures left main disease >=50%
generate left_main_50 = cond((stenleftmain == 1 | (pctstenlmain >= 50 & pctstenlmain <100)), 1, 0)
tab left_main_50, miss 

// pulmonary systolic pressure > 30 mmHgs
summarize pasys, detail 
generate pa_sys_30 = cond((pasys >= 30 & pasys != .), 1, 0)
tab pa_sys_30, miss 

// EF 
summarize hdef, detail 

// lvedd 
summarize lvedd, detail 

// aortic valve insufficiency grade
tab vdinsufa, miss 
generate av_insuff = " " 
replace av_insuff = "trace" if vdinsufa == 1
replace av_insuff = "mild" if vdinsufa == 2
replace av_insuff = "moderate" if vdinsufa == 3
replace av_insuff = "severe" if vdinsufa == 4
tab av_insuff, miss

// mitral valve insufficiency grade 
tab vdinsufm, miss
generate mv_insuff = " " 
replace mv_insuff = "trace" if vdinsufm == 1
replace mv_insuff = "mild" if vdinsufm == 2
replace mv_insuff = "moderate" if vdinsufm == 3
replace mv_insuff = "severe" if vdinsufm == 4
tab mv_insuff, miss 

// tricuspid valve insufficiency grade 
tab vdinsuft, miss 
generate tv_insuff = " " 
replace tv_insuff = "trace" if vdinsuft == 1
replace tv_insuff = "mild" if vdinsuft == 2
replace tv_insuff = "moderate" if vdinsuft == 3
replace tv_insuff = "severe" if vdinsuft == 4
tab tv_insuff, miss 

// redo cardiac surgery 
tab incidenc, miss
generate redo_srg = cond((incidenc == 2 | incidenc == 3 | incidenc == 4 | incidenc == 5), 1, 0)

// operative status
tab status, miss
generate op_status = " " 
replace op_status = "elective" if status == 1 
replace op_status = "urgent" if status == 2 
replace op_status = "emergent" if status == 3
replace op_status = "emergent_salvage" if status == 4 
replace op_status = "unknown" if status == . 
tab op_status, miss 

////////////////////////////////////////////////////////////////////////////////

// exposure: tee 

////////////////////////////////////////////////////////////////////////////////

// exposure 
tab inoptee, miss 
// generate tee
generate tee = cond((inoptee == 1), 1, 0)
tab tee
// capture obs missing tee 
generate tee_missing = cond((inoptee == .), 1, 0)
tab tee_missing, miss

////////////////////////////////////////////////////////////////////////////////

// outcomes (potential) 

////////////////////////////////////////////////////////////////////////////////

// all-cause death (defined as a death realated or unrelated to the surgery itself; in OR, in hospital, or after discharge) within 30 days of surgery
generate death_all_30 = cond((dischmortstat == 2 | dischmortstat == 4 | mortalty == 1 | mt30stat ==2 | surgtomort <= 30), 1, 0)

// "operative" mortality - used in Metkus paper - not "in OR" but defined in the paper as follows:
// "Operative Mortality includes: (1) all deaths regardless of cause, occuring during the hospitalization in which the operation was performed, even if after 30 days (including patients transferred to other acute care facilities); and (2) all deaths, regardless of cause, occurring after discharge from the hospital, but before the end of the thirtieth postoperative day."
// problem: above description from Methods of Metkus paper matches variable "mtopd" but reported death distribution from Metkus does not match "mtopd" but instead matches "opmort(i)" 
// decision: use opmort | opmorti variable to define "operative" mortality 
generate death_opmort = cond((opmort == 1 | opmorti == 1), 1, 0)




// Secondary (potentials): 
// definite: chest washout; stroke_complic; renal_fail; 

// ventilator hours
summarize venthrstot, detail

// chest exploration post-surgery
tab coprebld, miss 
generate chest_washout = cond((coprebld == 1), 1, 0)

// stroke post-surgery
tab cnstrokp, miss
generate stroke_complic = cond((cnstrokp == 1 | cnstrokp == 3 | cnstrokp == 4 | cnstrokp == 5), 1, 0) 
tab stroke_complic, miss 

// prolonged ventilation
tab cpvntlng, miss 
generate prolonged_vent = cond((cpvntlng == 1), 1, 0)

// new renal failure post-operatively 
tab crenfail, miss 
generate renal_fail = cond((crenfail == 1), 1, 0) 

// new renal failure requiring dialysis post-operatively 
tab crendial, miss 
generate renal_dial = cond((crendial == 1), 1, 0)

// post-operative, unplanned coronary reintervention 
// creintmi for 2017 - 2022
tab creintmi surgyear, miss
// copregtf for 2014 - 2017 
tab copregft surgyear, miss
// combine 
generate cor_reint = cond((copregft == 3 | copregft == 4 | creintmi == 1), 1, 0)

// reop for valve dysfunction 
tab coprevlv, miss 
generate valve_reint = cond((coprevlv == 3 | coprevlv == 4), 1, 0)

// potential negative control outcomes
// dvt 
tab cvte, miss 
generate dvt_complic = cond((cvte == 1 | dvt == 1), 1, 0)
// atrial fibrillation - new onset, post-operative atrial fibrillation 
tab cotafib, miss
generate afib_new = cond((cotafib == 1), 1, 0)


// post-operative, pre-discharge TTE
tabulate popttech, miss 
generate tte_post = cond((popttech == 1), 1, 0)


// running potential independent covariate list (goal = 30-50)
summarize age, detail 
summarize bmi, detail 
tab admit_source, miss 

tab arrhythmia_p, miss 
tab afib_parox, miss 
tab afib_persist, miss
tab chronic_lung_p, miss 
tab chf_p, miss 
tab nyha, miss 
tab cvd_p, miss 
tab cva_p, miss 
tab dialysis_p, miss 
tab liver_p, miss 
tab nyha, miss 
tab osa_p, miss 
tab pa_sys_30, miss 
tab pvd_p, miss 

tab prev_pci, miss 
tab pci_6_hrs_less, miss
tab pci_stent, miss
tab pci_ind_surgery, miss
tab prev_mi, miss 
summarize cathtosurg, detail 

tab prev_cabg, miss 
tab redo_srg, miss 

summarize rfhemoglobin, detail 
summarize platelets, detail 
summarize totalbumin, detail 
summarize creatlst, detail 
summarize inr, detail 

tab left_main_50, miss 

tab cardiac_sympt_adm, miss
tab cardio_shock, miss
tab num_diseased_v, miss
tab ace_i_48, miss
tab beta_24, miss 
tab inotrop_48, miss 

summarize hdef, detail 
tab av_insuff 
tab mv_insuff
tab tv_insuff 
tab redo_srg
tab op_status 

// exposure
tab tee, miss 

// cohort count & exclude
// inclusion count: 1,321,030
count 
// cohort and exclusion counts 
// (1)
tab tee_missing, miss 
count if tee_missing == 1 
count if tee_missing == 0
// exclusion count: N = 11,732
generate exclude = cond((tee_missing == 1), 1, 0)
count if exclude != 1 
// inclusion count: N = 1,309,298


// (2) 
tab opvalve if exclude != 1, miss 
count if opvalve == 2 & exclude != 1
count if opvalve != 2 & exclude != 1
// exclusion count: N = 2619
replace exclude = cond((tee_missing == 1 | opvalve != 2), 1, 0)
count if exclude != 1 
// inclusion count: N = 1,306,679
// sanity check 
di 11732 + 2619
count if exclude == 1 

// (3) 
tab sts_hospid if exclude != 1, miss 
count if sts_hospid != . & exclude != 1 
count if sts_hospid == . & exclude != 1 
// exclusion count: N = 28,386
replace exclude = cond((tee_missing == 1 | opvalve != 2 | sts_hospid == .), 1, 0)
count if exclude != 1
// inclusion count: N = 1,278,293
// sanity check 
di 11732 + 2619 + 28386
count if exclude == 1

// (4) 
tab numdisv if exclude != 1, miss 
count if (numdisv != . | numdisv != 1) & exclude != 1 
count if (numdisv == . | numdisv == 1) & exclude != 1 
// exclusion count: N = 9879
replace exclude = cond((tee_missing == 1 | opvalve != 2 | sts_hospid == . | (numdisv == . | numdisv == 1)), 1, 0)
count if exclude != 1
// inclusion count: N = 1,268,414
// sanity check 
di 11732 + 2619 + 28386 + 9879
count if exclude == 1

// (5) 
tab op_status if exclude != 1, miss
count if (op_status != "unknown" | op_status != "emergent_salvage") & exclude != 1 
count if (op_status == "unknown" | op_status == "emergent_salvage") & exclude != 1 
// exclusion count: N = 2359
replace exclude = cond((tee_missing == 1 | opvalve != 2 | sts_hospid == . | (numdisv == . | numdisv == 1) | (op_status == "unknown" | op_status == "emergent_salvage")), 1, 0)
count if exclude != 1 
// inclusion count: N = 1,266,055
// sanity check 
di 11732 + 2619 + 28386 + 9879 + 2359
count if exclude == 1



// check & final cohort count 
count 
count if exclude == 1 /* exclusion count: N = 54,975 */
count if exclude != 1 /* inclusion count: N = 1,266,055 */ 

///////////////////////////////////////////////////////////////////////////////

log close _all


///////////////////////////////////////////////////////////////////////////////






 