***Create variables***
replace galtan ="5" if galtan=="center"
replace lrecon="5" if lrecon=="center"
destring galtan lrecon, replace

egen galtancount = rowtotal(p_environment p_justice p_social p_foreign p_interior)
egen econcount = rowtotal(p_economy p_labour p_finance p_education p_health p_budget)
egen portfoliototal = rownonmiss(p_foreign p_finance p_budget p_interior p_defence p_economy p_industry p_justice p_social p_labour p_health p_agriculture p_environment p_education)

gen galtanshareall = (galtancount / portfoliototal)*100
gen econshareall = (econcount / portfoliototal) * 100

gen rilenew = (rile+100)/20

gen year = year(survey_start_date)

egen countryid = group(country)
egen yearcntr = group(country year)

gen niche = 0
replace niche = 1 if parfam == 10 | parfam == 70 | parfam == 90 
//niche parties as ecologists, nationalists and ethno-regional parties (Meguid 2005)

gen survdur = survey_start_date - cabinet_start_date
gen survdurdich = .
replace survdurdich = 0 if survdur <= 365
replace survdurdich = 1 if survdur > 365

gen survdursmall = survdur/100

***Label everything***
label variable lrecon "ECON position"
label variable galtan "GALTAN position"
label variable rilenew "RILE"
label variable gov_status "Government status"
label define govstuff 1 "Opp. only" 2 "Gov. prev." 3 "Gov. now"
label value gov_status govstuff
label variable galtanshareall "Share GALTAN portfolios"
label variable econshareall "Share ECON portfolios"
label variable survdur "Diff. survey and cabinet start"
label variable survdurdich "Long duration"

log using gopaper, replace

***Table 1***
xtset countryid
xtreg avgper lrecon galtan rilenew relative_salience if gov_status == 3, fe
estimates store mm1

xtset countryid
xtreg avgper lrecon galtan rilenew relative_salience econshareall galtanshareall if gov_status == 3, fe
estimates store mm2

xtset countryid
xtreg avgper c.lrecon##c.econshareall c.galtan##c.galtanshareall rilenew relative_salience if gov_status == 3, fe
estimates store mm3

***Figure 2***
margins, dydx(galtan) at(galtanshareall=(0(10)50)) atmeans 
marginsplot, addplot(hist galtanshareall)

***Calculate example***
margins, at(galtanshareall=(30 40) galtan=9) atmeans

esttab mm1 mm2 mm3 using main.tex , b(3) se(3) cons r2 scalars("ll Log likelihood") label star(* 0.1 ** 0.05 *** 0.01)  parentheses  replace plain 

***Table 2***
xtset countryid
xtreg avgper c.lrecon##c.econshareall##survdurdich c.galtan##c.galtanshareall##survdurdich rilenew relative_salience if gov_status == 3, fe
estimates store new1

***Figure 3***
margins survdurdich, dydx(galtan) at(galtanshareall=(0(10)50)) atmeans
marginsplot, by(survdurdich) 

esttab new1 using new.tex , b(3) se(3) cons r2 scalars("ll Log likelihood") label star(* 0.1 ** 0.05 *** 0.01)  parentheses  replace plain 

***Fixed effects years***
xtset year
xtreg avgper c.lrecon##c.econshareall c.galtan##c.galtanshareall rilenew relative_salience if gov_status == 3, fe
estimates store year1

esttab year1 using year.tex , b(3) se(3) cons r2 scalars("ll Log likelihood") label star(* 0.1 ** 0.05 *** 0.01)  parentheses  replace plain 


***Niche party***
xtset countryid
xtreg avgper c.lrecon##c.econshareall##i.niche c.galtan##c.galtanshareall##i.niche rilenew relative_salience if gov_status == 3, fe
estimates store niche1

esttab niche1 using niche.tex , b(3) se(3) cons r2 scalars("ll Log likelihood") label star(* 0.1 ** 0.05 *** 0.01)  parentheses  replace plain 

margins niche, dydx(galtan) at(galtanshareall=(0(10)50)) atmeans
marginsplot, by(niche) 
//Small but statistically significant effect for mainstream parties; large coefficient for niche parties but not significant

***PM party***
xtset countryid
xtreg avgper c.lrecon##c.econshareall##i.p_prime c.galtan##c.galtanshareall##i.p_prime rilenew relative_salience if gov_status == 3, fe
estimates store pm1

esttab pm1 using pm1.tex , b(3) se(3) cons r2 scalars("ll Log likelihood") label star(* 0.1 ** 0.05 *** 0.01)  parentheses  replace plain 


margins p_prime, dydx(galtan) at(galtanshareall=(0(10)50)) atmeans
marginsplot, by(p_prime) 
//Basically no effect for junior parties but strong effect for PM parties

***Econ and Galtan salience CMP instead of relative salience***
xtset countryid
xtreg avgper c.lrecon##c.econshareall c.galtan##c.galtanshareall galtancmp econcmp rilenew  if gov_status == 3, fe
estimates store sepa

esttab sepa using sepa.tex , b(3) se(3) cons r2 scalars("ll Log likelihood") label star(* 0.1 ** 0.05 *** 0.01)  parentheses  replace plain 

***Time variable continuous***
xtset countryid
xtreg avgper c.lrecon##c.econshareall##c.survdursmall c.galtan##c.galtanshareall##c.survdursmall rilenew relative_salience if gov_status == 3, fe
estimates store dura

esttab dura using dura.tex , b(3) se(3) cons r2 scalars("ll Log likelihood") label star(* 0.1 ** 0.05 *** 0.01)  parentheses  replace plain 

log close
translate gopaper.smcl gopaper.pdf

***Random intercepts models***
mixed avgper c.lrecon##c.econshareall c.galtan##c.galtanshareall rilenew if gov_status == 3 || countryid: 

mixed avgper c.lrecon##c.econshareall c.galtan##c.galtanshareall rilenew if gov_status == 3 || year: 

mixed avgper c.lrecon##c.econshareall##survdurdich c.galtan##c.galtanshareall##survdurdich rilenew if gov_status == 3 || countryid:

mixed avgper c.lrecon##c.econshareall##survdurdich c.galtan##c.galtanshareall##survdurdich galtancmp econcmp rilenew if gov_status == 3 || countryid:
margins survdurdich, dydx(galtan) at(galtanshareall=(0(10)50)) atmeans
marginsplot, by(survdurdich)
