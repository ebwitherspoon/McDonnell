import delimited "/Users/ebenwitherspoon/Dropbox/GitHub/McDonnell/McDonnell Coding/McDonnell_CLEAN.csv", encoding(ISO-8859-2) clear 

foreach var of varlist sim_cum-ambig_cum_pair altcount-relcount {
	destring `var', force replace
}

foreach var of varlist content - pairscycleprepost {
	encode `var', gen(`var'_cd)
}

* === Paper 1 === *

* Subset Data
keep if content == "Math"
keep if actor == "C:" | actor == "T:"
keep if cycle == "B" | cycle == "C"

* SimWords by Actor
bysort actor: tabstat simwords, stats(n sum mean sd sem median min max) by(pair)

anova simwords i.pair_cd##i.actor_cd
margins i.pair_cd##i.actor_cd
marginsplot
pwcompare i.pair_cd##i.actor_cd, effects

* Ambiguities by Actor
bysort actor: tabstat ambig_final if content == "Math" & (actor == "T:" | actor == "C:"), stats(n sum sd sem mean median min max) by(pair)

anova ambig_final i.pair_cd##i.actor_cd
margins i.pair_cd##i.actor_cd
marginsplot
pwcompare i.pair_cd##i.actor_cd, effects

* Alternatives by Actor
bysort actor: tabstat alt_final if content == "Math" & (actor == "T:" | actor == "C:"), stats(n sum mean sd sem median min max) by(pair)

anova alt_final i.pair_cd##i.actor_cd
margins i.pair_cd##i.actor_cd
marginsplot
pwcompare i.pair_cd##i.actor_cd, effects

* Weighting by Actor
bysort actor: tabstat eval_final if content == "Math" & (actor == "T:" | actor == "C:"), stats(n sum mean sd sem median min max) by(pair)

anova eval_final i.pair_cd##i.actor_cd
margins i.pair_cd##i.actor_cd
marginsplot
pwcompare i.pair_cd##i.actor_cd, effects
