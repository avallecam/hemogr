use "data/hem_cc_trd.dta"
*abaston. ~ diff_fecha*group + edad + sexo
*regress abaston_ i.num_visita##i.group edad sexo
xtset new_code num_visita

*xtgee abaston_ c.diff_fecha##i.group edad sexo, cor(ar1) fam(gau)
xtgee abaston_ c.diff_fecha##i.group edad sexo, cor(ar1) fam(nb)

*xtgee abaston_ i.num_visita##i.group edad sexo, cor(ar1) fam(gau)
xtgee abaston_ i.num_visita##i.group edad sexo, cor(ar1) fam(nb)
