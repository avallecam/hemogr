use "data/hem_cc_trd.dta"
regress plaqueta i.num_visita##i.group edad sexo
xtset new_code num_visita
xtgee plaqueta i.num_visita##i.group edad sexo, cor(ar1)
