context("qpGraph parser")

test_graph = '
root pan_troglodytes
label	pan_troglodytes	pan_troglodytes
label	AltaiNea		AltaiNea
label	DenisovaPinky		DenisovaPinky
label	Dinka	Dinka
label	Papuan	Papuan
label	Onge	Onge
label	Ami	Ami
label	Kostenki14	Kostenki14
edge	Pan_SuperArch	pan_troglodytes	SuperArch
edge	DenAncAnc_DenAnc	DenisovaAncAnc	DenisovaAnc
edge	DenAnc_Den	DenisovaAnc	DenisovaPinky
edge	SuperArch_Hominid		SuperArch	Hominid
edge	Hominid_Arch			Hominid		Arch
edge	Hominid_AfrAnc			Hominid		AfrAnc
edge	AfrAnc_Dinka			AfrAnc		Dinka
edge	Arch_NeaAnc			Arch		NeaAnc
edge	NeaAnc				NeaAnc		AltaiNea
edge	AdmNonAfr_NonAfr		AdmixedNonAfr	NonAfricans
edge  NonAfr_E	NonAfricans	K
edge  K2_Kostenki14 K2			Kostenki14
edge  NonAfr_EastEua		NonAfricans	EastEurasian
edge  EastEua_P			EastEurasian	P
edge  P2_Pap		P2	Papuan
edge  EastEua_X			EastEurasian	X
edge  X_OngeAnc			X			OngeAnc
edge  X_Ami			X			Ami
edge  OngeAnc_Onge		OngeAnc	Onge
edge  OngeAnc_Jarawa		OngeAnc	Jarawa
admix DenisovaAncAnc	SuperArch	Arch	50	50
admix AdmixedNonAfr	AfrAnc		NeaAnc	50	50
admix P2		P		DenisovaAnc	50	50
admix K2		K		NeaAnc	50	50
'

test_that("we can parse a graph", {
    graph <- parse_qpgraph(test_graph)

    expect_equal(graph$no_nodes, 25)
    is_leaf <- graph$get_leaf_status()
    expect_equal(sum(is_leaf), 8)
})
