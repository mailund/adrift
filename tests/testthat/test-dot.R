context("Dot parser")

test_graph <- '
digraph G {
size = "7.5,10" ;
labelloc = "t" ;
label = "Basic_OngeEA_wArch.graph ::      Alt        Den        Din        Ong      -0.003599    -0.005614    -0.002015     0.000769    -2.622
" ;

pan_troglodytes  [ label = "pan_troglodytes" ] ;
Dinka  [ label = "Dinka" ] ;
AltaiNea  [ label = "AltaiNea" ] ;
DenisovaPinky  [ label = "DenisovaPinky" ] ;
Kostenki14  [ label = "Kostenki14" ] ;
Ami  [ label = "Ami" ] ;
Onge  [ label = "Onge" ] ;
Papuan  [ label = "Papuan" ] ;
pan_troglodytes -> SuperArch [  label = "227" ] ;
SuperArch -> Hominid [  label = "109" ] ;
Hominid -> Arch [  label = "175" ] ;
Hominid -> AfrAnc [  label = "273" ] ;
Arch -> NeaAnc [  label = "48" ] ;
AfrAnc -> Dinka [  label = "5" ] ;
NeaAnc -> AltaiNea [  label = "44" ] ;
DenisovaAncAnc -> DenisovaAnc [  label = "180" ] ;
DenisovaAnc -> DenisovaPinky [  label = "44" ] ;
AdmixedNonAfr -> NonAfricans [  label = "104" ] ;
NonAfricans -> K [  label = "218" ] ;
NonAfricans -> EastEurasian [  label = "24" ] ;
K2 -> Kostenki14 [  label = "223" ] ;
EastEurasian -> P [  label = "54" ] ;
EastEurasian -> X [  label = "2" ] ;
X -> OngeAnc [  label = "44" ] ;
X -> Ami [  label = "60" ] ;
OngeAnc -> Onge [  label = "44" ] ;
OngeAnc -> Jarawa [  label = "0" ] ;
P2 -> Papuan [  label = "57" ] ;
SuperArch -> DenisovaAncAnc [ style=dotted,  label = "51%" ] ;
Arch -> DenisovaAncAnc [ style=dotted,  label = "49%" ] ;
AfrAnc -> AdmixedNonAfr [ style=dotted,  label = "97%" ] ;
NeaAnc -> AdmixedNonAfr [ style=dotted,  label = "3%" ] ;
K -> K2 [ style=dotted,  label = "99%" ] ;
NeaAnc -> K2 [ style=dotted,  label = "1%" ] ;
P -> P2 [ style=dotted,  label = "97%" ] ;
DenisovaAnc -> P2 [ style=dotted,  label = "3%" ] ;
}
'

test_graph_2 <- '
digraph G {
R -> DAM ;
R -> A ;
A -> HAN ;
A -> B ;
B -> C ;
C -> CH ;
C -> D ;
D -> BK ;
D -> MA ;
B -> E ;
E -> RO ;
E -> F ;
F -> BU ;
F -> G ;
G -> MT ;
G -> H ;
H -> HE ;
H -> I ;
I -> J ;
J -> K ;
K -> BS ;
K -> FL ;
J -> L ;
L -> JER ;
L -> O ;
O -> P ;
P -> Q ;
Q -> GA ;
Q -> N2 ;
N2 -> EL ;
N2 -> WP ;
P -> KC ;
O -> S ;
S -> HOL ;
S -> T ;
T -> U ;
U -> DF ;
U -> DB ;
T -> V ;
V -> GW ;
V -> MR ;
I -> X ;
X -> Y ;
Y -> BN ;
Y -> BC ;
X -> Z ;
Z -> PA ;
Z -> A2 ;
A2 -> CA ;
A2 -> B2 ;
B2 -> C2 ;
B2 -> SA ;
C2 -> D2 ;
D2 -> BRA ;
D2 -> E2 [ style=dotted,  label = "85%" ] ;
C2 -> F2 ;
F2 -> G2 ;
G2 -> H2 ;
H2 -> E2 [ style=dotted,  label = "15%" ] ;
E2 -> LI ;
G2 -> MIR ;
H2 -> LM ;
F2 -> I2 ;
I2 -> J2 ;
J2 -> MER ;
J2 -> L2 ;
L2 -> PRE ;
L2 -> ALT ;
I2 -> M ;
M -> ARO ;
M -> N ;
N -> MRO ;
N -> M2 ;
M2 -> CC ;
M2 -> BAR ;
}
'

test_graph_3 <- '
digraph Foo {
A -> D;
B -> D;
C -> E;
D -> F;
E -> F;
}
'

test_that("we can parse a .dot file", {
    if (!requireNamespace("minilexer")) {
        skip("Parsers require the minilexer package, which is not installed")
        return()
    }
    res <- tryCatch({
        loadNamespace(
            "matchbox",
            versionCheck = list(op = ">=", version = "0.0.0.9004")
        )
        TRUE
    },
    error = function(e) { skip(e) ; FALSE }
    )
    if (!res) {
        return()
    }


    graph <- parse_dot(test_graph)
    expect_equal(graph$no_nodes, 25)
    admix_props <- attr(graph, "admixture_proportions")
    expect_equal(length(admix_props), 8)

    graph <- parse_dot(test_graph_2)
    expect_equal(graph$no_nodes, 75)
    admix_props <- attr(graph, "admixture_proportions")
    expect_equal(length(admix_props), 2)

    graph <- parse_dot(test_graph_3)
    expect_equal(graph$no_nodes, 6)
    admix_props <- attr(graph, "admixture_proportions")
    expect_equal(length(admix_props), 0)
})

test_that("we can handle parse errors", {
    if (!requireNamespace("minilexer")) {
        skip("Parsers require the minilexer package, which is not installed")
        return()
    }
    res <- tryCatch({
        loadNamespace(
            "matchbox",
            versionCheck = list(op = ">=", version = "0.0.0.9004")
        )
        TRUE
    },
        error = function(e) { skip(e) ; FALSE }
    )
    if (!res) {
        return()
    }

    expect_error(parse_dot("foo bar baz"))
    expect_error(parse_dot("digraph Foo { blah blah }"))
})
