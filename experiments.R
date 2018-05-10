library(magrittr)
library(adrift)

graph <- parse_qpgraph(readr::read_file("data-raw/Basic_OngeEA_wArch.graph"))
plt <- graph %>%
    make_graph_plot(
        ggplot2::aes(filter = !admixed),
        edge_colour = "darkblue"
    ) %>%
    show_leaf_labels(nudge_y = -0.5, angle = 15)
print(plt)

admixture_vars <- c(
    "P_P2" = "alpha", #"DenisovaAnc_P2" = "1-alpha",
    "K_K2" = "beta",  #"NeaAnc_K2" = "1-beta"
    "AfrAnc_AdmixedNonAfr" = "gamma"
)
plt %>% add_admixture_labels(
    admixture_vars,
    linetype = "dotted",
    colour = "darkred"
)

plt %>% add_admixture_labels(
    attr(graph, "admixture_proportions"),
    label_size = 3,
    linetype = "dashed",
    colour = "darkgray"
)
