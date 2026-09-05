library(igraph)
g <- make_graph(c(1,2,2,3,3,4,4,1),
                directed = F,
                n=7)

g1 <- make_graph(c("Amy", "Ram", "Ram", "Li", "Li", "Amy",
                   "Amy", "Li", "Kate", "Li"),
                 directed=T)

degree(g1, mode='all')
degree(g1, mode='in')
degree(g1, mode='out')

diameter(g1, directed=F, weights = NA)
edge_density(g1, loops = F)
ecount(g1)/(vcount(g1)*(vcount(g1)-1))
reciprocity(g1)
closeness(g1, mode='all', weights = NA)
betweenness(g1, directed=T, weights=NA)
edge_betweenness(g1, directed=T, weights=NA)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- read.csv('networkdata.csv', header=T)
y <- data.frame(data$first, data$second)

net <- graph_from_data_frame(y, directed=T)
V(net)$label <- V(net)$name
V(net)$degree <- degree(net)

png('degree_histogram.png', width=800, height=600)
hist(V(net)$degree)
dev.off()

png('network_plot.png', width=800, height=600)
plot(net)
dev.off()

png('network_plot_degree_sized.png', width=800, height=600)
plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree*0.4,
     edge.arrow.size = 0.1,
     layout=layout.fruchterman.reingold)
dev.off()

hs <- hits_scores(net)$hub
as <- hits_scores(net)$authority

png('hubs_and_authorities.png', width=1200, height=600)
par(mfrow=c(1,2))
set.seed(123)
plot(net,
     vertex.size=hs*30,
     main = 'Hubs',
     vertex.color = rainbow(52),
     edge.arrow.size=0.1,
     layout = layout.kamada.kawai)

plot(net,
     vertex.size=as*30,
     main = 'Authorities',
     vertex.color = rainbow(52),
     edge.arrow.size=0.1,
     layout = layout.kamada.kawai)
par(mfrow=c(1,1))
dev.off()

net <- graph_from_data_frame(y, directed = F)
cnet <- cluster_edge_betweenness(net)

png('community_detection.png', width=800, height=600)
plot(cnet, net)
dev.off()


cat("Degree (net):\n"); print(sort(V(net)$degree, decreasing=TRUE))
cat("\nCloseness (net):\n"); print(closeness(net, mode='all', weights=NA))
cat("\nBetweenness (net):\n"); print(betweenness(net, directed=TRUE, weights=NA))
cat("\nEdge density (net):\n"); print(edge_density(net, loops=FALSE))
cat("\nReciprocity (net):\n"); print(reciprocity(net))

cat("1. Degree histogram — Most nodes (35 of them) have very low degree (0–10), and only a handful have high degree (40–70). This is a classic heavy-tailed / power-law-like distribution, typical of real-world social networks: a few well-connected hubs, many peripheral nodes.")

cat("2. Basic network plot — Shows the raw structure: a dense, tangled core cluster (AA, CA, CC, CD, DD, BB, BF, etc.) with a long tail of loosely attached or single-link nodes (LB, GC, KC, LA, FD, etc.) hanging off the edges.")

cat("3. Degree-sized plot — Confirms the same thing visually: CA, CC, and CD are the largest nodes, meaning they have the most connections and act as central hubs holding the network together.")

cat("4. Hubs vs Authorities — CB and CC are the top hubs (they point to many other well-connected nodes), while CA is the dominant authority (it's the node most pointed to by other important nodes). This suggests CA is the most referenced/trusted entity, while CB/CC are the biggest connectors/distributors.")

cat("5. Community detection — The network splits into a few clear communities (colored clusters): a red cluster around CD/DD/AA, a green cluster around CA/CB/CC/EB, a yellow/olive cluster around AE/AD/GA, and smaller isolated groups (EF/FD, CF/DE). This indicates the network isn't one uniform blob — it has sub-groups likely representing tighter-knit social circles, with CA/CB/CC bridging or centrally connecting them.")

cat("Overall takeaway: The network shows a classic hub-and-spoke social structure — a small set of highly connected, influential nodes (CA, CB, CC, CD) dominate the network, most other nodes are peripheral, and the network naturally breaks into a few distinct communities connected through these hubs.")