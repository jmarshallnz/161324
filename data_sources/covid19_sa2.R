library(tidyverse)
library(rmapshaper)

vacc <- read_csv("https://www.massey.ac.nz/~jcmarsha/data/covid19/dose1_sa2.csv") %>%
  rename(sa2 = SA2)

# independent test set will be sampled from some spatial strata I guess?
library(sf)
map <- read_sf("~/teaching/161122/data/covid19_vacc_sa2/sa2_map/statistical-area-2-2018-clipped-generalised.shp")
map <- map %>% filter(SA22018__1 != "Chatham Islands") %>%
  filter(LAND_AREA_ > 0) %>%
  mutate(sa2 = as.numeric(SA22018_V1)) %>%
  select(sa2)

map_simple <- ms_simplify(map, keep = 0.03,
                          keep_shapes = FALSE)
st_write(map_simple, "../data/covid19/sa2_boundary.sqlite", delete_layer=TRUE)

# cluster our map centroids into say 400 odd chunks of ~5 SA2s?
centroids <- map %>% 
  bind_cols(st_coordinates(st_centroid(.$geometry))) %>%
  select(sa2, X, Y) %>% st_set_geometry(NULL)

if (0) { # using kmeans
  km <- kmeans(centroids %>% select(X, Y), centers=500, nstart = 100, iter.max = 100)
  cent_map <- centroids %>% bind_cols(cluster = km$cluster) %>%
    left_join(map) %>% st_as_sf()
  png("temp_map.png", width=700, height=1200)
  ggplot(cent_map) +
    geom_sf(mapping=aes(fill=as_factor(cluster))) +
    guides(fill='none')
  dev.off()
}

library(spdep)
library(igraph)
nz_nb <- poly2nb(map, queen=TRUE)
nb_B <- nb2listw(nz_nb, style="B", zero.policy=TRUE)
B <- as(nb_B, "symmetricMatrix")
B <- as.matrix(B)

g <- graph.adjacency(B, mode="undirected")

#g_clust <- cluster_louvain(g)
# ok, now produce the SNN thing. For this we need spatial weights corresponding
# to the 
#foo <- sNNclust(as.dist(d), k=5, eps=2, minPts=2, borderPoints = TRUE)

lec <- cluster_leading_eigen(g)
h_clust <- as.hclust(lec)
#plot(h_clust)
foo <- cutree(h_clust, k=200)
table(foo)

# OK, this looks good. Let's go and drop a random sample of these out
set.seed(17)# 7, 14
test.clusters <- sample(unique(foo), 40)
test_map <- centroids %>% 
  bind_cols(cluster=foo) %>%
  mutate(set = if_else(cluster %in% test.clusters, "test", "train")) %>%
  left_join(map) %>% st_as_sf()

all <- test_map %>% left_join(vacc) %>%
  st_set_geometry(NULL)


all_imp <- all %>% VIM::kNN(k=5)

all_imp %>%  count(DHB, set) %>% group_by(DHB) %>% mutate(n = n/sum(n)) %>% pivot_wider(names_from=set, values_from=n)

if (0) {
png("temp_map.png", width=700, height=1200)
ggplot(test_map) +
  geom_sf(mapping=aes(fill=set)) +
  guides(fill='none')
dev.off()
}

# TODO: Need to do imputation on this.

# OK, dump out our datasets
all_imp %>% mutate(VaccRate = if_else(set == "train", VaccRate, NA_real_)) %>%
  select(one_of(names(vacc))) %>%
  write_csv("../data/covid19/covid-vacc.csv")
all_imp %>% filter(set == "test") %>% select(sa2, VaccRate) %>%
  write_csv("../data/covid19/covid-vacc-test-y.csv")
