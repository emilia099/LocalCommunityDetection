network <- read.csv('friendship_matrix.csv', row.names = 1)
atr <- read.csv((file('users_final.csv', encoding = "UTF-16LE")), header=T, sep=",")


library(network)
library(sna)

#creating dataframe for regressions
gender <- ifelse(atr$sex=='??????????????', 1, 0)
work <- ifelse(atr$occupationtype=='work', 1, 0)
school <- ifelse(atr$occupationtype=='school', 1, 0)

id <- atr$Id
age <- atr$age
activity <- atr$activity_bin
indegree <- degree(net_friendship, gmode = 'digraph', 
                   diag = FALSE, cmode = 'indegree', 
                   rescale = FALSE, ignore.eval = FALSE)
between <- betweenness(net_friendship, gmode = 'digraph',
                       diag = FALSE, cmode = 'directed'
close <- closeness(net_friendship, gmode = 'digraph',
                   diag = FALSE, cmode = 'directed', rescale = FALSE)
id_1 <- as.numeric(rownames(network))
df_net <- cbind(id_1, indegree, between, close)
colnames(df_net) <- c('id','indegree','between', 'close')
df_vk <- cbind(id, gender, age, work, school, activity)


#creating network
net_matrix <- as.matrix(network)
net_friendship <- as.network(net_matrix, directed=FALSE)



#scale-free network test
library(data.table)
library(magrittr)
library(dplyr)
library(ggplot2)



sex <- atr$sex
occupation <- atr$occupation.type


data <- data.frame(cbind(row.names(net_matrix), indeg))
colnames(data) <- c('id', 'indegree')

over_0 <- data$indegree > 0
fr.table <- table(data[over_0,]$indegree)%>% as.data.table()
colnames(fr.table) <- c("indegree", "freq")
fr.table

p1<-ggplot(data =fr.table, aes(x = as.numeric(freq), y = as.numeric(indegree)))+
  scale_x_continuous(trans='log10', labels = scales::comma)+
  scale_y_continuous(trans='log10', labels = scales::comma)+
  ylab("Frequency")+
  xlab("Indegree")+
  theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text=element_text(size=12, colour="black"))+
  theme(axis.title=element_text(size=12, face="bold")) +
  geom_point(color='#0F2B43', size=2.5, alpha = 0.5)

print(p1)



#cleaning and ploting network
detach("package:sna", unload=TRUE)
detach("package:network", unload=TRUE)


library(igraph)
library(intergraph)

local_graph<-graph_from_adjacency_matrix(net_matrix)


#plotting initial graph

par(mar=c(0,0,1,0))
plot(local_graph, vertex.size=2, vertex.label=NA)

#remove directions of edges
local_undirected <- as.undirected(local_graph, mode='collapse')

local_clean <- delete_vertices(local_undirected,
                               V(local_undirected)[degree(local_undirected) ==0])

#setting coordinates
coords=layout_with_fr(local_clean)

par(mar=c(0,0,1,0))
plot(local_clean, vertex.label=NA,
     vertex.size=2)



#community detection
library(tidyverse) 
library(readr)

# run louvain with edge weights 
louvain_partition <- igraph::cluster_louvain(local_clean) 
# assign communities to graph 
local_clean$community <- louvain_partition$membership 
# see how many communities there are 
unique(local_clean$community)


communities <- data.frame() 
for (i in unique(local_clean$community)) { 
  # create subgraphs for each community
  subgraph <- induced_subgraph(local_clean, v = which(local_clean$community == i)) 
  # get size of each subgraph 
  size <- igraph::gorder(subgraph) 
  # get betweenness centrality 
  btwn <- igraph::betweenness(subgraph) 
  communities <- communities %>% 
    dplyr::bind_rows(data.frame(
      community = i, 
      n_characters = size, 
      most_important = names(which(btwn == max(btwn))) 
    ) 
    ) 
} 
knitr::kable(
  communities %>% 
    dplyr::select(community, n_characters, most_important)
)


#get larger communities

top_five <- data.frame() 
for (i in unique(local_clean$community)) { 
  # create subgraphs for each community 
  subgraph <- induced_subgraph(local_clean, v = which(local_clean$community == i)) 
  # for larger communities 
  if (igraph::gorder(subgraph) > 100) { 
    # get degree 
    degree <- igraph::degree(subgraph)
    
    size <- igraph::gorder(subgraph) 
    # get top five degrees 
    top <- names(head(sort(degree, decreasing = TRUE), 5)) 
    
    result <- data.frame(community = i, n_community = sort(size, decreasing = T)) 
  } else { 
    result <- data.frame(community = NULL, n_community = NULL) 
  } 
  top_five <- top_five %>% 
    dplyr::bind_rows(result) 
} 
knitr::kable(
  top_five %>% 
    tidyr::pivot_wider(names_from = community, values_from = n_community) 
)


#ploting communities

# give our nodes some properties, incl scaling them by degree and coloring them by community 
V(local_clean)$size <- 2
V(local_clean)$frame.color <- "white"
V(local_clean)$color <- local_clean$community
V(local_clean)$label <- V(local_clean)$name
V(local_clean)$label.cex <- 1.5 
# also color edges according to their starting node 
edge.start <- ends(local_clean, es = E(local_clean), names = F)[,1] 
E(local_clean)$color <- V(local_clean)$color[edge.start]
E(local_clean)$arrow.mode <- 0
# only label central characters 
v_labels <- V(local_clean)
for (i in 1:length(V(local_clean))) { 
  if (!(i %in% v_labels)) { V(local_clean)$label[i] <- "" } 
}

l1 <-  layout_with_fr(local_clean, dim = 3) 
plot(local_clean, vertex.label=NA,
     vertex.size=2, layout = l1)


library(kableExtra)
library(knitr)
library(magrittr)
library(dplyr)


users <- read.table('analysis.csv', sep = ',', header = TRUE)[, c(-1, -2, -10)]
users['log_between'] <- log(users$between + 1)
users <- users[c(-7)]

# NaNs
users <- users[is.na(users$gender) == FALSE,]
users[is.na(users$activity),] <- 0


# Age Nans
users[is.na(users$age), 'age'] <- median(users[!is.na(users$age), 'age'])









set.seed(3)

r1 <- runif(2243)
r2 <- runif(213)

# Activity 0
users.train <- users[users$activity == 0,][r1 > .3,]
users.test <- users[users$activity == 0,][r1 < .3,]

# Activity 1
users.train <- rbind(users.train, users[users$activity == 1,][r2 > .3,])
users.test <- rbind(users.test, users[users$activity == 1,][r2 < .3,])


library(xtable)
glm.fit <- glm(activity ~ age + gender + school + work, users.train, family = 'binomial')
summary(glm.fit) %>%
  xtable() %>%
  kbl() %>%
  kable_classic(full_width = T, html_font = "Times New Roman")

train <- round(summary(factor(users.train$activity)) / dim(users.train)[1], 2)
test <- round(summary(factor(users.test$activity)) / dim(users.test)[1], 2)
all <- cbind(train, test)
kbl(all, col.names = c('Train sample', 'Test sample')) %>%
  kable_classic(full_width = F, html_font = "Times New Roman", font_size = 18)

chi_test <- anova(glm.fit, test='Chisq')
chi_test
chi_test %>%
  kbl() %>%
  kable_classic(full_width = T, html_font = "Times New Roman", font_size = 18)


library(pscl)
pR2(glm.fit) %>%
  kbl() %>%
  kable_classic(full_width = T, html_font = "Times New Roman", font_size = 18)


library(ROCR)

proba <- predict(glm.fit, users.test)
prediction <- prediction(proba, users.test$activity)
prf <- performance(prediction, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(prediction, measure = "auc")
auc <- auc@y.values[[1]]
auc











library(ggplot2)

ggplot(users, aes(x=log_between[users$log_between != 0])) + geom_histogram(color= 'darkorange4', fill="darkgoldenrod2") + theme_classic()








library(xtable)
glm.fit <- glm(activity ~., users.train, family = 'binomial')
summary(glm.fit) %>%
  xtable() %>%
  kbl() %>%
  kable_classic(full_width = T, html_font = "Times New Roman")

train <- round(summary(factor(users.train$activity)) / dim(users.train)[1], 2)
test <- round(summary(factor(users.test$activity)) / dim(users.test)[1], 2)
all <- cbind(train, test)
kbl(all, col.names = c('Train sample', 'Test sample')) %>%
  kable_classic(full_width = F, html_font = "Times New Roman", font_size = 18)

chi_test <- anova(glm.fit, test='Chisq')
chi_test
chi_test %>%
  kbl() %>%
  kable_classic(full_width = T, html_font = "Times New Roman", font_size = 18)


library(pscl)
pR2(glm.fit) %>%
  kbl() %>%
  kable_classic(full_width = T, html_font = "Times New Roman", font_size = 18)


library(ROCR)

proba <- predict(glm.fit, users.test)
prediction <- prediction(proba, users.test$activity)
prf <- performance(prediction, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(prediction, measure = "auc")
auc <- auc@y.values[[1]]
auc