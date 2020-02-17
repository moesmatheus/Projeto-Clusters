library(readxl)
library(cluster)
library(fpc)
library(factoextra)

dados <- read_excel("dados.xlsx", sheet = "CEREALS_2")

# Selecionando Colunas
ce = dados[2:12]

# Transformando chr em factor para calcular distâncias com daisy
ce$type=as.factor(ce$type)
ce$mfr = as.factor(ce$mfr)

# Calcular correlacao
correl=cor(ce[3:11])
round(correl,digits=3)
ce_novo=ce[,-c(10)]

# Selecionando colunas drivers
ce_novo.drivers=ce_novo[,-c(10)]
colnames(ce_novo.drivers)

#cálculo da matriz de distâncias utilizando função daisy
#não precisamos padronizar. daisy usa a métrica de Gower que já faz isso(*)
dist.cerv=daisy(ce_novo.drivers)
dist.cerv


# Utilizar kmdoide porque tem variavis quali
#algorimo pamk da library fpc ajuda a definir número de clusters com asw
ce_cluster = ce_novo
set.seed(18) # recomenda-se que testemos com vários set seeds para modificar partição inicial
kk=pamk(dist.cerv, krange = 2:10, diss = T, critout = T )
kk$nc
kk$pamobject$medoids
ce_cluster$kpamc=kk$pamobject$clustering
ce_cluster$kpamc

#para a representação só podemos trabalhar com as drivers quantitativas
fviz_cluster(list(data = ce_cluster[,3:9], cluster = ce_cluster$kpamc), show.clust.cent = F) 

