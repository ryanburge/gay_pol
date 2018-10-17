library(fst)
library(building)
library(car)
library(patchwork)
library(ggsci)


cces16 <- read.fst("C://cces16.fst")

source("D://measuring_evangelicals/reltrad16.R")
source("D://measuring_evangelicals/making_baprot.R")

g1 <- cces16 %>% select(V101, evangelical, mainline, bprot, catholic, jewish, other, none) %>% 
  gather(reltrad, x1, evangelical:none) %>% 
  filter(x1 ==1) %>% select(V101, reltrad)

cces16 <- cces16 %>% left_join(g1)
 

