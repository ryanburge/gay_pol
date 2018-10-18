library(tidyverse)
library(extrafont)
library(haven)
library(car)
library(janitor)
library(labelled)
source("D://cces/ggthemes.R")

cces16 <- read_dta("D://cces/data/cces16.dta")

## Run RELTRAD evangelical

aa1 <- cces16 %>% 
  mutate(gay = recode(sexuality, "1=0; 2:5=1; else = 0")) %>% 
  mutate(trans2 = recode(trans, "1=1; else =0")) %>% 
  mutate(lgbt = gay + trans2) %>% 
  mutate(lgbt = recode(lgbt, "1=1; else = 0")) %>% 
  filter(lgbt ==0) %>%  
  ct(pid7, wt = commonweight_vv_lgbt) %>% 
  filter(pid7 !=8) %>% 
  mutate(pid7 = to_factor(pid7)) %>% 
  mutate(type = c("Entire Sample")) %>% 
  select(-n)

aa2 <- cces16 %>% 
  mutate(gay = recode(sexuality, "1=0; 2:5=1; else = 0")) %>% 
  mutate(trans2 = recode(trans, "1=1; else =0")) %>% 
  mutate(lgbt = gay + trans2) %>% 
  mutate(lgbt = recode(lgbt, "1=1; else = 0")) %>% 
  filter(lgbt ==1) %>%  
  ct(pid7, wt = commonweight_vv_lgbt) %>% 
  filter(pid7 !=8) %>% 
  mutate(pid7 = to_factor(pid7)) %>% 
  mutate(type = c("LGBT Sample")) %>% 
  select(-n)


pid7 <- bind_df("aa")


my.cols <- brewer.pal(7, "RdBu")

my.cols[4] <- "#777676"


font_add_google("Playfair Display", "font")
showtext_auto()

gg1 <- pid7 %>% 
  filter(pid7 != "Skipped") %>% 
  filter(pid7 != "Not Asked") %>% 
  filter(type == "Entire Sample") %>% 
  ggplot(.,aes(x=pid7, y=pct, fill = pid7)) + 
  geom_col(color = "black") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(reverse=FALSE)) +
  theme_minimal() +
  theme(legend.position = "none")+
  scale_fill_manual(values = rev(my.cols)) +
  theme(axis.text.x = element_text(size = 22)) +
  theme(text=element_text(size=45, family="font")) +
  geom_text(aes(y = pct + .02, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 16, family = "font") +
  labs(x= "", y = "Percent of Each Sample", title = "Self-Described Political Partisanship", caption = "", subtitle = "Entire Sample")


gg2 <- pid7 %>% 
  filter(pid7 != "Skipped") %>% 
  filter(pid7 != "Not Asked") %>% 
  filter(type != "Entire Sample") %>% 
  ggplot(.,aes(x=pid7, y=pct, fill = pid7)) + 
  geom_col(color = "black") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(reverse=FALSE)) +
  theme_minimal() +
  theme(legend.position = "none")+
  scale_fill_manual(values = rev(my.cols)) +
  theme(axis.text.x = element_text(size = 22)) +
  theme(text=element_text(size=45, family="font")) +
  geom_text(aes(y = pct + .035, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 16, family = "font") +
  labs(x= "Party Identification", y = "Percent of Each Sample", title = "", caption = "Data: CCES 2016", subtitle = "LGBT Sample")

both <- gg1 + gg2 + plot_layout(ncol =1)


ggsave("/gay_pol/images/pid7.png", width = 9, both) 

