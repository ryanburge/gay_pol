
cces16 <- read_dta("D://cces/data/cces16.dta")


aa1 <- cces16 %>% 
  mutate(gay = recode(sexuality, "1=0; 2:5=1; else = 0")) %>% 
  mutate(trans2 = recode(trans, "1=1; else =0")) %>% 
  mutate(lgbt = gay + trans2) %>% 
  mutate(lgbt = recode(lgbt, "1=1; else = 0")) %>% 
  mutate(pid3 = recode(pid7, "1:3 = 'Democrats'; 4 = 'Independents'; 5:7 = 'Republicans'; else = NA")) %>% 
  filter(pid3 == "Republicans") %>% 
  filter(lgbt ==0) %>%  
  ct(ideo5, wt = commonweight_vv_lgbt) %>% 
  filter(ideo5 !=8) %>% 
  mutate(ideo5 = to_factor(ideo5)) %>% 
  mutate(type = c("Republicans")) %>% 
  select(-n)



aa2 <- cces16 %>% 
  mutate(gay = recode(sexuality, "1=0; 2:5=1; else = 0")) %>% 
  mutate(trans2 = recode(trans, "1=1; else =0")) %>% 
  mutate(lgbt = gay + trans2) %>% 
  mutate(lgbt = recode(lgbt, "1=1; else = 0")) %>% 
  mutate(pid3 = recode(pid7, "1:3 = 'Democrats'; 4 = 'Independents'; 5:7 = 'Republicans'; else = NA")) %>% 
  filter(pid3 == "Democrats") %>% 
  filter(lgbt ==0) %>%  
  ct(ideo5, wt = commonweight_vv_lgbt) %>% 
  filter(ideo5 !=8) %>% 
  mutate(ideo5 = to_factor(ideo5)) %>% 
  mutate(type = c("Democrats")) %>% 
  select(-n)
  
aa3 <- cces16 %>% 
  mutate(gay = recode(sexuality, "1=0; 2:5=1; else = 0")) %>% 
  mutate(trans2 = recode(trans, "1=1; else =0")) %>% 
  mutate(lgbt = gay + trans2) %>% 
  mutate(lgbt = recode(lgbt, "1=1; else = 0")) %>% 
  mutate(pid3 = recode(pid7, "1:3 = 'Democrats'; 4 = 'Independents'; 5:7 = 'Republicans'; else = NA")) %>% 
  filter(pid3 == "Independents") %>% 
  filter(lgbt ==0) %>%  
  ct(ideo5, wt = commonweight_vv_lgbt) %>% 
  filter(ideo5 !=8) %>% 
  mutate(ideo5 = to_factor(ideo5)) %>% 
  mutate(type = c("Independents")) %>% 
  select(-n)

  
aa4 <- cces16 %>% 
  mutate(gay = recode(sexuality, "1=0; 2:5=1; else = 0")) %>% 
  mutate(trans2 = recode(trans, "1=1; else =0")) %>% 
  mutate(lgbt = gay + trans2) %>% 
  mutate(lgbt = recode(lgbt, "1=1; else = 0")) %>% 
  mutate(pid3 = recode(pid7, "1:3 = 'Democrats'; 4 = 'Independents'; 5:7 = 'Republicans'; else = NA")) %>% 
  filter(lgbt ==1) %>%  
  ct(ideo5, wt = commonweight_vv_lgbt) %>% 
  filter(ideo5 !=8) %>% 
  mutate(ideo5 = to_factor(ideo5)) %>% 
  mutate(type = c("LGBT Sample")) %>% 
  select(-n)

graph <- bind_df("aa")

graph <- graph %>% 
  mutate(ideo5 = recode(ideo5, "1 = 'Very Liberal';
                                2 = 'Liberal';
                                3 = 'Moderate';
                                4 = 'Conserv.';
                                5 = 'Very Conserv.'; 
                                6 = 'Not Sure'"))

graph$ideo5 <- factor(graph$ideo5, levels = c("Very Liberal", "Liberal", "Moderate", "Conserv.", "Very Conserv.", "Not Sure"))



my.cols <- c("darkorchid", "#B2182B", "#EF8A62", "#777676", "#67A9CF", "#2166AC")

font_add_google("Playfair Display", "font")
showtext_auto()

gay <- graph %>% 
  filter(type == "LGBT Sample") %>% 
  ggplot(.,aes(x=ideo5, y=pct, fill = ideo5)) + 
  geom_col(color = "black") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(reverse=FALSE)) +
  theme_minimal() +
  theme(legend.position = "none")+
  scale_fill_manual(values = rev(my.cols)) +
  theme(axis.text.x = element_text(size = 22)) +
  theme(text=element_text(size=45, family="font")) +
  geom_text(aes(y = pct + .03, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 16, family = "font") +
  labs(x= "", y = "Percent of Each Sample", title = "Self-Described Political Ideology", caption = "", subtitle = "LGBT Sample") 

ind <- graph %>% 
  filter(type == "Independents") %>% 
  ggplot(.,aes(x=ideo5, y=pct, fill = ideo5)) + 
  geom_col(color = "black") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(reverse=FALSE)) +
  theme_minimal() +
  theme(legend.position = "none")+
  scale_fill_manual(values = rev(my.cols)) +
  theme(axis.text.x = element_text(size = 22)) +
  theme(text=element_text(size=45, family="font")) +
  geom_text(aes(y = pct + .03, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 16, family = "font") +
  labs(x= "", y = "", title = "", caption = "", subtitle = "Independents")

dem <- graph %>% 
  filter(type == "Democrats") %>% 
  ggplot(.,aes(x=ideo5, y=pct, fill = ideo5)) + 
  geom_col(color = "black") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(reverse=FALSE)) +
  theme_minimal() +
  theme(legend.position = "none")+
  scale_fill_manual(values = rev(my.cols)) +
  theme(axis.text.x = element_text(size = 22)) +
  theme(text=element_text(size=45, family="font")) +
  geom_text(aes(y = pct + .03, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 16, family = "font") +
  labs(x= "", y = "Percent of Each Sample", title = "", caption = "", subtitle = "Democrats")

rep <- graph %>% 
  filter(type == "Republicans") %>% 
  ggplot(.,aes(x=ideo5, y=pct, fill = ideo5)) + 
  geom_col(color = "black") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(reverse=FALSE)) +
  theme_minimal() +
  theme(legend.position = "none")+
  scale_fill_manual(values = rev(my.cols)) +
  theme(axis.text.x = element_text(size = 22)) +
  theme(text=element_text(size=45, family="font")) +
  geom_text(aes(y = pct + .03, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 16, family = "font") +
  labs(x= "", y = "", title = "", caption = "Data: CCES 2016", subtitle = "Republicans")


graph <- gay + ind + dem + rep + plot_layout(ncol = 2)


ggsave("/gay_pol/images/ideo5.png", graph) 

