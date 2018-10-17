g2 <- cces16 %>% 
  filter(CC16_415r <= 100) %>%
  mutate(ab1 = recode(CC16_332a, "2=1; 1=0; else = NA")) %>% 
  mutate(ab2 = recode(CC16_332c, "1=1; 2=0; else = NA")) %>% 
  mutate(ab3 = recode(CC16_332d, "1=1; 2=0; else = NA")) %>% 
  mutate(ab4 = recode(CC16_332e, "1=1; 2=0; else = NA")) %>% 
  mutate(ab5 = recode(CC16_332f, "1=1; 2=0; else = NA")) %>%
  mutate(gaym = recode(CC16_335, "2=1; 1=0; else = NA")) %>% 
  mutate(social = ab1 + ab2 + ab3 + ab4 + ab5 + gaym) %>% 
  mutate(econ = CC16_415r)

g2 %>% 
  mean_ci(social) # 2.39


g2 %>% 
  mean_ci(econ) # 57.5


g2 %>% 
  mutate(gay = recode(sexuality, "1=0; 2:5=1; else = 0")) %>% 
  mutate(trans2 = recode(trans, "1=1; else =0")) %>% 
  filter(gay ==1 | trans2 ==1) %>% 
  mean_ci(social) # 1.36


g2 %>% 
  mutate(gay = recode(sexuality, "1=0; 2:5=1; else = 0")) %>% 
  mutate(trans2 = recode(trans, "1=1; else =0")) %>% 
  filter(gay ==1 | trans2 ==1) %>% 
  mean_ci(econ) # 1.36


a1 <- g2 %>% 
  mutate(gay = recode(sexuality, "1=0; 2:5=1; else = 0")) %>% 
  mutate(trans2 = recode(trans, "1=1; else =0")) %>% 
  mutate(lgbt = gay + trans2) %>% 
  mutate(lgbt = recode(lgbt, "1=1; else = 0")) %>% 
  filter(lgbt ==0) %>% 
  ggplot(., aes(x= econ, y = social)) + 
  geom_jitter(alpha = .1, color = "azure4") + 
  theme_minimal() + 
  theme(text=element_text(size=55, family="font")) +
  theme(legend.position = "none") + 
  labs(x = "<-- Economic Liberal: Economic Conservative -->", y = "<-- Social Liberal: Social Conservative -->", title = "                                        Economic and Social Positions in 2016", caption = "Data: CCES 2016", subtitle = "Entire Population" ) +
  geom_vline(xintercept = 57.5, linetype = "dashed") +
  geom_hline(yintercept = 2.38, linetype = "dashed") + 
  annotate("text", x=20, y = 5, label = "16.7%", size = 16, family = "font") +
  annotate("text", x=20, y = 1, label = "36.0%", size = 16, family = "font") +
  annotate("text", x=80, y = 5, label = "29.2%", size = 16, family = "font") +
  annotate("text", x=80, y = 1, label = "17.0%", size = 16, family = "font") 
  
g2 %>% 
  mutate(gay = recode(sexuality, "1=0; 2:5=1; else = 0")) %>% 
  mutate(trans2 = recode(trans, "1=1; else =0")) %>% 
  mutate(lgbt = gay + trans2) %>% 
  mutate(lgbt = recode(lgbt, "1=1; else = 0")) %>% 
  filter(lgbt ==1) %>%
  mutate(econ_con = recode(econ, "0:57 = 'Econ Liberal'; 58:100 ='Econ Conservative'")) %>% 
  mutate(social_con = recode(social, "0:2.39= 'Social Liberal'; 2.4:6 = 'Social Conservative'")) %>% 
  tabyl(econ_con, social_con) %>% 
  adorn_crosstab(denom = "all")
  


a2 <- g2 %>% 
  mutate(gay = recode(sexuality, "1=0; 2:5=1; else = 0")) %>% 
  mutate(trans2 = recode(trans, "1=1; else =0")) %>% 
  mutate(lgbt = gay + trans2) %>% 
  mutate(lgbt = recode(lgbt, "1=1; else = 0")) %>% 
  filter(lgbt ==1) %>% 
  ggplot(., aes(x= econ, y = social)) + 
  geom_jitter(alpha = .1, color = "azure4") + 
  theme_minimal() + 
  theme(text=element_text(size=55, family="font")) +
  theme(legend.position = "none") + 
  labs(x = "<-- Economic Liberal: Economic Conservative -->", y = "<-- Social Liberal: Social Conservative -->", title = "", caption = "Data: CCES 2016", subtitle = "LBGT Population" ) +
  geom_vline(xintercept = 57.5, linetype = "dashed") +
  geom_hline(yintercept = 2.38, linetype = "dashed") + 
  annotate("text", x=20, y = 5, label = "9.1%", size = 16, family = "font") +
  annotate("text", x=20, y = 1, label = "57.0%", size = 16, family = "font") +
  annotate("text", x=80, y = 5, label = "14.7%", size = 16, family = "font") +
  annotate("text", x=80, y = 1, label = "18.7%", size = 16, family = "font") 



both <- a1 + a2

ggsave("/gay_pol/images/four_square_position.png", width = 12, both) 



