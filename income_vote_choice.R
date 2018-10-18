trump_lgbt <- cces16 %>% 
  mutate(gay = recode(sexuality, "1=0; 2:5=1; else = 0")) %>% 
  mutate(trans2 = recode(trans, "1=1; else =0")) %>% 
  mutate(income = recode(faminc, "14:16=14 ; 31:99 = 99")) %>% 
  mutate(trump = recode(CC16_410a, "1=1; 2:4=0; else = NA")) %>% 
  filter(gay ==1 | trans2 ==1) %>% 
  group_by(income) %>% 
  mean_ci(trump, wt = commonweight_vv_lgbt) %>% 
  mutate(candidate = "LGBT - Trump")

clinton_lgbt <- cces16 %>% 
  mutate(gay = recode(sexuality, "1=0; 2:5=1; else = 0")) %>% 
  mutate(trans2 = recode(trans, "1=1; else =0")) %>% 
  mutate(income = recode(faminc, "14:16=14 ; 31:99 = 99")) %>%  
  mutate(clinton = recode(CC16_410a, "1=0; 2=1; 3:4=0; else = NA")) %>% 
  filter(gay ==1 | trans2 ==1) %>% 
  group_by(income) %>% 
  mean_ci(clinton, wt = commonweight_vv_lgbt) %>% 
  mutate(candidate = "LGBT - Clinton")

trump_not_lgbt <- cces16 %>% 
  mutate(income = recode(faminc, "14:16=14 ; 31:99 = 99")) %>% 
  mutate(trump = recode(CC16_410a, "1=1; 2:4=0; else = NA")) %>% 
  group_by(income) %>% 
  mean_ci(trump, wt = commonweight_vv_post) %>% 
  mutate(candidate = "Entire Sample - Trump")

clinton_not_lgbt <- cces16 %>% 
  mutate(income = recode(faminc, "14:16=14 ; 31:99 = 99")) %>%  
  mutate(clinton = recode(CC16_410a, "1=0; 2=1; 3:4=0; else = NA")) %>% 
  group_by(income) %>% 
  mean_ci(clinton, wt = commonweight_vv_post) %>% 
  mutate(candidate = "Entire Sample - Clinton")

graph <- bind_df("lgbt")

graph %>% 
  filter(income != 99) %>% 
  ggplot(., aes(x= income, y = mean, group = candidate, color = candidate)) +
  geom_point() +
  geom_smooth() +
  theme_minimal() +
  scale_color_lancet() +
  theme(text=element_text(size=45, family="font")) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent) +
  annotate("text", x= 50, y = .77, label = "LGBT - Clinton", size = 12, family = "font") +
  annotate("text", x= 45, y = .56, label = "Entire Sample - Clinton", size = 12, family = "font") +
  annotate("text", x= 42, y = .37, label = "Entire Sample - Trump", size = 12, family = "font") +
  annotate("text", x= 50, y = .20, label = "LGBT - Trump", size = 12, family = "font") +
  labs(x = "Age", y = "Percent of the Vote", title = "Vote Choice Across the Age Spectrum", caption = "Data: CCES 2016") +
  ggsave("/gay_pol/images/vote_across_age.png") 



