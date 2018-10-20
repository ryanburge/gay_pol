clinton_lgbt <- cces16 %>% 
  mutate(gay = recode(sexuality, "1=0; 2:5=1; else = 0")) %>% 
  mutate(trans2 = recode(trans, "1=1; else =0")) %>% 
  mutate(age = 2016 - birthyr) %>% 
  mutate(clinton = recode(CC16_410a, "1=0; 2=1; 3:4=0; else = NA")) %>% 
  filter(gay ==1 | trans2 ==1) %>% 
  group_by(age) %>% 
  mean_ci(clinton, wt = commonweight_vv_lgbt) %>% 
  mutate(candidate = "LGBT")

clinton_dem <- cces16 %>% 
  filter(pid7 <= 3) %>% 
  mutate(age = 2016 - birthyr) %>% 
  mutate(clinton = recode(CC16_410a, "1=0; 2=1; 3:4=0; else = NA")) %>%  
  group_by(age) %>% 
  mean_ci(clinton, wt = commonweight_vv_post) %>% 
  mutate(candidate = "Democrats")

clinton_rep <- cces16 %>% 
  filter(pid7 >= 4) %>% 
  mutate(age = 2016 - birthyr) %>% 
  mutate(clinton = recode(CC16_410a, "1=0; 2=1; 3:4=0; else = NA")) %>% 
  group_by(age) %>% 
  mean_ci(clinton, wt = commonweight_vv_post) %>% 
  mutate(candidate = "Republicans")

clinton_ind <- cces16 %>% 
  filter(pid7 == 4) %>% 
  mutate(age = 2016 - birthyr) %>% 
  mutate(clinton = recode(CC16_410a, "1=0; 2=1; 3:4=0; else = NA")) %>% 
  group_by(age) %>% 
  mean_ci(clinton, wt = commonweight_vv_post) %>% 
  mutate(candidate = "Independents")


graph <- bind_df("clinton")

graph %>% 
  filter(age <= 80) %>% 
  ggplot(., aes(x= age, y = mean, group = candidate, color = candidate)) +
  geom_point() +
  geom_smooth() +
  theme_minimal() +
  scale_color_manual(values=c("dodgerblue3", "azure4", "forestgreen", "firebrick1")) +
  theme(text=element_text(size=45, family="font")) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent) +
  annotate("text", x= 50, y = .94, label = "Democrats", size = 12, family = "font") +
  annotate("text", x= 50, y = .77, label = "LGBT", size = 12, family = "font") +
  annotate("text", x= 50, y = .35, label = "Independents", size = 12, family = "font") +
  annotate("text", x= 50, y = .16, label = "Republicans", size = 12, family = "font") +
  labs(x = "Age", y = "Percent of the Vote for Clinton", title = "Vote Choice Across the Age Spectrum", caption = "Data: CCES 2016") +
  ggsave("/gay_pol/images/vote_across_age.png") 



