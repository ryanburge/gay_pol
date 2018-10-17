vote16 <- cces16 %>% 
  mutate(gay = recode(sexuality, "1=0; 2:5=1; else = 0")) %>% 
  mutate(trans2 = recode(trans, "1=1; else =0")) %>% 
  filter(gay ==1 | trans2 ==1) %>% 
  filter(CC16_410a <= 4) %>% 
  ct(CC16_410a, wt = commonweight_vv_lgbt) %>% 
  mutate(vote16 = recode(CC16_410a, "1='Donald Trump';
                    2='Hillary Clinton';
                    3='Gary Johnson';
                    4='Jill Stein'"))

font_add_google("Playfair Display", "font")
showtext_auto()

a1 <- vote16 %>% 
  ggplot(., aes(x = reorder(vote16, -pct), y = pct, fill = vote16)) +
  geom_col(color = "black") +
  theme_minimal() +
  theme(text=element_text(size=45, family="font")) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "none") +
  geom_text(aes(y = pct + .025, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 16, family = "font") +
  scale_fill_manual(values=c("firebrick1", "goldenrod1", "dodgerblue3", "forestgreen")) +
  labs(x = "Vote Choice", y = "Percent", title = "", subtitle = "LGBT Population", caption = "Data: CCES 2016") 

  ggsave("/gay_pol/images/vote16.png") 


vote16 <- cces16 %>% 
  # mutate(gay = recode(sexuality, "1=0; 2:5=1; else = 0")) %>% 
  # mutate(trans2 = recode(trans, "1=1; else =0")) %>% 
  # filter(gay ==1 | trans2 ==1) %>% 
  filter(CC16_410a <= 4) %>% 
  ct(CC16_410a, wt = commonweight_vv_lgbt) %>% 
  mutate(vote16 = recode(CC16_410a, "1='Donald Trump';
                    2='Hillary Clinton';
                    3='Gary Johnson';
                    4='Jill Stein'"))

font_add_google("Playfair Display", "font")
showtext_auto()

a2 <- vote16 %>% 
  ggplot(., aes(x = reorder(vote16, -pct), y = pct, fill = vote16)) +
  geom_col(color = "black") +
  theme_minimal() +
  theme(text=element_text(size=45, family="font")) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "none") +
  geom_text(aes(y = pct + .025, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 16, family = "font") +
  scale_fill_manual(values=c("firebrick1", "goldenrod1", "dodgerblue3", "forestgreen")) +
  theme(plot.title = element_text(size = 64, face = "bold")) +
  labs(x = "Vote Choice", y = "Percent", title = "                                                        Vote Choice in 2016", subtitle = "Entire Population") 

both <- a2 + a1

ggsave("/gay_pol/images/vote16_patch.png", width = 12, both) 


##### By Race #####

vote16_r <- cces16 %>% 
  # mutate(gay = recode(sexuality, "1=0; 2:5=1; else = 0")) %>% 
  # mutate(trans2 = recode(trans, "1=1; else =0")) %>% 
  # filter(gay ==1 | trans2 ==1) %>% 
  filter(CC16_410a <= 4) %>% 
  mutate(race = recode(race, "5:10 = 99")) %>% 
  group_by(race) %>% 
  ct(CC16_410a, wt = commonweight_vv_lgbt) %>% 
  mutate(vote16 = recode(CC16_410a, "1='Trump';
                    2='Clinton';
                    3='Johnson';
                    4='Stein'")) %>% 
  ungroup(race) %>% 
  mutate(race = recode(race, "1 = 'White';
                              2 = 'Black';
                              3 = 'Hispanic';
                              4 = 'Asian';
                              99 = 'All Others'"))

vote16_r$race_f = factor(vote16_r$race, levels=c('White','Black','Hispanic','Asian', 'All Others'))


b1 <- vote16_r %>% 
  ggplot(., aes(x = reorder(vote16, -pct), y = pct, fill = vote16)) +
  geom_col(color = "black") +
  theme_minimal() +
  facet_wrap(. ~ race_f) +
  theme(text=element_text(size=45, family="font")) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "none") +
  geom_text(aes(y = pct + .0545, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 16, family = "font") +
  scale_fill_manual(values=c("dodgerblue3", "goldenrod1", "forestgreen", "firebrick1")) +
  theme(plot.title = element_text(size = 64, face = "bold")) +
  labs(x = "Vote Choice", y = "Percent", title = "                                                                  Vote Choice in 2016 by Race", subtitle = "Entire Population") 

  # ggsave("/gay_pol/images/vote16_race.png") 


vote16_r <- cces16 %>% 
  mutate(gay = recode(sexuality, "1=0; 2:5=1; else = 0")) %>% 
  mutate(trans2 = recode(trans, "1=1; else =0")) %>% 
  filter(gay ==1 | trans2 ==1) %>% 
  filter(CC16_410a <= 4) %>% 
  mutate(race = recode(race, "5:10 = 99")) %>% 
  group_by(race) %>% 
  ct(CC16_410a, wt = commonweight_vv_lgbt) %>% 
  mutate(vote16 = recode(CC16_410a, "1='Trump';
                    2='Clinton';
                    3='Johnson';
                    4='Stein'")) %>% 
  ungroup(race) %>% 
  mutate(race = recode(race, "1 = 'White';
                              2 = 'Black';
                              3 = 'Hispanic';
                              4 = 'Asian';
                              99 = 'All Others'"))

vote16_r$race_f = factor(vote16_r$race, levels=c('White','Black','Hispanic','Asian', 'All Others'))


b2 <- vote16_r %>% 
  ggplot(., aes(x = reorder(vote16, -pct), y = pct, fill = vote16)) +
  geom_col(color = "black") +
  theme_minimal() +
  facet_wrap(. ~ race_f) +
  theme(text=element_text(size=45, family="font")) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "none") +
  geom_text(aes(y = pct + .0545, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 16, family = "font") +
  scale_fill_manual(values=c("dodgerblue3", "goldenrod1", "forestgreen", "firebrick1")) +
  labs(x = "Vote Choice", y = "Percent", title = "", subtitle = "LGBT Population", caption = "Data: CCES 2016") 

both2 <- b1 + b2

ggsave("/gay_pol/images/vote16_race.png", width = 16, both2) 
