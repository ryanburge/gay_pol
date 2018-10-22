##### By Race 

vote16_r <- cces16 %>% 
  # mutate(gay = recode(sexuality, "1=0; 2:5=1; else = 0")) %>% 
  # mutate(trans2 = recode(trans, "1=1; else =0")) %>% 
  # filter(gay ==1 | trans2 ==1) %>% 
  filter(CC16_410a <= 4) %>% 
  mutate(race = recode(race, "5:10 = 99")) %>% 
  mutate(pid3 = recode(pid7, "1:3 = 'Democrats'; 4 = 'Independents'; 5:7 = 'Republicans'; else = NA")) %>% 
  group_by(race, pid3) %>% 
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


dems <- vote16_r %>% 
  filter(pid3 == "Democrats")  %>% 
  filter(pct > .05) %>% 
  ggplot(., aes(x = reorder(vote16, -pct), y = pct, fill = vote16)) +
  geom_col(color = "black") +
  theme_minimal() +
  facet_wrap(. ~ race_f) +
  theme(text=element_text(size=45, family="font")) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "none") +
  geom_text(aes(y = pct + .085, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 16, family = "font") +
  scale_fill_manual(values=c("dodgerblue3", "firebrick1")) +
  theme(plot.title = element_text(size = 64, face = "bold")) +
  labs(x = "Vote Choice", y = "Percent", title = "", subtitle = "Democrats") 


rep <- vote16_r %>% 
  filter(pid3 == "Republicans")  %>% 
  filter(vote16 == "Trump" | vote16 == "Clinton") %>% 
  ggplot(., aes(x = reorder(vote16, -pct), y = pct, fill = vote16)) +
  geom_col(color = "black") +
  theme_minimal() +
  facet_wrap(. ~ race_f) +
  theme(text=element_text(size=45, family="font")) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "none") +
  geom_text(aes(y = pct + .085, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 16, family = "font") +
  scale_fill_manual(values=c("dodgerblue3", "firebrick1")) +
  theme(plot.title = element_text(size = 64, face = "bold")) +
  labs(x = "Vote Choice", y = "Percent", title = "", subtitle = "Republicans", caption = "Data: CCES 2016") 

ind <- vote16_r %>% 
  filter(pid3 == "Independents")  %>% 
  filter(vote16 == "Trump" | vote16 == "Clinton") %>% 
  ggplot(., aes(x = reorder(vote16, -pct), y = pct, fill = vote16)) +
  geom_col(color = "black") +
  theme_minimal() +
  facet_wrap(. ~ race_f) +
  theme(text=element_text(size=45, family="font")) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "none") +
  geom_text(aes(y = pct + .085, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 16, family = "font") +
  scale_fill_manual(values=c("dodgerblue3", "firebrick1")) +
  theme(plot.title = element_text(size = 64, face = "bold")) +
  labs(x = "Vote Choice", y = "Percent", title = "", subtitle = "Independents") 




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


font_add_google("Playfair Display", "font")
showtext_auto()

gay <- vote16_r %>% 
  filter(vote16 == "Trump" | vote16 == "Clinton") %>% 
  ggplot(., aes(x = reorder(vote16, -pct), y = pct, fill = vote16)) +
  geom_col(color = "black") +
  theme_minimal() +
  facet_wrap(. ~ race_f) +
  theme(text=element_text(size=45, family="font")) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "none") +
  geom_text(aes(y = pct + .085, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 16, family = "font") +
  scale_fill_manual(values=c("dodgerblue3", "firebrick1")) +
  labs(x = "Vote Choice", y = "Percent", title = "Vote in 2016 by Race", subtitle = "LGBT Population", caption = "") 


graph <- gay + ind + dems + rep + plot_layout(ncol = 2)

ggsave("/gay_pol/images/vote16_patch_pid_race.png", width = 12, height = 10,  graph) 