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

lgbt <- vote16 %>% 
  ggplot(., aes(x = reorder(vote16, -pct), y = pct, fill = vote16)) +
  geom_col(color = "black") +
  theme_minimal() +
  theme(text=element_text(size=45, family="font")) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "none") +
  geom_text(aes(y = pct + .045, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 16, family = "font") +
  scale_fill_manual(values=c("firebrick1", "goldenrod1", "dodgerblue3", "forestgreen")) +
  labs(x = "Vote Choice", y = "Percent", title = "Vote Choice in 2016", subtitle = "LGBT Population") 



vote16 <- cces16 %>% 
  mutate(pid3 = recode(pid7, "1:3 = 'Democrats'; 4 = 'Independents'; 5:7 = 'Republicans'; else = NA")) %>% 
  group_by(pid3) %>% 
  filter(CC16_410a <= 4) %>% 
  ct(CC16_410a, wt = commonweight_vv_lgbt) %>% 
  mutate(vote16 = recode(CC16_410a, "1='Donald Trump';
                    2='Hillary Clinton';
                    3='Gary Johnson';
                    4='Jill Stein'"))

font_add_google("Playfair Display", "font")
showtext_auto()

dem <- vote16 %>% 
  filter(pid3 == "Democrats") %>% 
  ggplot(., aes(x = reorder(vote16, -pct), y = pct, fill = vote16)) +
  geom_col(color = "black") +
  theme_minimal() +
  theme(text=element_text(size=45, family="font")) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "none") +
  geom_text(aes(y = pct + .045, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 16, family = "font") +
  scale_fill_manual(values=c("firebrick1", "goldenrod1", "dodgerblue3", "forestgreen")) +
  theme(plot.title = element_text(size = 64, face = "bold")) +
  labs(x = "Vote Choice", y = "Percent", title = "", subtitle = "Democrats") 

font_add_google("Playfair Display", "font")
showtext_auto()

rep <- vote16 %>% 
  filter(pid3 == "Republicans") %>% 
  ggplot(., aes(x = reorder(vote16, -pct), y = pct, fill = vote16)) +
  geom_col(color = "black") +
  theme_minimal() +
  theme(text=element_text(size=45, family="font")) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "none") +
  geom_text(aes(y = pct + .045, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 16, family = "font") +
  scale_fill_manual(values=c("firebrick1", "goldenrod1", "dodgerblue3", "forestgreen")) +
  theme(plot.title = element_text(size = 64, face = "bold")) +
  labs(x = "Vote Choice", y = "Percent", title = "", subtitle = "Republicans", caption = "Data: CCES 2016") 


ind <- vote16 %>% 
  filter(pid3 == "Independents") %>% 
  ggplot(., aes(x = reorder(vote16, -pct), y = pct, fill = vote16)) +
  geom_col(color = "black") +
  theme_minimal() +
  theme(text=element_text(size=45, family="font")) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "none") +
  geom_text(aes(y = pct + .045, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 16, family = "font") +
  scale_fill_manual(values=c("firebrick1", "goldenrod1", "dodgerblue3", "forestgreen")) +
  theme(plot.title = element_text(size = 64, face = "bold")) +
  labs(x = "Vote Choice", y = "Percent", title = "", subtitle = "Independents", caption = "") 



graph <- lgbt + ind + dem + rep + plot_layout(ncol = 2)

ggsave("/gay_pol/images/vote16_patch_pid.png", width =12, height =10, graph) 


