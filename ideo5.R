
aa1 <- cces16 %>% 
  mutate(gay = recode(sexuality, "1=0; 2:5=1; else = 0")) %>% 
  mutate(trans2 = recode(trans, "1=1; else =0")) %>% 
  mutate(lgbt = gay + trans2) %>% 
  mutate(lgbt = recode(lgbt, "1=1; else = 0")) %>% 
  filter(lgbt ==0) %>%  
  ct(ideo5, wt = commonweight_vv_lgbt) %>% 
  filter(ideo5 !=8) %>% 
  mutate(ideo5 = to_factor(ideo5)) %>% 
  mutate(type = c("Entire Sample")) %>% 
  select(-n)

aa2 <- cces16 %>% 
  mutate(gay = recode(sexuality, "1=0; 2:5=1; else = 0")) %>% 
  mutate(trans2 = recode(trans, "1=1; else =0")) %>% 
  mutate(lgbt = gay + trans2) %>% 
  mutate(lgbt = recode(lgbt, "1=1; else = 0")) %>% 
  filter(lgbt ==1) %>%  
  ct(ideo5, wt = commonweight_vv_lgbt) %>% 
  filter(ideo5 !=8) %>% 
  mutate(ideo5 = to_factor(ideo5)) %>% 
  mutate(type = c("LGBT Sample")) %>% 
  select(-n)

graph <- bind_df("aa")


my.cols <- c("#B2182B", "#EF8A62", "#777676", "#67A9CF", "#2166AC")



font_add_google("Playfair Display", "font")
showtext_auto()

gg1 <- graph %>% 
  filter(ideo5 != "Not sure") %>% 
  filter(type == "Entire Sample") %>% 
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
  labs(x= "", y = "Percent of Each Sample", title = "Self-Described Political Ideology", caption = "", subtitle = "Entire Sample")


gg2 <- graph %>% 
  filter(ideo5 != "Not sure") %>% 
  filter(type != "Entire Sample") %>% 
  ggplot(.,aes(x=ideo5, y=pct, fill = ideo5)) + 
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


ggsave("/gay_pol/images/ideo5.png", both) 

