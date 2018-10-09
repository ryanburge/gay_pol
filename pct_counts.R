
library(building)
library(car)
library(fst)
library(labelled)

# cces16 <- read_dta("D://cces/data/cces16.dta")
cces16 <- read.fst("C://cces16.fst")


graph <- cces16 %>% 
  ct(sexuality, wt = commonweight_vv_lgbt) %>% 
  mutate(sex = recode(sexuality, "1 = 'Straight';
                                  2 = 'Lesbian/Gay Woman';
                                  3 = 'Gay Man';
                                  4 = 'Bisexual';
                                  5 = 'Other'; 
                                  6 = 'Prefer Not to Say';
                                  8 = 'Skipped';
                                  9 = 'Not Asked'"))

font_add_google("Playfair Display", "font")
showtext_auto()

graph %>% 
  filter(sex != "Skipped") %>% 
  mutate(pct = round(pct, 3)) %>% 
  ggplot(., aes(x= reorder(sex, pct), y = pct)) +
  geom_col(color = "black", fill = "azure4") +
  theme_minimal() +
  theme(text=element_text(size=45, family="font")) +
  scale_y_continuous(labels = percent) +
  geom_text(aes(y = pct + .045, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 16, family = "font") +
  coord_flip() +
  labs(x = "Self-Described Sexuality", y = "Percent of Sample", title = "With which group do you most closely identify?",  caption = "Data: CCES 2016", subtitle = "Results are Weighted") +
  ggsave("/gay_pol/images/orientation_count.png", width = 8)

graph2 <- cces16 %>% 
  ct(trans, wt = commonweight_vv_lgbt) %>% 
  mutate(trans2 = recode(trans, "1 = 'Yes';
                                 2 = 'No';
                                 3 = 'Prefer Not to Say';
                                 8 = 'Skipped';
                                 9 = 'Not Asked'"))


graph2 %>% 
  mutate(pct = round(pct, 3)) %>% 
  ggplot(., aes(x= reorder(trans2, pct), y = pct)) +
  geom_col(color = "black", fill = "azure4") +
  theme_minimal() +
  theme(text=element_text(size=45, family="font")) +
  scale_y_continuous(labels = percent) +
  geom_text(aes(y = pct + .045, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 16, family = "font") +
  coord_flip() +
  labs(x = "Self-Described Answer", y = "Percent of Sample", title = "Have You Gone Through a Gender Transition Process?",  caption = "Data: CCES 2016", subtitle = "Results are Weighted") +
  ggsave("/gay_pol/images/trans_count.png", width = 8)


