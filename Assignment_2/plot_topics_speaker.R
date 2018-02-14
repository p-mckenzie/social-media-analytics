library(tidyverse)

df_topics <- read_csv("topics_speech.csv")

df_test <- df_topics %>% select(year,speaker,contains("Topic_"))
test <- df_test %>% group_by(year) %>% arrange(speaker,.by_group=TRUE) 

df_sum <- df_test %>% select(-year) %>% group_by(speaker) %>%  summarise_all(funs(sum))

df_heatmap <- reshape2::melt(df_sum, id.vars = "speaker")
df_heatmap$speaker <- factor(df_heatmap$speaker,levels=rev(unique(test$speaker)))



ggplot(df_heatmap,aes(speaker,variable)) + 
  geom_tile(aes(fill=value)) + 
  scale_fill_distiller(palette = "Spectral") + 
  labs(x="President",y="Topics",
       titles="Topics identified for each President's SOTU",
       subtitle="Ordered by Presidency") + 
  coord_flip() + 
  theme_bw(base_size = 16) + 
  scale_y_discrete(labels=as.character(seq(0,10)))


df_year_sum <- df_test %>% select(-speaker)  %>% group_by(year) %>%  summarise_all(funs(sum))
df_year_heatmap <- reshape2::melt(df_year_sum, id.vars = "year")


ggplot(df_year_heatmap) + 
  geom_line(aes(x=year,y=value)) + 
  facet_wrap(~variable) +
  labs(x="Year",y="Topics",
       titles="Change in topics in time for Presidents SOTU") + 
  theme_bw(base_size = 16) 


### Word distribution per topic

topic_word <- read_csv("topic_word_weights.csv") %>% 
  mutate(Topic = as.factor(Topic)) %>% group_by(Topic) %>% arrange(weighted_numbers) %>%
  mutate(order = row_number()) %>% ungroup()


ggplot(topic_word,aes(x=reorder(word,weighted_numbers),y=weighted_numbers,fill=Topic)) + 
  geom_bar(stat='identity',show.legend = FALSE) +
  facet_wrap(~Topic,scales = "free_y") +
  coord_flip() + 
  theme_bw(base_size = 16) + 
  labs(y='% of Topic',x='Words',
       title='Distribution of Top 10 words per Topic') + 
  scale_y_continuous(labels = scales::percent)

