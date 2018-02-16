library(tidyverse)

df_topics <- read_csv("topics_speech.csv")

df_test <- df_topics %>% select(year,speaker,contains("Topic_"))
test <- df_test %>% group_by(year) %>% arrange(speaker,.by_group=TRUE) 

df_sum <- df_test %>% select(-year) %>% group_by(speaker) %>%  summarise_all(funs(sum))

df_heatmap <- reshape2::melt(df_sum, id.vars = "speaker")
df_heatmap$speaker <- factor(df_heatmap$speaker,levels=rev(unique(test$speaker)))

# add percentage column
df_heatmap = group_by(df_heatmap, speaker) %>% mutate(percent = value/sum(value))

# plot percentage
ggplot(df_heatmap,aes(speaker,variable)) + 
  geom_tile(aes(fill=percent)) + 
  scale_fill_distiller(palette = "Spectral") + 
  labs(x="President",y="Topics",
       titles="Topics identified for each President's SOTU",
       subtitle="Ordered by Presidency") + 
  coord_flip() + 
  theme_bw(base_size = 16) + 
  scale_y_discrete(labels=as.character(seq(0,10)))

# --------------------------------------------------
# plot trump's speeches over time
mini_df = df_topics[df_topics$speaker=='Donald Trump',]
mini_df$date = with(mini_df, ISOdatetime(year, month, day,0,0,0))
mini_df = mini_df[,seq(9,19)]

mini_df = reshape2::melt(mini_df, id.vars = "date")
mini_df = group_by(mini_df, variable) %>% mutate(wt = value/sum(value))

# build the plot
ggplot(mini_df) +
  aes(x = variable, y = date, weight = wt, fill=variable) +
  geom_violin() + theme(legend.position="none") + 
  geom_jitter(shape=16, position=position_jitter(0),
              data=mini_df[mini_df$value==1,]) +
  labs(x="Topic",y="Date",
       titles="Distribution of Topics over Time for Donald Trump")
# --------------------------------------------------
# plot topic over years
df_year_sum <- df_test %>% select(-speaker)  %>% group_by(year) %>%  summarise_all(funs(sum))
df_year_heatmap <- reshape2::melt(df_year_sum, id.vars = "year")

# add percentage column
df_year_heatmap = group_by(df_year_heatmap, year) %>% mutate(percent = value/sum(value))


ggplot(df_year_heatmap) + 
  geom_line(aes(x=year,y=percent)) + 
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

