# STAT 574 HW4 Problem 6

#install.packages("gutenbergr")
#install.packages("stringr")
#install.packages("tidytext")
#install.packages("stopwords")
#install.packages("tibble")
#install.packages("wordcloud")
#install.packages("ggplot2")

library(readr)
library(dplyr)
library(gutenbergr)
library(stringr)
library(tidytext)
library(stopwords)
library(tibble)
library(wordcloud)
library(ggplot2)

# Book selected: The White Company by Sir Arthur Conan Doyle

book_sel = gutenberg_download(903, meta_fields="author")

book_sel = as_tibble(book_sel %>%
mutate(document=row_number()) %>%
select(-gutenberg_id))

tidy_book = book_sel %>% unnest_tokens(word, text) %>%
group_by(word) %>% filter(n()>10) %>%
ungroup()

# Identifying and dropping stopwords from text. 

stopword = as_tibble(stopwords::stopwords("en"))
stopword = rename(stopword, word=value)
tb = anti_join(tidy_book, stopword, by='word')

word_count = tb %>% count(word, sort=T)
print(head(word_count, 25))

# Displaying top 25 words. 

print(tb %>%
count(author, word, sort=T) %>%
filter(n>=190) %>%
mutate(word=reorder(word,n)) %>%
ggplot(aes(word,n)) +
geom_col(aes(fill=author)) +
xlab(NULL) + 
scale_y_continuous(expand=c(0, 0)) +
coord_flip() +
theme_classic(base_size=12) +
labs(fill="Author", title="Word Frequency", subtitle="25 top words")+
theme(plot.title = element_text(lineheight=.8, face="bold"))+
scale_fill_brewer())

# Plotting word cloud.

tb %>%
count(word) %>%
with(wordcloud(word, n, max.words=25, colors=brewer.pal(10, "Set1")))