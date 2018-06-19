library(tidyverse)
library(stringr)
str_length(c('a','R for data science',NA))
x <- c('abc',NA)
str_c('|-',x,'-|')
str_c('|-',str_replace_na(x),'-|')

x <- c("Apple", "Banana", "Pear")
str_sub(x,1,1) <- str_to_lower(str_sub(x,1,1))

str_view(x,'an')
str_view(x,'.a.')

dot <- '\\.'
writeLines(dot)

str_view(c('abc','a.c','bef'),'a\\.c')

x <- 'a\\b'
writeLines(x)
str_view(x,'\\\\')

x <- c('apple','banana','pear')
str_view(x,'^a')
str_view(x,'a$')

x <- c('apple pie','apple','apple cake')
str_view(x,'apple')
str_view(x,'^apple$')

str_view(c('grey','gray'),'gr(e|a)y')

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x,'CC?')
str_view(x,'CC+')
str_view(x,'CC*')
str_view(x,'C[LX]+')
str_view(x, "C{2,3}")

no_vowels_1 <- !str_detect(words, "[aeiou]")

words[str_detect(words,'x$')]
str_subset(words,'x$')

df <- tibble(
  word=words,
  i=seq_along(word)
)
df %>% 
  filter(str_detect(words,'x$'))

mean(str_count(words,'[aeiou]'))

df %>% 
  mutate(
    vowels=str_count(word,'[aeiou]'),
    consonants=str_count(word,'^[aeiou]')
  )

str_count("abababa", "aba")
str_view_all("abababa", "aba")

length(sentences)
head(sentences)

colors <- c(
  "red", "orange", "yellow", "green", "blue", "purple"
)
color_match <- str_c(colors, collapse = "|")
color_match

has_color=str_subset(sentences,color_match)
matches <- str_extract(has_color,color_match)

more <- sentences[str_count(sentences,color_match)>1]
str_view_all(more,color_match)

noun <- '(a|the) ([^ ]+)'
has_noun <- sentences %>% 
  str_subset(noun) %>% 
  head(10)
has_noun %>% 
  str_extract(noun)

tibble(sentence = sentences) %>%
  tidyr::extract(
    sentence, c("article", "noun"), "(a|the) ([^ ]+)",
    remove = FALSE
  )

sentences %>%
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>%
  head(5)

bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")
str_view(bananas,regex('banana',ignore_case = T))

a1 <- "\u00e1"
a2 <- "a\u0301"
c(a1, a2)
str_detect(a1,fixed(a2))
str_detect(a1,coll(a2))

i <- c("I", "İ", "i", "ı")
str_subset(i, coll("i", ignore_case = TRUE))
str_subset(
  i,
  coll("i", ignore_case = TRUE, locale = "tr")
)


















































