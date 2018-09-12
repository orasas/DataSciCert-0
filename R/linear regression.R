library(Lahman)
library(tidyverse)
ds_theme_set()

head(Teams)
teams<-Teams
teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR/G, R_per_game = R/G) %>%
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = .5)

#correlation
library(HistData)
galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == 'male') %>%
  select (father, childHeight) %>%
  rename(son = childHeight)

galton_heights %>% summarize(mean(father), sd(father), mean(son), sd(son))

set.seed(0)
B <- 1000
N <- 25
R <- replicate(B, {
  sample_n(galton_heights, N, replace = T) %>%
    summarize(r=cor(father, son)) %>% .$r
})
data.frame(R) %>% ggplot(aes(R)) + geom_histogram(binwidth = .05, color = 'black')
