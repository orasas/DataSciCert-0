library(dslabs)
require(tidyverse)
library(HistData)
galton_heights <- Galton

r <- galton_heights %>% summarize(r = cor(parent, child)) %>% .$r

galton_heights %>% mutate(father = round(parent)) %>% group_by(father) %>% summarize(son = mean(child)) %>%
  mutate(z_father = scale(father), z_son = scale(son)) %>%
  ggplot(aes(z_father, z_son)) +
  geom_point() +
  geom_abline(intercept = 0, slope = r)

?sapply

rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

beta1 = seq(0, 1, len=nrow(galton_heights))

results <- data.frame(beta1 = beta1, rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)


fit <- lm(child~parent, data =  galton_heights)
fit

library(Lahman)
teams <- Teams
teams <- teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR/G, R_per_game = R/G, BB_per_game = BB/G)


fit <- lm(R_per_game ~ BB_per_game + HR_per_game, data = teams)
fit

#father son
galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == 'male') %>%
  select (father, childHeight) %>%
  rename(son = childHeight)

B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef 
})


cor(lse[1,], lse[2,])

