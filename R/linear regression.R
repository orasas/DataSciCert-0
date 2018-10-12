library(Lahman)
library(tidyverse)
ds_theme_set()

head(Teams)
teams<-Teams
teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR/G, R_per_game = R/G, BB_per_game = BB/G) %>%
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


##runs model
require(broom)

fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G,
         singles = (H-X2B-X3B-HR)/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR=HR/G,
         R=R/G) %>%
  lm(R~ BB + singles + doubles + triples + HR, data = .)

coefs <- tidy(fit, conf.int = T)
coefs

Teams %>%
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G,
         singles = (H-X2B-X3B-HR)/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR=HR/G,
         R=R/G) %>%
  mutate(R_hat = predict(fit, newdata=.))


pa_per_game <- Batting %>% 
  filter(yearID == 2002) %>%
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/ max(G)) %>%
  .$pa_per_game %>%
  mean
  

players <- Batting %>% 
  filter(yearID %in% 1999:2001) %>%
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>% 
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G,
            triples = sum(X3B)/G,
            HR=sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >=300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata=.))

players <- Salaries%>%
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by= "playerID")

players %>% ggplot(aes(R_hat)) + 
  geom_histogram(binwidth = .5, color = 'black')

players <- Fielding %>%
  filter(yearID == 2002) %>%
  filter(!POS %in% c('OF', 'P')) %>%
  group_by(playerID) %>%
  top_n(1,G) %>% 
  filter(row_number(G) == 1) %>%
  ungroup() %>%
  select(playerID, POS) %>%
  right_join(players, by='playerID') %>%
  filter(!is.na(POS) & !is.na(salary))

players <- Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  right_join(players, by = 'playerID')

players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>%
  arrange(desc(R_hat)) %>%
  top_n(10)

players %>% 
  filter(debut < 1997) %>%
  ggplot(aes(salary, R_hat, color = POS)) +
  geom_point() +scale_x_log10()


##linear programming
library(reshape2)
library(lpSolve)

players <- players %>% filter(debut <= 1997 & debut > 1988)
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
npos <- nrow(constraint_matrix)
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1, npos), 50*10^6)
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int = TRUE)
#algorithm chooses 9 players
our_team <- players %>%
  filter(lp_solution$solution == 1) %>%
  arrange(desc(R_hat))

our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)

my_scale <- function(x) (x - median(x))/mad(x)
players %>% mutate(BB = my_scale(BB), 
                   singles = my_scale(singles),
                   doubles = my_scale(doubles),
                   triples = my_scale(triples),
                   HR = my_scale(HR),
                   AVG = my_scale(AVG),
                   R_hat = my_scale(R_hat)) %>%
  filter(playerID %in% our_team$playerID) %>%
  select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>%
  arrange(desc(R_hat))
