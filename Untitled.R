library(magrittr)
MASS::fitdistr(df$Distance, "normal")
MASS::fitdistr(df$Distance, "lognormal")
# MASS::fitdistr(df$Distance, "Poisson")    # non-interger
# MASS::fitdistr(df$Distance, "geometric")  # non-interger
MASS::fitdistr(dist_TBI$dist_cent, "exponential")

# rpois(12, )
MASS::fitdistr(dist_TBI$dist_cent, "normal") %$%
  rnorm(1000, estimate[1] , estimate[2]) %>%
  tibble(est = .) %>%
  ggplot(aes(x=est)) + geom_density(color="red") +
  geom_density(data=dist_TBI, aes(x=dist_cent))

MASS::fitdistr(dist_TBI$dist_cent, "lognormal") %$%
  rlnorm(3108, estimate[1] , estimate[2]) %>%
  tibble(est = .) %>%
  ggplot(aes(x=est)) + geom_density(color="red") +
  geom_density(data=dist_TBI, aes(x=dist_cent))
last_plot() + scale_x_log10()

MASS::fitdistr(dist_TBI$dist_cent, "exponential") %$%
  rexp(1000, estimate[1]) %>%
  tibble(est = .) %>%
  ggplot(aes(x=est)) + geom_density(color="red") +
  geom_density(data=dist_TBI, aes(x=dist_cent))
last_plot() + scale_x_log10()


# MASS::fitdistr(df$Distance, "Poisson") %$%
#   rpois(1000, estimate[1]) %>%
#   tibble(pois = .) %>%
#   ggplot(aes(x=pois)) + geom_density(color="red") +
#   geom_density(data=df, aes(x=Distance))




TBIs %>% # Deaths or Crude_Rate
  filter(!is.na(Deaths), Crude_Rate<75) %$%
  MASS::fitdistr(Crude_Rate, "exponential")

TBIs %>% # Deaths or Crude_Rate
  filter(!is.na(Deaths), Crude_Rate<75) %>%
  ggplot(aes(x=Crude_Rate)) + geom_density()




tibble(
  x = 0,
  y = c(2,3,4, 4,9,16),
  id = rep(1:3,2)
) %>%
  st_as_sf(coords = c("x", "y")) %>%
  st_cast(ids=rep(1:3,2))



filter(dist_TBI, !is.na(Deaths)) %>%
  mutate(Rate = 2 / StdPop*100000) %>%
  ggplot(aes(x=Rate)) + geom_density()




MASS::fitdistr(dist_TBI$dist_cent, "lognormal") %$%
  dlnorm(1:3108, estimate[1] , estimate[2]) %>%
  tibble(est = ., dis=1:1:3108) %>%
  ggplot(aes(x=dis,y=est)) + geom_line(color="red") +
  scale_x_log10()
