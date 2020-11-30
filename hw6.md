HW6
================
Ruwen Zhou
11/30/2020

# Problem 1

Read in the data.

``` r
homicide_df =
  read_csv("data/data-homicides-master/homicide-data.csv") %>%
  mutate(
    city_state = str_c(city, state, sep = "_"),
    resolved = case_when(
      disposition == "Closed without arrest" ~ "unsolved",
      disposition == "Open/No arrest"        ~ "unsolved",
      disposition == "Closed by arrest"      ~ "solved"
    ),
    resolved = forcats::fct_relevel(resolved, "unsolved"),
    victim_age = as.numeric(victim_age),
    victim_sex = as.factor(victim_sex)
  ) %>%
  select(-city,-state,-victim_last,-victim_first,-lon,-lat) %>%
  filter(
    !str_detect(city_state, "Tulsa|Dallas|Phoenix|Kansas"),
    victim_race %in% c("White", "Black"),
    victim_sex %in% c("Male", "Female")
  ) %>%
  nest(c(-city_state))
```

Do linear regression

``` r
homicide_df =
  homicide_df %>%
  mutate(log = map(
    .x = data,
    ~ glm(
      resolved ~ victim_age + victim_sex + victim_race,
      data = .x,
      family = binomial()
    )
  ),
  result  = map(log, broom::tidy)) %>%
  select(-data) %>%
  unnest(result) %>%
  mutate(ci = map2(.x = log,
                   .y = term,
                   ~ confint(.x, .y))) %>%
  select(city_state, term, estimate, ci) %>%
  filter(term == "victim_raceWhite")
homicide_df %>%
  head() %>%
  knitr::kable()
```

| city\_state     | term              | estimate | ci             |
| :-------------- | :---------------- | -------: | :------------- |
| Albuquerque\_NM | victim\_raceWhite |    0.412 | \-0.419, 1.224 |
| Atlanta\_GA     | victim\_raceWhite |    0.269 | \-0.278, 0.843 |
| Baltimore\_MD   | victim\_raceWhite |    0.842 | 0.501, 1.187   |
| Baton Rouge\_LA | victim\_raceWhite |    0.447 | \-0.302, 1.238 |
| Birmingham\_AL  | victim\_raceWhite |  \-0.068 | \-0.592, 0.462 |
| Boston\_MA      | victim\_raceWhite |    2.365 | 1.53, 3.37     |
