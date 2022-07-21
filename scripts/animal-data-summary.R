library(tidyverse)

dat <- read_rds(here::here("data/civ-bat-cov.rds"))

# n bats tested for cov
n_total <- n_distinct(dat$animal_id)

# confirm dates
unique(dat$event_date)

# confirm interfaces
unique(dat$disease_transmission_interfaces)

# n bats by season
dat |>
  distinct(animal_id, season) |>
  group_by(season) |>
  summarize(n = n(), perc = 100*n()/n_total)

# n bats by general, species
dat |>
  distinct(animal_id, genus, species) |>
  group_by(genus) |>
  summarize(n = n(), perc = 100*n()/n_total) |>
  arrange(-n)

# supp 1
dat |>
  distinct(animal_id, genus, species) |>
  group_by(genus, species) |>
  summarize(n = n(), perc = 100*n()/n_total) |>
  arrange(-n)

# figure 3
dat |>
  distinct(animal_id, genus, species) |>
  group_by(genus) |>
  count() |>
  ungroup() |>
  mutate(freq = formattable::percent(n / sum(n), digits = 1L)) |>
  mutate(genus = fct_reorder(genus, n)) |>
  mutate(label = paste0(n, " (", freq, ")")) |>
  ggplot(aes(x = genus, y = n)) +
  geom_bar(stat = "identity", fill = "gray70") +
  geom_text(aes(label=label, y = n+1), hjust = "left", size = 5) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 160)) +
  labs(x = "", y = "") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 14, color = "black"),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 14, hjust = 0, color = "black"),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# how many defined at species level
dat |>
  distinct(animal_id, genus, species) |>
  filter(species != "NULL"  ) |>
  nrow()

# reproductive ages
dat |>
  distinct(animal_id, genus, species, age_class) |>
  group_by(age_class) |>
  summarize(n = n(), perc = 100*n()/n_total)

# sex
dat |>
  distinct(animal_id, genus, species, sex) |>
  group_by(sex) |>
  summarize(n = n(), perc = 100*n()/n_total)

# number of specimen
n_distinct(dat$specimen_id)
dat |>
  distinct(animal_id, specimen_id, specimen_type) |>
  group_by(specimen_type) |>
  count()

# how many by test protocol
dat |>
  distinct(specimen_id, specimen_type, test_requested_protocol) |>
 group_by(specimen_id, specimen_type) |>
  summarize(test_type = paste(unique(test_requested_protocol), collapse = "; ")) |>
  group_by(test_type) |>
  count()

dat |>
  distinct(animal_id, test_requested_protocol) |>
  group_by(animal_id) |>
  summarize(test_type = paste(unique(test_requested_protocol), collapse = "; ")) |>
  group_by(test_type) |>
  count()

# positive results
pos <- dat |>
  filter(confirmation_result == "Positive")
pos$season
pos$test_requested_protocol
