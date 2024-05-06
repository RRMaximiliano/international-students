
# Packages ----------------------------------------------------------------

library(tidyverse)
library(xlsx)
library(janitor)
library(hrbrthemes)

data <- read.xlsx("data/Census_All-Places-of-Origin_OD23.xlsx", sheetName = "copy") %>%
  clean_names()

# Cleaning ----------------------------------------------------------------

## I actually just care of CA, so I can just clean those rows plus the vars

centro <- c("Nicaragua", "Costa Rica", "Belize", "Guatemala", "El Salvador", "Panama", "Honduras")

ca_data <- data %>%
  filter(
    place_of_origin %in% centro
  ) %>%
  as_tibble() %>%
  select(-na)

ca_data_long <- ca_data %>%
  rename_with(~ str_remove(., "x"), everything()) %>%
  select(-`_total`, -`_change`) %>%
  pivot_longer(-place_of_origin) %>%
  mutate(
    value = as.numeric(value)
  ) %>%
  rename(year = name, students = value)

ca_data_long %>%
  # mutate(
  #   year = str_extract(year, ".*(?=_)"),
  #   year = as.numeric(year)
  # ) %>%
  ggplot(
    aes(
      x = year,
      y = students,
      group = place_of_origin,
      fill = place_of_origin
    )
  ) +
  geom_col(color = "black") +
  facet_wrap(~place_of_origin) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ",")) +
  labs(
    x = "Academic Year",
    y = "Total number of International Students",
    title = "International Students by Place of Origin",
    subtitle = "Selected Years, 1949/50 - 2022/23"
  ) +
  theme_ipsum_rc() +
  theme(
    plot.title = element_text(size = rel(2)),
    plot.subtitle = element_text(size = rel(1.5)),
    legend.position = "none"
  )

ggsave("figs/fig1.png", dpi = 320, bg = "white", height = 10, width = 16)
