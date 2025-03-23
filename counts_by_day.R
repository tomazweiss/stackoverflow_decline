
library(tidyverse)
library(janitor)
library(ggplot2)
library(slider)

# renv::init()


# data import -------------------------------------------------------------

data <- read_csv('data/counts_by_day.csv') |> clean_names()


# data preparation --------------------------------------------------------

data <- 
  data |> 
  mutate(day = as.Date(day)) |> 
  arrange(day) |> 
  mutate(rolling_avg = slider::slide_period_dbl(
    .x = n_questions,
    .i = day,
    .period = "day",
    .f = mean,
    .before = 27,
    .complete = FALSE
  ))



# plot --------------------------------------------------------------------

# arrow location
arrow_x <- as.Date("2022-11-30")
arrow_y <- data$rolling_avg[data$day == arrow_x]  # corresponding y value


fig <- 
  data |> 
  ggplot(aes(x = day, y = rolling_avg)) +
  geom_line(size = 1, color = 'steelblue') + 
  scale_x_date(date_breaks = "1 year", 
               date_labels = "%Y", 
               minor_breaks = NULL, 
               expand = expansion(add = c(30, 30))) +
  scale_y_continuous(breaks = seq(0, 7000, by = 1000), expand = expansion(add = c(5, 80))) +
  labs(title = "Number of Stack Overflow Questions over Time", 
       subtitle = 'Showing 28 day rolling average.',
       x = "Date", 
       y = "Number of Questions") +
  annotate("text", x = max(data$day), y = 6600, 
           label = "tomazweiss.github.io \nData Source: StackExchange Data Explorer", 
           hjust = 1, vjust = 0, size = 4) +
  theme_light() +
  annotate("segment", x = arrow_x + 100, y = arrow_y + 1000, xend = arrow_x + 0, yend = arrow_y + 150,
           arrow = arrow(type = "closed", length = unit(0.15, "inches")), color = "black") +
  annotate("text", x = arrow_x + 350, y = arrow_y + 1100, label = "ChatGPT launches",
           hjust = 1, size = 4, color = "black")

fig

ggsave('plots/timeseries.png', plot = fig, width = 400, height = 200, units = 'mm', dpi = 'retina')


