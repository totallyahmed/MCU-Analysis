# Loading Packages
library(tidyverse)
library(lubridate)
library(ggplot2)

# Importing Data
mcu <- read_csv("C:\\Users\\Ahmed Hussain\\Desktop\\mcu_box_office.csv")
str(mcu)
mcu$release_date <- as.Date(mcu$release_date,"%m/%d/%Y")
is.Date(mcu$release_date)
anyDuplicated(mcu)
sum(is.na(mcu))


# Data Analysis
mcu %>%
  group_by(mcu_phase) %>%
  count(mcu_phase) %>%
  arrange(mcu_phase)

mcu %>%
  ggplot(mapping=aes(x = worldwide_box_office, y = production_budget)) +
  geom_point() +
  labs(title = 'Correlation Between Worldwide Box Office Revenue & Production Budget',
       x = 'Worldwide Box Office',
       y = 'Production Budget')
#Phase 4 has the lowest amount of movies as it just started.
#Interstingly Phase 1 and 2 has the same amount of movies.
#Phase 3 has the most amount of movies.

mcu %>%
  ggplot(aes(x=worldwide_box_office,y=reorder(movie_title,release_date))) +
  geom_col(color = 'black', fill = 'cadetblue3') +
  labs(title = 'MCU Movie Worldwide Box Office Revenue', 
       subtitle = 'Sorted by release date',
       x = 'Worldwide box office revenue', 
       y = 'Movie title')

mcu %>%
  ggplot(aes(x=worldwide_box_office,y=reorder(movie_title,worldwide_box_office))) +
  geom_col(color = 'black', fill = 'cadetblue3') +
  labs(title = 'MCU Movie Worldwide Box Office Revenue', 
       subtitle = 'Sorted by highest revenue',
       x = 'Worldwide box office revenue', 
       y = 'Movie title')

#COVID had on the beginning of phase 4.
#Black Widow, Shang-Chi and the Legend of the Ten Rings, and Eternals didn't even cross $500,000.
#Avengers movie made it into the top 5 in worldwide box office revenue. 
#Spider-Man: No Way Home made it at the 3rd spot.

phase_1 <- filter(mcu, mcu_phase == 1)
phase_2 <- filter(mcu, mcu_phase == 2)
phase_3 <- filter(mcu, mcu_phase == 3)
phase_4 <- filter(mcu, mcu_phase == 4)

phase_1_sum <- sum(phase_1$worldwide_box_office)
phase_2_sum <- sum(phase_2$worldwide_box_office)
phase_3_sum <- sum(phase_3$worldwide_box_office)
phase_4_sum <- sum(phase_4$worldwide_box_office)

phase_1_sum
phase_2_sum
phase_3_sum
phase_4_sum

slices <- c(phase_1_sum, phase_2_sum, phase_3_sum, phase_4_sum)
lbls <- c("Phase 1", "Phase 2", "Phase 3", "Phase 4")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls, "%", sep="")
pie(slices, labels = lbls, col = rainbow(length(lbls)), main = "Pie Chart of Worldwide Box Office Revenue by MCU Phase")
#With 11 movies in phase 3 including both Avengers: Infinity War and Endgame, it's no suprise that phase 3 carries the majority of the MCU's total worldwide box office revenue.


mean(mcu$movie_duration)
mcu %>% group_by(mcu_phase) %>% summarise(mean_movie_duration = mean(movie_duration))
#The movie duration has increased with each phase of the MCU.
mcu %>%
  ggplot(aes(x=movie_duration,y=reorder(movie_title,release_date))) +
  geom_col(color = 'black', fill = 'cornsilk3') +
  geom_vline(xintercept = 132, linetype = 'longdash') +
  labs(title = 'MCU Movie Durations', 
       subtitle = 'Sorted by release date',
       caption = 'Dashed line represents a mean of 131.93 minutes',
       x = 'Movie duration in minutes', 
       y = 'Movie title')

#Compare this to phase 1, where every movie is below the mean besides The Avengers.
#Every Avenger movie places well above the mean, with Avengers: Endgame being by far the longest movie.

mcu %>%
  ggplot(mapping=aes(x = tomato_meter, y = audience_score)) +
  geom_point() +
  labs(title = 'Correlation Between Tomato Meter and Audience Score',
       x = 'Tomato meter',
       y = 'Audience score')
#This shows us that both scores generally trend in the same direction, with two clear outliers.
#To visualize the difference between the two scores and get a better idea of these outliers, let's create another column that calculates the difference between the two scores.
#We'll also assign two colors to indicate positive and negative values.

mcu$score_difference <- mcu$tomato_meter - mcu$audience_score
color <- ifelse(mcu$score_difference < 0, "pink", "lightblue")

mcu %>%
  ggplot(aes(x = reorder(movie_title,release_date), y = score_difference)) + 
  geom_bar(stat = 'identity', 
           show.legend = FALSE,
           fill = color,
           color = 'black') +
  geom_text(aes(label = score_difference, 
                hjust = ifelse(score_difference < 0, 1.5, -1),
                vjust = 0.5), 
            size = 2.25) +
  labs(title = 'Tomato Meter and Audience Score Difference', 
       subtitle = 'Sorted by release date',
       caption = 'Calculated as tomato meter minus audience score') +
  xlab("Movie title") + 
  ylab('Score difference') + 
  coord_flip() + 
  theme_minimal()
#The two outliers from the correlation chart were Eternals at -31 and Captain Marvel at 34.
#It appears that critics reviewed Eternals pretty negatively, and that the audience heavily disliked Captain Marvel.

mcu %>%
  ggplot(aes(x = reorder(movie_title,score_difference), y = score_difference)) + 
  geom_bar(stat = 'identity', 
           show.legend = FALSE,
           fill = color,
           color = 'black') +
  geom_text(aes(label = score_difference, 
                hjust = ifelse(score_difference < 0, 1.5, -1),
                vjust = 0.5), 
            size = 2.25) +
  labs(title = 'Tomato Meter and Audience Score Difference', 
       subtitle = 'Sorted by score difference',
       caption = 'Calculated as tomato meter minus audience score') +
  xlab("Movie title") + 
  ylab('Score difference') + 
  coord_flip() + 
  theme_minimal()

#The Avengers and Guardians of the Galaxy were the only two movies where both scores were identical.
#Almost every movie's score difference was less than 10 besides 4 movies: Captain Marvel, Black Panther, Black Widow, and Eternals.