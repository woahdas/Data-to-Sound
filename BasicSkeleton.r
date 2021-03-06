#load and plot the MNIST database
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

mnist_raw <- read_csv("https://pjreddie.com/media/files/mnist_train.csv", col_names = FALSE)
pixels_gathered <- mnist_raw %>%  head(10000) %>%  rename(label = X1) %>%  mutate(instance = row_number()) %>%  gather(pixel, value, -label, -instance) %>%  tidyr::extract(pixel, "pixel", "(\\d+)", convert = TRUE) %>%  mutate(pixel = pixel - 2,         x = pixel %% 28,         y = 28 - pixel %/% 28)pixels_gathered
theme_set(theme_light())pixels_gathered %>%  filter(instance <= 12) %>%  ggplot(aes(x, y, fill = value)) +  geom_tile() +  facet_wrap(~ instance + label)


#create centroids of the numbers 0-9, which is just creating the 'average' 6 or 'average' 7 from this database
pixel_summary <- pixels_gathered %>%  group_by(x, y, label) %>%  summarize(mean_value = mean(value)) %>%  ungroup()pixel_summary
averagedigits <- library(ggplot2)pixel_summary %>%  ggplot(aes(x, y, fill = mean_value)) +  geom_tile() +  scale_fill_gradient2(low = "white", high = "black", mid = "gray", midpoint = 127.5) +  facet_wrap(~ label, nrow = 2) +  labs(title = "Average value of each pixel in 10 MNIST digits",       fill = "Average value") +  theme_void()

#play notes

library(tuneR)
f=392
sr=8000 
bits=16
secs=2           
amp=1
t=seq(0, secs, 1/sr)
y= amp*sin(2*pi*f*t)  
s=floor(2^(bits-2)*y) 
u=Wave(s, samp.rate=sr, bit=bits)
play(u)
