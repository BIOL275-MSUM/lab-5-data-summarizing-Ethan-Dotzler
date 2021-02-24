

# load packages -----------------------------------------------------------

library(tidyverse) 


# load data --------------------------------------------------------------

iris
iris <- as_tibble(iris) 
iris


# Question #1 -------------------------------------------------------------

iris_new <- 
  rename( 
    iris, 
    sepal_length = Sepal.Length, 
    sepal_width = Sepal.Width, 
    petal_length = Petal.Length,
    petal_width = Petal.Width 
  )  

iris_new

# Question #2 -------------------------------------------------------------

iris_numbers <- select(iris_new, sepal_length, sepal_width, petal_length, petal_width, Species)

iris_numbers 

iris_cm <-
  mutate( 
    iris_numbers, 
    sepal_length = 10*sepal_length, 
    sepal_width = 10*sepal_width, 
    petal_length = 10*petal_length, 
    petal_width = 10*petal_width
  ) 

iris_cm

# Question #3 -------------------------------------------------------------

iris_area <- mutate(iris_cm, sepal_area = sepal_length * sepal_width, 
                    petal_area = petal_length * petal_width) %>%
  select(petal_area, sepal_area, Species) 

iris_area

# Question #4 -------------------------------------------------------------

count(iris_cm) 

summarize(
  iris_cm,
  sample_size = n(), 
  mean = mean(sepal_length), 
  max = max(sepal_length), 
  min = min(sepal_length), 
  range = max - min,
  median = median(sepal_length), 
  q1 = quantile(sepal_length, probs = 0.25), 
  q3 = quantile(sepal_length, probs = 0.75),
  iqr = IQR(sepal_length)
  )


# Question 5 -------------------------------------------------------------

iris_sum <-
  iris_cm %>%
  group_by(Species) %>%
  summarize(
    sample_size = n(),
    mean_w = mean(petal_width),
    str_dev = sd(petal_width),
    var = var(petal_width),
    sem = mean(petal_width) / sqrt(n()),
    ci_upper = mean_w + 2 * sem,
    ci_lower = mean_w - 2 * sem
  )

iris_sum

# Question 6 -------------------------------------------------------------

ggplot(data = iris_cm) + 
  geom_jitter(mapping = aes(x = Species, y = petal_width)) 


# Question 7 -------------------------------------------------------------

ggplot(data = iris_cm) + 
  geom_jitter(mapping = aes(x = Species, y = petal_width)) + 
  geom_crossbar( 
    data = iris_sum, 
    mapping = aes(x = Species, y = mean_w, ymax = ci_upper, ymin = ci_lower), 
    color = "lightslateblue"
    )

# Question 8 -------------------------------------------------------------

ggplot(data = iris_cm) + 
  geom_point(mapping = aes(x = petal_length, y = petal_width, color = Species))


