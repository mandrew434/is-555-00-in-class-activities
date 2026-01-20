library(tidyverse)

# Because `starwars` is a "hidden" dataset in memory for demonstration, it won't 
# show up in our environment at first, and it'll also be hard to reset it if we 
# make a mistake. So assign it to `df` to make sure you can work with it.
df <- starwars

# glimpse turns a data frame on its side for viewing. Super useful.
df |> glimpse()


# iteratively add operations with the pipe operator: 
# height > 100, sex == female, 
# choose name, height, mass, species, films, 
# mass > 50, 
# arrange by mass
# note: columns can contain lists; more on that later
# note: filtering on some logical excludes NAs

df |> 
  filter(height > 100,
         sex == 'female') |> 
  select(name, height, mass, species,films) |> 
  filter(mass > 50) |> 
  arrange(mass)


# calculate a new column,weight_lbs = mass * 2.204623
# Make sure it gets saved to the tibble...
df <- df |> 
  mutate(weight_lbs = mass * 2.204623) |> 
  # mutate(mass = mass * 2.204623) |>  -- this assigns the variable back to the same column! Does not affect the original df
  glimpse()


# group and summarize. Pay attention to NAs
# get a count and mean mass by species

df |> 
  select(species, mass, weight_lbs) |>
  filter(!is.na(species)) |> 
  group_by(species) |> 
  summarize(count = n(),
            mass_avg = mean(mass, na.rm = TRUE),
            weight_avg = mean(weight_lbs, na.rm = TRUE),
            mass_max= max(mass, na.rm = TRUE),
            count_nas = sum(is.na(mass))) |> 
  arrange(desc(weight_avg))

# Lots of NAs for average... why? Even for large groups it's NA...


# Humans definitely have SOME mass data...


# so let's exclude NAs from the mean aggregation:

df |> 
  select(name,species, mass, weight_lbs) |>
  filter(!is.na(species)) |> 
  group_by(species) |>
  mutate(count = n(),
         mass_avg = mean(mass, na.rm = T)) |> 
  mutate(diff_from_mean = mass_avg - mass)

df |> 
  select(name, species, mass) |> 
  group_by(species) |> 
  slice_max(mass, n = 1)



# top 5 tallest people overall - without grouping this is simply finding the top five across all characters


# Here's another way to accomplish the same thing (note that slice_max/slice_min allow ties)



# vs. shortest 2 of each species. 
# When doing these operations within the grouping, we get very different 
# results. You can play around a bit with the ordering of the piped commands 
# to make sure you get a feel for what is going on.



# Grouping by multiple categoricals
# Was is the average birth year for each gender from each homeworld
# This is a simple example, but it groups first by homeworld, 
# then within each homeworld it groups by gender




# Grouping, then ungrouping, then grouping again:
# find average mass for each sex, but only for the tallest of each species.
# Again, a very contrived example, but understanding what's happening here will 
# be a good way for you to convince yourself that you've got a good handle on 
# the idea of grouping.




