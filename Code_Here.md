# Random_R_Code
Random R Code from Exercises that I do in DataCamp

separate() separate_rows()
pivot_longer() pivot_wider()
complete() unnest_wider() unnest_longer()

planet_df %>% 
  # Unnest the moons list column over observations
  unnest_longer(moons) %>% 
  # Further unnest the moons column
  unnest_wider(moons) %>% 
  # Unnest the moon_data column
  unnest_wider(moon_data) %>% 
  # Get the top five largest moons by radius
  slice_max(radius, n = 5)

sensor_df %>% 
  # Complete the time column with a 20 minute interval
  complete(time = seq(min(time), max(time), by = "20 min"),
           fill = list(enter = 0L, exit = 0L)) %>%
  # Calculate the total number of people inside
  mutate(total_inside = cumsum(enter + exit)) %>% 
  # Pivot the enter and exit columns to long format
  pivot_longer(enter:exit, names_to = "direction", values_to = "n_people") %>% 
  # Plot the number of people over time, fill by direction
  ggplot(aes(x = time, y = n_people, fill = direction)) +
  geom_area() +
  geom_line(aes(y = total_inside))


patient_df %>% 
  # Pivot the infected and recovered columns to long format
  pivot_longer(-patient, names_to = "status", values_to = "date") %>% 
  select(-status) %>% 
  # Group by patient
  group_by(patient) %>% 
  # Complete the date range per patient using full_seq()
  complete(date = full_seq(date, period = 1)) %>% 
  # Ungroup the data
  ungroup() %>% 
  # Count the dates, the count goes in the n_sick variable
  count(date, name = "n_sick") %>% 
  ggplot(aes(x = date, y = n_sick))+
  geom_line()

medal_df %>% 
  # Give each continent an observation at each Olympic event
  complete(
    continent, 
    nesting(season, year), 
    fill = list(medals_per_participant = 0L)
  ) %>%
  # Plot the medals_per_participant over time, colored by continent
  ggplot(aes(x = year, y = medals_per_participant, color = continent)) +
  geom_line() +
  facet_grid(season ~ .)

cumul_nukes_1962_df %>% 
  # Complete the dataset
  complete(country, date = full_seq(date, period = 1)) %>% 
  # Group the data by country
  group_by(country) %>% 
  # Impute missing values with the last known observation
  fill(total_bombs) %>% 
  # Plot the number of bombs over time, color by country
  ggplot(aes(x = date, y = total_bombs, color = country)) +
  # These two lines will mark the Cuban Missile Crisis 
  geom_rect(xmin = as.Date("1962-10-16"), xmax = as.Date("1962-10-29"), ymin = -Inf, ymax = Inf, color = NA)+ 
  geom_text(x = as.Date("1962-10-22"), y = 15, label = "Cuban Missile Crisis", angle = 90, color = "white")+
  geom_line()

outer_dates <- c(as.Date("1980-01-01"), as.Date("1980-12-31"))
# Generate the dates for all days in 1980
full_seq(outer_dates, period = 1)

medal_df %>% 
  # Count the medals won per team and year
  count(team, year, name = "n_medals") %>% 
  # Complete the team and year variables, fill n_medals with zeros
  complete(team, year, fill = list(n_medals = 0L)) %>% 
  # Plot n_medals over year, colored by team
  ggplot(aes(x = year, y = n_medals, color = team)) +
  geom_line() +
  scale_color_brewer(palette = "Paired")


planets = c("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune")

planet_df %>% 
  complete(
    # Complete the planet variable
    planet = planets,
    # Overwrite NA values for n_moons with 0L
    fill = list(n_moons = 0L)
  )

# Create a tibble with all combinations of years and species
full_df <- expand_grid(
  year = 1951:1970, 
  species = c("Human", "Dog")
)

space_df %>% 
  # Join with full_df so that missing values are introduced
  right_join(full_df, by = c("year", "species")) %>% 
  # Overwrite NA values for n_in_space with 0L
  replace_na(list(n_in_space = 0L)) %>% 
  # Create a line plot with n_in_space over year, color by species
  ggplot(aes(x = year, y = n_in_space, color = species)) +
  geom_line()


letters <- c("A", "C", "G", "U")

# Create a tibble with all possible 3 way combinations
codon_df <- expand_grid(
  letter1 = letters,
  letter2 = letters,
  letter3 = letters
)

codon_df %>% 
  # Unite these three columns into a "codon" column
  unite("codon", letter1:letter3, sep = "")

space_dogs_df %>% 
  # Pivot the data to a wider format
  pivot_wider(names_from = dog_id, values_from = gender, names_prefix = "gender_") %>% 
  # Drop rows with NA values
  drop_na() %>% 
  # Create a Boolean column on whether both dogs have the same gender
  mutate(same_gender = gender_1 == gender_2)%>% 
  summarize(pct_same_gender = mean(same_gender))

stock_df %>% 
  # Pivot the data to create 3 new columns: year, week, price
  pivot_longer(
    -company,
    names_to = c("year", "week"),
    values_to = "price",
    names_sep = "_week",
    names_transform = list(
      year = as.integer,
      week = as.integer)
  ) %>%
  # Create a line plot with price per week, color by company
  ggplot(aes(x = week, y = price, color = company)) +
  geom_line() +
  facet_grid(. ~ year)

bird_df %>%
  # Pivot the data to create a 2 column data frame
  pivot_longer(
    starts_with("points_"),
    names_to = "points",
    names_prefix = "points_",
    names_transform = list(points = as.integer),
    values_to = "species",
    values_drop_na = TRUE
  ) %>%
  group_by(species) %>% 
  summarize(total_points = sum(points)) %>% 
  slice_max(total_points, n = 5)


bond_df %>% 
  # Pivot the data to long format
  pivot_longer(
    -Bond, 
    # Overwrite the names of the two newly created columns
    names_to = "decade", 
    values_to = "n_movies", 
    # Drop na values
    values_drop_na = TRUE, 
    # Transform the decade column data type to integer
    names_transform = list(decade = as.integer)) %>% 
  ggplot(aes(x = decade + 5, y = n_movies, fill = Bond))+
  geom_col()

obesity_df %>% 
  # Pivot the male and female columns
  pivot_longer(c(male, female), names_to = "sex",
               values_to = "pct_obese") %>% 
  # Create a scatter plot with pct_obese per country colored by sex
  ggplot(aes(x = pct_obese, color = sex,
             y = forcats::fct_reorder(country, both_sexes))) +
  geom_point() +
  scale_y_discrete(breaks = c("India", "Nauru", "Cuba", "Brazil",
                              "Pakistan", "Gabon", "Italy", "Oman",
                              "China", "United States of America")) +
  labs(x = "% Obese", y = "Country")


nuke_df %>% 
  # Pivot the data to a longer format
  pivot_longer(
    -year, 
    # Overwrite the names of the two new columns
    names_to = "country", 
    values_to = "n_bombs") %>% 
  # Replace NA values for n_bombs with 0L
  replace_na(list(n_bombs = 0L)) %>% 
  # Plot the number of bombs per country over time
  ggplot(aes(x = year, y = n_bombs, color = country)) +
  geom_line()

drink_df %>% 
  # Separate the ingredients over rows
  separate_rows(ingredients, sep = "; ") %>% 
  # Separate ingredients into three columns
  separate(
    ingredients, 
    into = c("ingredient", "quantity", "unit"), 
    sep = " ", 
    convert = TRUE
  ) %>% 
  # Group by ingredient and unit
  group_by(ingredient, unit) %>% 
  # Calculate the total quantity of each ingredient
  summarize(quantity = sum(quantity))

select() filter() mutate() group_by()
summarize() 

# Remove parentheses from phone column
phone_no_parens <- sfo_survey$phone %>%
  # Remove "("s
  str_remove_all(fixed("(")) %>%
  # Remove ")"s
  str_remove_all(fixed(")"))

# Add phone_no_parens as column
sfo_survey %>%
  mutate(phone_no_parens = phone_no_parens,
  # Replace all hyphens in phone_no_parens with spaces
         phone_clean = str_replace_all(phone_no_parens, "-", " "))

--------------------------------------------------------------------
# XLConnect is already available

# Build connection to urbanpop.xlsx
my_book <- loadWorkbook("urbanpop.xlsx")

# Add a worksheet to my_book, named "data_summary"
createSheet(my_book, "data_summary")

# Create data frame: summ
sheets <- getSheets(my_book)[1:3]
dims <- sapply(sheets, function(x) dim(readWorksheet(my_book, sheet = x)), USE.NAMES = FALSE)
summ <- data.frame(sheets = sheets,
                   nrows = dims[1, ],
                   ncols = dims[2, ])

# Add data in summ to "data_summary" sheet
writeWorksheet(my_book, summ, sheet = "data_summary")

# Save workbook as summary.xlsx
saveWorkbook(my_book, file = "summary.xlsx")

--------------------------------------------------------------------
# Turn data into correct dataframe format
film_by_character <- tibble(filmtitle = map_chr(sw_films, "title")) %>%
    mutate(filmtitle, characters = map(sw_films, "characters")) %>%
    unnest()

# Pull out elements from sw_people
sw_characters <- map_df(sw_people, `[`, c("height", "mass", "name", "url"))

# Join the two new objects
character_data <- inner_join(film_by_character, sw_characters, by = c("characters" = "url")) %>%
    # Make sure the columns are numbers
    mutate(height = as.numeric(height), mass = as.numeric(mass))

# Plot the heights, faceted by film title
ggplot(character_data, aes(x = height)) +
  geom_histogram(stat = "count") +
  facet_wrap(~ filmtitle)


-----------------------------------------------------------------
# List of 1, 2 and 3
means <- list(1, 2, 3)

# Create sites list
sites <- list("north", "west", "east")

# Map over two arguments: sites and means
list_of_files_map2 <- map2(sites, means, ~data.frame(sites = .x,
                           a = rnorm(mean = .y, n = 200, sd = (5/2))))

list_of_files_map2

# List of sites north, east, and west
sites <- list("north", "east", "west")

# Create a list of dataframes, each with a years, a, and b column 
list_of_df <-  map(sites,  
  ~list(sites = .x,
       a = rnorm(mean = 5, n = 200, sd = (5/2)),
       b = rnorm(mean = 200, n = 200, sd = 15)))

# print list_of_df
list_of_df

the Map function outputs a list. sometimes this can be use ful, but we may want a vector output instead.
map(object, function)


tagged_answers %>%
### Aggregate by tag_name
    group_by(tag_name) %>%
### Summarize questions and average_answers
    summarize(questions = n(),
              average_answers = mean(n)) %>%
### Sort the questions in descending order
    arrange(desc(questions))

stock_df %>% 
### Pivot the data to create 3 new columns: year, week, price
  pivot_longer(
    -company,
    names_to = c("year", "week"),
    values_to = "price",
    names_sep = "_week",
    names_transform = list(
      year = as.integer,
      week = as.integer)
  ) %>%
### Create a line plot with price per week, color by company
  ggplot(aes(x = week, y = price, color = company)) +
  geom_line() +
  facet_grid(. ~ year)
  
  complete(time= seq(from = min(time), to = max(time), by = "20 min"))

### Left join the Millennium Falcon colors to the Star Destroyer colors
millennium_falcon_colors %>%
  left_join(star_destroyer_colors, by = c("color_id"), suffix = c("_falcon", "_star_destroyer"))
### Aggregate Star Destroyer for the total quantity in each part
star_destroyer_colors <- star_destroyer %>%
  group_by(color_id) %>%
  summarize(total_quantity = sum(quantity))
### Aggregate Millennium Falcon for the total quantity in each part
millennium_falcon_colors <- millennium_falcon %>%
  group_by(color_id) %>%
  summarize(total_quantity = sum(quantity))
 
 parts %>%
	count(part_cat_id) %>%
	right_join(part_categories, by = c("part_cat_id" = "id")) %>%
	# Use replace_na to replace missing values in the n column
	replace_na(list(n = 0))
 
 batman_parts %>%
  ### Combine the star_wars_parts table 
full_join(star_wars_parts, by = c("part_num", "color_id"), suffix = c("_batman", "_star_wars")) %>%
  ### Replace NAs with 0s in the n_batman and n_star_wars columns 
replace_na(list(n_batman = 0, n_star_wars = 0))

 batman_colors %>%
### Join the Batman and Star Wars colors
  full_join(star_wars_colors, by = "color_id", suffix = c("_batman", "_star_wars")) %>%
### Replace NAs with 0 in the total_batman and total_star_wars columns
  replace_na(list(total_batman = 0, total_star_wars = 0)) %>%
  inner_join(colors, by = c("color_id" = "id")) %>%
### Create the difference and total columns
  mutate(difference = fraction_batman - fraction_star_wars,
         total = total_batman + total_star_wars) %>%
### Filter for totals greater than 200
  filter(total, total >= 200)

### Create a bar plot using colors_joined and the name and difference columns
ggplot(colors_joined, aes(name, difference, fill = name)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = color_palette, guide = FALSE) +
  labs(y = "Difference: Batman - Star Wars")
