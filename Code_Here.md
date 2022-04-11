# Random_R_Code
Random R Code from Exercises that I do in DataCamp

tagged_answers %>%
### Aggregate by tag_name
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
