# A4 Data Wrangling

# Loading and Exploring Data -------------------------------- (**28 points**)

# To begin, you'll need to download the Kickstarter Projects data from the
# Kaggle website: https://www.kaggle.com/kemical/kickstarter-projects
# Download the `ks-projects-201801.csv` file into a new folder called `data/`

# Load the `dplyr` package
library(dplyr)

# Load your data, making sure to not interpret strings as factors
ks_projects <- read.csv("Data/ks-projects-201801.csv", stringsAsFactors = FALSE)

# To start, write the code to get some basic information about the dataframe:
# - What are the column names?
colnames(ks_projects)
# - How many rows is the data frame?
nrow(ks_projects)
# - How many columns are in the data frame?
ncol(ks_projects)

# Use the `summary` function to get some summary information
summary <- summarize(
  ks_projects,
  mean_pledged = mean(pledged, na.rm = TRUE),
  mean_usd.pledged = mean(usd.pledged, na.rm = TRUE),
  mean_usd_pledged_real = mean(usd_pledged_real, na.rm = TRUE)
)


# A function that takes as parameters a column name and a dataframe. If the
# values in column are *numeric*, the function returns a list with the keys:
# - `min`: the minimum value of the column
# - `max`: the maximum value of the column
# - `mean`: the mean value of the column
# If the column is *not* numeric and there are fewer than 10 unique values in
# the column, it returns a list with the keys:
# - `n_values`: the number of unique values in the column
# - `unique_values`: a vector of each unique value in the column
# If the column is *not* numeric and there are 10 or *more* unique values in
# the column, it returns a list with the keys:
# - `n_values`: the number of unique values in the column
# - `sample_values`: a vector containing a random sample of 10 column values
get_col_info <- function(col, df) {
  # Creates vector of unique values in column.
  unique_values <- unique(select(df, col))
  # Counts number of rows in new vector.
  n_values <- nrow(unique_values)

  # If the column values are of double type, it will follow this procedure.
  if (typeof(df$col) == "double") {
    # Returns list of the minimum, maximum, and mean value of the column.
    answer <- list(
      # Selects minimum value in column
      min = min(select(df, col), na.rm = TRUE),
      # Selects maxmimum value in column
      max = max(select(df, col), na.rm = TRUE),
      # At first I couldn't find a solution to find mean of the column without
      # getting an error, so I used the solution from this post:
      #https://stackoverflow.com/questions/35485536/r-argument-is-not-numeric-
      # or-logical-returning-na
      mean = mean(data.matrix(select(df, col)))
    )

    # If the column values aren't double type and have less than 10 unique
    # values, it will follow this procedure.
  } else if ((typeof(df$col) != "double") && (n_values <= 10)) {
    answer <- list(
      n_values = n_values,
      unique_values = unique_values
    )

    # If the column values aren't double type and have more than 10 unique
    # values, it will follow this procedure
  } else {
    answer <- list(
      n_values = n_values,
      # A sample of 10 unique column values
      sample_values = unique_values[sample(nrow(unique_values), 10), ]
    )
  }
  return(answer)
}

# Demonstrates that the function works by passing a column name and the
# kickstarter data to the function. Stores the result in a variable.
col_category <- get_col_info("category", ks_projects)


# A function that takes in a data frame  and returns a *list* of information
# for each column (where the *keys* of the returned list are the column names,
# and the _values_ are the summary information returned by the `get_col_info()`
# function.
get_summary_info <- function(df) {
  cols <- colnames(df)
  answer <- lapply(
    cols,
    get_col_info,
    df
  )
  return(answer)
}

# Demonstrates that function works by passing the kickstarter data into it and
# saving the result in a variable
summary_info <- get_summary_info(ks_projects)

# Take note of 3 observations that you find interesting from this summary
# information (and/or questions that arise that want to investigate further)
#
# For the ID column there are 378,661 unqiue values but for the name column
# there are 375,765. I wonder if there is a big difference between these two
# columns because I would assume the numbers to be very similar. Also, there
# are more unique values in category than in main_category, which I expected
# because main_category has more general terms. Finally, in the launched
# column, it only gives a sample of the date does not include the time, which
# is reported with every date. I wonder why it doesn't include the time.



# Asking questions of the data ----------------------------- (**29 points**)

# Write the appropriate dplyr code to answer each one of the following questions
# Make sure to return (only) the desired value of interest (e.g., use `pull()`)
# Store the result of each question in a variable with a clear + expressive name
# If there are multiple observations that meet each condition, the results
# can be in a vector. Make sure to *handle NA values* throughout!
# You should answer each question using a single statement with multiple pipe
# operations!

# What was the name of the project(s) with the highest goal?
highest_goal <- ks_projects %>%
  # Project with the highest goal.
  filter(goal == max(goal)) %>%
  # Name of project.
  select(name)


# What was the category of the project(s) with the lowest goal?
lowest_goal <- ks_projects %>%
  # Project(s) with the lowest goal
  filter(goal == min(goal)) %>%
  # Category of the project(s)
  select(category)


# How many projects had a deadline in 2018?
deadline_2018 <- ks_projects %>%
  # Projects with a deadline between January 1, 2018 and December 31, 2018.
  filter((deadline >= as.Date("2018-01-01")) & (deadline <=
           as.Date("2018-12-31"))) %>%
  # Number of projects listed.
  nrow()


# What proportion or projects weren't successful? Your result can be a decimal
unsuccess_projects <- ks_projects %>%
  # Summarizes the numbers of unsuccessful projects, successful projects, and
  # the proportion that weren't successful.
  summarize(
    num_unsuccess = sum(state == "failed" | state == "canceled" | state == 
                          "undefined" | state == "suspended"),
    num_success = sum(state == "successful"),
    unsuccess_proportion = ((num_unsuccess) / (num_success + num_unsuccess))
  ) %>%
  # Selects proportion decimal.
  select(unsuccess_proportion)


# What was the amount pledged for the project with the most backers?
amt_most_backers <- ks_projects %>%
  # Project with the most backers
  filter(backers == max(backers)) %>%
  # Name of the project.
  select(pledged)


# Of all of the projects that *failed*, what was the name of the project with
# the highest amount of money pledged?
fail_most_pledged <- ks_projects %>%
  # Projects that only failed.
  filter(state == "failed") %>%
  # The project with the highest money pledged
  filter(pledged == max(pledged)) %>%
  # The name of the project.
  select(name)


# How much total money was pledged to projects that weren't successful?
fail_total_money <- ks_projects %>%
  # Projects that weren't successful.
  filter(state == "failed" | state == "canceled" | state == "undefined" |
           state == "suspended") %>%
  # Selects 'pledged' columns.
  select(pledged) %>%
  # The total sum of pledged money.
  sum()



# Performing analysis by *grouped* observations ----------------- (38 Points)

# Which category had the most money pledged (total)?
category_most_pledged <- ks_projects %>%
  # Group all rows of the same category.
  group_by(category) %>%
  # Calculates sum of money pledged in that category.
  summarize(
    sum_pledged = sum(pledged)
  ) %>%
  # Category with the maximum money pledged.
  filter(sum_pledged == max(sum_pledged)) %>%
  # Selects name of category.
  select(category)


# Which country had the most backers?
country_most_backers <- ks_projects %>%
  # Group all rows of the same country.
  group_by(country) %>%
  # Calculates sum of backers in the country.
  summarize(
    sum_backers = sum(backers)
  ) %>%
  # Country with the most backers
  filter(sum_backers == max(sum_backers)) %>%
  # Selects abbreviation of country
  select(country)
  

# Which year had the most money pledged (hint: you may have to create a new
# column)?
year_most_pledged <- ks_projects %>%
  # Creates a column which shows the year the project was launched.
  mutate(
    year = format(as.Date(launched, format = "%Y-%m-%d"), "%Y")
  ) %>%
  # Group all rows of the same year
  group_by(year) %>%
  # Calculates sum of money pledged in each year.
  summarize(
    sum_pledged = sum(pledged)
  ) %>%
  # Year with the most money pledged.
  filter(sum_pledged == max(sum_pledged)) %>%
  # Selects year.
  select(year)


# What were the top 3 main categories in 2018 (as ranked by number of backers)?
top_main_2018 <- ks_projects %>%
  # Projects with a deadline between January 1, 2018 and December 31, 2018.
  filter((deadline >= as.Date("2018-01-01")) & (deadline <=
                                                  as.Date("2018-12-31"))) %>%
  # Group all rows of the same main category.
  group_by(main_category) %>%
  # Calculates sum of backers in main category.
  summarize(
    sum_backers = sum(backers)
  ) %>%
  # Arrange rows in descending order
  arrange(-sum_backers) %>%
  # Returns the top 3 main categories
  head(3)


# What was the most common day of the week on which to launch a project?
# (return the name of the day, e.g. "Sunday", "Monday"....)
common_day_launch <- ks_projects %>%
  # Creates a column which shows the weekday of the date launched.
  mutate(
    weekday = weekdays(as.Date(launched, format = "%Y-%m-%d"))
  ) %>% 
  # Group all rows of the same weekday.
  group_by(weekday) %>%
  # Calculate the number of occurrences of weekdays
  summarize(
    num_launches = n()
  ) %>%
  # Weekday with the most launches.
  filter(num_launches == max(num_launches)) %>%
  # Selects weekday.
  select(weekday)


# What was the least successful day on which to launch a project? In other
# words, which day had the lowest success rate (lowest proportion of projects
# that were successful)?
unsuccessful_day_launch <- ks_projects %>%
  # Creates a column which shows the weekday of the date launched.
  mutate(
    weekday = weekdays(as.Date(launched, format = "%Y-%m-%d"))
  ) %>%
  # Group all rows of the same weekday.
  group_by(weekday) %>%
  # Calculate the number of successful and unsuccessful projects on weekday.
  # Also calculates the success rate.
  summarize(
    num_unsuccess = sum(state == "failed" | state == "canceled" | state == 
                          "undefined" | state == "suspended"),
    num_success = sum(state == "successful"),
    success_proportion = ((num_success) / (num_success + num_unsuccess))
  ) %>%
  # Weekday with the lowest success rate.
  filter(success_proportion == min(success_proportion)) %>%
  # Selects weekday.
  select(weekday)