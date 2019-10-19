ny = read.csv('new-york-city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv', na.strings = c("", "NA"))

library(ggplot2)


## 1. For 90's kids in New York, which cohort (by year) used bikeshare the most in terms of number of trips?

# Create a plot that counts the number of trips made by individuals of birth years between 1990 to 1999.
ggplot(aes(x = Birth.Year), data = subset(ny, Birth.Year >= 1990 & Birth.Year < 2000)) +
  geom_histogram(binwidth = 1, color = 'black') +
  scale_x_continuous(breaks = seq(1990, 1999, by = 1)) +
  ggtitle('NY 90\'s Kids Bikeshare Use') +
  labs(x = 'Birth Year of Users', y = 'Number of Trips') +
  stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust=-1.5)

# Count the number of trips made by people born in each year of the 90s, then create a matrix of trip count by birth year.
nineties_years = seq(1990, 1999, 1)
trip_count_by_birth_year = c()
i = 1
for (year in nineties_years) {
  trip_count_by_birth_year[i] = dim(subset(ny, Birth.Year == year))[1]
  i = i+1
  }
year_trip_matrix = cbind(nineties_years, trip_count_by_birth_year)
print(year_trip_matrix)

# Find the index (row) in the matrix where the maximum trip count value is, then return the corresponding birth year at that index (row).
max_index = which(year_trip_matrix[,2] == max(trip_count_by_birth_year))
most_popular_year = year_trip_matrix[,1][max_index]
print(most_popular_year)

### Of the hip 90's NY kids, the bikeshare seems to be most popular with the cohort born in 1990 with a total of 2027 trips.


## 2. For Chicago, is there a difference in trip duration depending on gender?

# Create boxplots of trip duration (converted from seconds to minutes), aggregated by gender. Note that blank entries in the gender data have been converted to NA when reading the csv file.
ggplot(aes(x = Gender, y = Trip.Duration/60), data = subset(chi, !is.na(Gender))) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 25)) +
  ggtitle('Differences in Chicago Users\' Trip Duration by Gender') +
  labs(x = 'Gender', y = 'Trip Duration (in Minutes)')

# Get summary statistics corresponding to those boxplots.
by(chi$Trip.Duration/60, chi$Gender, summary)

### When looking at measures of central tendency, it looks like there was a difference, with females having a greater trip duration than males. Specifically, the means were about 13 minutes to 11, and the medians about 11 minutes to 9.  This pattern also holds up when we compare the 1st quartiles between genders (females 7 minutes, males 6 minutes) or the 3rd quartiles (females 17 minutes, males 14 minutes).

## 3. For Washington, of the rentals lasting over 24 hours, which start station was the most popular?

# Create a subset of the data where trip duration is converted from seconds to hours then satisfies the condition of being over 24 hours.
trips_over_24hrs = subset(wash, Trip.Duration/3600 > 24)

# Count the number of occurrances for each starting station in this subset. Create a matrix of each station and it's corresponding count, one entry per unique station.
start_stations = trips_over_24hrs[,5]
i = 1
unique_station_list = c()
rental_count_per_station = c()
for (station in start_stations) {
  if (!(station %in% unique_station_list)) {
    rental_count_per_station[i] = dim(subset(trips_over_24hrs, start_stations == station))[1]
    unique_station_list[i] = station
    i = i + 1}}
station_count_matrix = cbind(unique_station_list, rental_count_per_station)
print(station_count_matrix)

# Find where in the matrix the entries with the max rental counts are located, then print the corresponding station names.
max_indicies = which(station_count_matrix[,2] == max(rental_count_per_station))
most_popular_stations = c()
j = 1
for (index in max_indicies) {
  most_popular_stations[j] = station_count_matrix[,1][index]
  j = j + 1
}

# Print the answer to the question verbally.
cat('The most popular station is: ', most_popular_stations, sep='\n')
cat('This station saw', max(rental_count_per_station), 'rentals.')

# Create a bar graph of the data, making sure the street names are in a readable orientation.
ggplot(aes(x = Start.Station), data = trips_over_24hrs) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle('Count of Rentals lasting over 24 Hours from Different Stations') +
  labs(x = 'Start Stations', y = 'Number of Trips')

### Looks like Franklin & S Washington St was indeed the most popular station when we consider the subset of start stations from which trips of over 24 hours started.  It has the highest number of trips at 3.  Every other station shown here has a count of 1 trip, except for 8th & H St NW at 2 trips.  It appears that rentals lasting over 24 hours don't happen very often at any given location.