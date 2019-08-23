2+2
5+5
c(3,5)
x <- 2+3
y <- 1:100
inf_data_file <- "/home/nwknoblauch/Public/r-novice-inflammation/data/inflammation-01.csv"
read.csv(inf_data_file,header = FALSE)
inf_data <- read.csv(inf_data_file,header = FALSE)
inf_data
car_data_file <- "/home/nwknoblauch/Public/r-novice-inflammation/data/car-speeds.csv"
car_data <- read.csv(car_data_file,header = TRUE)
#take a peek at the file
head(car_data)
car_color <- car_data$Color
car_speed <- car_data$Speed


max(car_speed)
min(car_speed)
mean(car_speed)

day_3 <- inf_data$V3
day_5 <- inf_data$V5

median(inf_data$V7)

day_7 <- inf_data$V7
median(day_7)

days <- c(3,5,7)
pts <- 1:10
inf_days <- inf_data[pts,days]
inf_days_all_pts <- inf_data[,days]


max(car_data$Speed) - min(car_data$Speed)
max_speed <- max(car_data$Speed)
car_data[car_data$Speed == max_speed,]

car_data$Color[car_data$Speed == max_speed]

#assigning values to variables

x <- 2
# data <- read.csv(...)
# y <- 2
# x <- y
#

# subsetting data and indexing by position

day_3 <- inf_data$V3
day_3[1:3]  #first 3 patients
length(day_3)
num_pt <- length(day_3) # equals 60
day_3[num_pt] #wil give us the 60th patient
# to get the last 3 patients
day_3[58:60]
#or 
day_3[c(58,59,60)]
#or
length(day_3)-2
length (day_3)
begin <- length(day_3)-2
day_3[begin:num_pt]

#for last 5 pts
begin <- length(day_3)-4
day_3[begin:num_pt]

#indexing by name

car_color_speed <- car_data[,c("Color","Speed")]
inf_data[,c("V1","V3")]

#logical indexing aka boolean indexing aka TRUE/FALSE indexing
#using == operator

22==11*2 #TRUE
day_3 == 1
day_3_ones <- day_3[day_3 == 1]
length(day_3_ones)
day_3_no_ones <- day_3[day_3 !=1]
length (day_3_no_ones)
max(day_3_ones)
min(day_3_ones)
min(day_3_no_ones)
max(day_3_no_ones)

is_one <- day_3 == 1
day_5 <- inf_data$V5
day_5_day_3 <- day_5[is_one]
min(day_5_day_3)
max(day_5_day_3)

inf_data[is_one,]

inf_data$V5[inf_data$V5 > 0]
length(inf_data$V5[inf_data$V5 > 0])

pts <- 1:10

pts[pts<0]


car_data

#getting the speeds of the red cars vs the blue cars and seeing which has the higher average speed

red_car_speeds <- car_data$Speed[car_data$Color == "Red"]
blue_car_speeds <- car_data$Speed[car_data$Color == "Blue"]
mean(red_car_speeds)
mean(blue_car_speeds)
mean(blue_car_speeds) - mean(red_car_speeds)

#or

car_speed <- car_data$Speed
car_color <- car_data$Color
car_speed[car_color == "Red"]
mean(car_speed[car_color == "Red"])

#or

car_data[car_data$Color == "Blue", c("Speed")]
mean(car_data[car_data$Color == "Blue", c("Speed")])
mean(car_data[car_data$Color == "Red", c("Speed")])


write.csv(car_data,"./data/car_data.csv",row.names = FALSE)

#writing functions

#kelvin to celsius

fahrenheit_to_Kelvin <-function(temp_F){
  
  temp_K <- ((temp_F - 32)*(5/9)) +273.15
return(temp_K)
}

#fahrenheit to celsius

fahrenheit_to_celsius <- function(temp_F){
  
  temp_K <- fahrenheit_to_Kelvin(temp_F)
  temp_C <- kelvin_to_celsius(temp_K)
  
  return(temp_C)
}

fahrenheit_to_celsius(32)

temp_F <- 32
kelvin_to_celsius(fahrenheit_to_Kelvin(temp_F))

c(4.5)
vec <- c(4,5)
vec2 <- c(vec,6)
vec3 <- c(2,3,"a","b")
vec4 <- c("four",4)


lenght(vec3)
length(vec3)


# writing a new function called edges that takes an arguement called vec and returns a new vector composed of the first and last element of vec

edges <- function(vec){
  
  first <- vec[1]
  last <- vec[length(vec)]
  first_last <- c(first,last)
  
  return(first_last)
  
}

