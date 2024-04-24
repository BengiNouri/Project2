
library(readr)
cardata <- read_delim("Car_data4.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(Car_data4)


########## Year --> Production year of the car

library(dplyr)
library(stringr)

cardata <- cardata %>%
  mutate(Year = case_when(
    # When the format is four digits, take as is
    str_detect(`Production Year`, "^[0-9]{4}$") ~ str_extract(`Production Year`, "[0-9]{4}"),
    # When the format is two digits
    str_detect(`Production Year`, "[0-9]{2}$") ~ {
      year_part <- str_extract(`Production Year`, "[0-9]{2}")
      # Convert to four digits, assuming '20' for years 00-21 and '19' for years 22-99
      if_else(as.numeric(year_part) <= 30, str_c("20", year_part), str_c("19", year_part))
    },
    # Set to NA if the year is not in the expected format
    TRUE ~ as.character(NA)
  ))

# Convert Year to numeric type
cardata$Year <- as.numeric(cardata$Year)
cardata$`Production Year`= NULL

# View the dataframe to confirm the changes
View(cardata)


library(dplyr)
library(stringr)

######## Seperating motor size and fuel to columns for themselves

cardata <- cardata %>%
  mutate(
    # Extract motor size and fuel type (assuming they are always present before the first numeric value)
    Motor_Size_Fuel_Type = str_extract(`Motor Specs`, "^[0-9]+\\.[0-9]+\\s+\\w+"),
    # Extract horsepower, which is the numeric value before "HK"
    Horsepower = str_extract(`Motor Specs`, "\\d+\\s?(?=HK)")
  ) %>%
  # Convert extracted horsepower to numeric, handling NAs
  mutate(
    Horsepower = ifelse(is.na(Horsepower), NA_real_, as.numeric(Horsepower))
  )


cardata$`Motor Specs`= NULL

# View the dataframe to confirm the changes
View(cardata)

################ Automatgear and manuelt gear cleaning. 

cardata <- cardata %>%
  mutate(Gear = case_when(
    str_detect(Gear, "Automatgear") ~ "Automatgear",
    str_detect(Gear, "Manuelt gear") ~ "Manuelt gear",
    TRUE ~ as.character(NA) # Set to NA if neither pattern matches
  ))

# View the dataframe to confirm the changes
View(cardata)

################# Cleaning the Color for better clarity and understanding.  
library(dplyr)
library(stringdist)

install.packages("stringdist")


# Define a list of known color mappings
known_colors <- c(
  "White" = "white|hv\\w*|pearl|flash|diamond|icy|nacre",
  "Silver" = "silver|aluminium|zircon|platinum|platinium",
  "Black" = "black|sort|midnight|onyx|mica|sapphire|carbon",
  "Grey" = "grey|gray|grå|gråmetal|metalstream|titanium",
  "Red" = "red|rød|rosa|rose|burgundy|flame",
  "Blue" = "blue|blå|cobalt|azure|celestial",
  "Green" = "green|grøn|khaki|olive|lime",
  "Yellow" = "yellow|gul|gold|golden",
  "Brown" = "brown|brun|beige|copper|bronze|champagne",
  "Orange" = "orange|coral",
  "Pink" = "pink|fuchsia",
  "Purple" = "purple|lilac|violet",
  "Maroon" = "maroon|burgundy|merlot|wine|cordovan",
  "Copper" = "copper|bronze|metallic red|cinnamon",
  "Ivory" = "ivory|cream|eggshell|off-white",
  "Magenta" = "magenta|cerise|deep pink",
  "Teal" = "teal|turquoise|aquamarine",
  "Gold" = "gold|golden|metallic yellow",
  "Crimson" = "crimson|ruby|cherry|scarlet",
  "Emerald" = "emerald|jade|forest green",
  "Indigo" = "indigo|navy blue|deep blue",
  "Taupe" = "taupe|mushroom|beige",
  "Charcoal" = "charcoal|dark gray|gunmetal",
  "Lavender" = "lavender|lilac|mauve",
  "Amber" = "amber|tawny|golden orange",
  "Plum" = "plum|violet|purple",
  "Tan" = "tan|khaki|camel|sand",
  "Olive" = "olive|army green|military green",
  "Other" = "other"
)

# Function to find the closest known color using Levenshtein distance
find_closest_color <- function(color_description) {
  distances <- sapply(names(known_colors), function(x) stringdist::stringdist(tolower(color_description), tolower(x), method = "lv"))
  closest_color <- names(known_colors)[which.min(distances)]
  closest_color
}

# Apply the function to each color description
cardata$closest_color <- sapply(cardata$Color, find_closest_color)




