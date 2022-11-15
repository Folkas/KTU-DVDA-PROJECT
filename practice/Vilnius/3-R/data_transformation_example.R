#install.packages("tidyverse")

library(tidyverse)

data <- read_csv("../../../project/1-data/1-sample_data.csv")

select(data, y)

data %>%
  select(y)

additional_features <- read_csv("../../../project/1-data/3-additional_features.csv")

joined_data <- inner_join(data, additional_features, by = "id")


joined_data <- data %>%
  inner_join(additional_features, by = "id")


write_csv(joined_data, "../../../project/1-data/train_data.csv")  
