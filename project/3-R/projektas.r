
library(tidyr)
library(dplyr)
library(h2o)
library(tidyverse)
h2o.init(max_mem_size = "12G")

data1 <- read_csv("C:/Users/Vartotojas/Desktop/Projektas/1-sample_data.csv")

select(data1, y)

data2 <- read_csv("C:/Users/Vartotojas/Desktop/Projektas/2-additional_data.csv")

data3 <- read_csv("C:/Users/Vartotojas/Desktop/Projektas/3-additional_features.csv")

data4 <- rbind(data1, data2)

joined_data <- inner_join(data4, data3, by = "id")


data_full <- data4 %>%
inner_join(data3, by = "id")

write_csv(data_full, "C:/Users/Vartotojas/Desktop/Projektas/train_data.csv") 

df <- h2o.importFile("C:/Users/Vartotojas/Desktop/Projektas/train_data.csv")
df
class(df)
summary(df)

y <- "y"
x <- setdiff(names(df), c(y, "id"))
df$y <- as.factor(df$y)
summary(df)

#library(httr)    
#set_config(use_proxy(url="10.3.100.207",port=8080))

splits <- h2o.splitFrame(df, c(0.6,0.2), seed=123)
train  <- h2o.assign(splits[[1]], "train") # 60%
valid  <- h2o.assign(splits[[2]], "valid") # 20%
test   <- h2o.assign(splits[[3]], "test")  # 20%


### GBM AUC 0,84

gbm_params1 <- list(learn_rate = c(0.01, 0.1),
                    max_depth = c(12,14,16),
                    sample_rate = c(0.8, 1.0),
                    col_sample_rate = c(1.0, 1.2, 1.5))

gbm_grid1 <- h2o.grid("gbm", 
                      x = x, 
                      y = y,
                      grid_id = "gbm_grid1",
                      training_frame = train,
                      validation_frame = valid,
                      ntrees = 100,
                      seed = 1,
                      hyper_params = gbm_params1)

# Get the grid results, sorted by validation AUC

gbm_gridperf1 <- h2o.getGrid(grid_id = "gbm_grid1",
                             sort_by = "auc",
                             decreasing = TRUE)

print(gbm_gridperf1)

model_gbm <- h2o.getModel("gbm_grid1_model_3")

h2o.performance(model_gbm, newdata = test)

perf <- h2o.performance(model_gbm, train = TRUE)
perf
perf_valid <- h2o.performance(model_gbm, valid = TRUE)
perf_valid
perf_test <- h2o.performance(model_gbm, newdata = test)
perf_test

h2o.auc(perf)
plot(perf_valid, type = "roc")

test_data <- h2o.importFile("C:/Users/Vartotojas/Desktop/Projektas/test_data.csv")
h2o.performance(model_gbm, newdata = test_data)

predictions <- h2o.predict(model_gbm, test_data)

predictions %>%
  as_tibble() %>%
  mutate(id = row_number(), y = p0) %>%
  select(id, y) %>%
  write_csv("C:/Users/Vartotojas/Desktop/Projektas/predictions5.csv")

### ID, Y

h2o.saveModel(model_gbm, "C:/Users/Vartotojas/Desktop/Projektas/8-model", filename = "my_model")

model <- h2o.loadModel("C:/Users/Vartotojas/Desktop/Projektas/8-model/my_model")
h2o.varimp_plot(model)


### GBM AUC 0,86
h2o.shutdown()

h2o.init()

gbm_params1 <- list(learn_rate = c(0.01, 0.1),
                    max_depth = c(16, 18),
                    sample_rate = c(0.8, 1.0),
                    col_sample_rate = c(0.8, 1.0))

gbm_grid1 <- h2o.grid("gbm", 
                      x = x, 
                      y = y,
                      grid_id = "gbm_grid1",
                      training_frame = train,
                      validation_frame = valid,
                      ntrees = 100,
                      seed = 1,
                      hyper_params = gbm_params1)

# Get the grid results, sorted by validation AUC
gbm_gridperf1 <- h2o.getGrid(grid_id = "gbm_grid1",
                             sort_by = "auc",
                             decreasing = TRUE)

print(gbm_gridperf1)

model_gbm1 <- h2o.getModel("gbm_grid1_model_40")

h2o.performance(model_gbm1, newdata = test)

perf1 <- h2o.performance(model_gbm1, train = TRUE)
perf1
perf_valid1 <- h2o.performance(model_gbm1, valid = TRUE)
perf_valid1
perf_test1 <- h2o.performance(model_gbm1, newdata = test)
perf_test1

h2o.auc(perf1)
plot(perf_valid1, type = "roc")

test_data <- h2o.importFile("C:/Users/Vartotojas/Desktop/Projektas/test_data.csv")
h2o.performance(model_gbm1, newdata = test_data)

predictions1 <- h2o.predict(model_gbm1, test_data)

predictions1 %>%
  as_tibble() %>%
  mutate(id = row_number(), y = p0) %>%
  select(id, y) %>%
  write_csv("C:/Users/Vartotojas/Desktop/Projektas/predictions2.csv")

### ID, Y

h2o.saveModel(model_gbm1, "C:/Users/Vartotojas/Desktop/Projektas/5-model", filename = "my_model1")

model1 <- h2o.loadModel("C:/Users/Vartotojas/Desktop/Projektas/5-model/my_model1")
h2o.varimp_plot(model1)




### GBM AUC 0,868
h2o.shutdown()

h2o.init()

gbm_params1 <- list(learn_rate = 0.1,
                    max_depth = 20,
                    sample_rate = c(0.8, 1.0),
                    col_sample_rate = 1.0)

gbm_grid1 <- h2o.grid("gbm", 
                      x = x, 
                      y = y,
                      grid_id = "gbm_grid1",
                      training_frame = train,
                      validation_frame = valid,
                      ntrees = 150,
                      seed = 1,
                      hyper_params = gbm_params1)

# Get the grid results, sorted by validation AUC
gbm_gridperf1 <- h2o.getGrid(grid_id = "gbm_grid1",
                             sort_by = "auc",
                             decreasing = TRUE)

print(gbm_gridperf1)

model_gbm1 <- h2o.getModel("gbm_grid1_model_2")

h2o.performance(model_gbm1, newdata = test)

perf1 <- h2o.performance(model_gbm1, train = TRUE)
perf1
perf_valid1 <- h2o.performance(model_gbm1, valid = TRUE)
perf_valid1
perf_test1 <- h2o.performance(model_gbm1, newdata = test)
perf_test1

h2o.auc(perf1)
plot(perf_valid1, type = "roc")

test_data <- h2o.importFile("C:/Users/Vartotojas/Desktop/Projektas/test_data.csv")
h2o.performance(model_gbm1, newdata = test_data)

predictions1 <- h2o.predict(model_gbm1, test_data)

predictions1 %>%
  as_tibble() %>%
  mutate(id = row_number(), y = p0) %>%
  select(id, y) %>%
  write_csv("C:/Users/Vartotojas/Desktop/Projektas/predictions3.csv")

### ID, Y

h2o.saveModel(model_gbm1, "C:/Users/Vartotojas/Desktop/Projektas/6-model", filename = "my_model2")

model1 <- h2o.loadModel("C:/Users/Vartotojas/Desktop/Projektas/6-model/my_model2")
h2o.varimp_plot(model1)



### GBM AUC 0,871
h2o.shutdown()

h2o.init()

gbm_params1 <- list(learn_rate = 0.1,
                    max_depth = 26,
                    sample_rate = c(0.8, 1.0),
                    col_sample_rate = 1.0)

gbm_grid1 <- h2o.grid("gbm", 
                      x = x, 
                      y = y,
                      grid_id = "gbm_grid1",
                      training_frame = train,
                      validation_frame = valid,
                      ntrees = 200,
                      seed = 1,
                      hyper_params = gbm_params1)

# Get the grid results, sorted by validation AUC
gbm_gridperf1 <- h2o.getGrid(grid_id = "gbm_grid1",
                             sort_by = "auc",
                             decreasing = TRUE)

print(gbm_gridperf1)

model_gbm1 <- h2o.getModel("gbm_grid1_model_3")

h2o.performance(model_gbm1, newdata = test)

perf1 <- h2o.performance(model_gbm1, train = TRUE)
perf1
perf_valid1 <- h2o.performance(model_gbm1, valid = TRUE)
perf_valid1
perf_test1 <- h2o.performance(model_gbm1, newdata = test)
perf_test1

h2o.auc(perf1)
plot(perf_valid1, type = "roc")

test_data <- h2o.importFile("C:/Users/Vartotojas/Desktop/Projektas/test_data.csv")
h2o.performance(model_gbm1, newdata = test_data)

predictions1 <- h2o.predict(model_gbm1, test_data)

predictions1 %>%
  as_tibble() %>%
  mutate(id = row_number(), y = p0) %>%
  select(id, y) %>%
  write_csv("C:/Users/Vartotojas/Desktop/Projektas/predictions4.csv")

### ID, Y

h2o.saveModel(model_gbm1, "C:/Users/Vartotojas/Desktop/Projektas/7-model", filename = "my_model87")

model1 <- h2o.loadModel("C:/Users/Vartotojas/Desktop/Projektas/7-model/my_model87")
h2o.varimp_plot(model1)



### GBM AUC 0,XX
h2o.shutdown()

h2o.init()

gbm_params1 <- list(learn_rate = 0.1,
                    max_depth = 40,
                    sample_rate = c(0.8, 1.0),
                    col_sample_rate = c(0.8, 1.0))

gbm_grid1 <- h2o.grid("gbm", 
                      x = x, 
                      y = y,
                      grid_id = "gbm_grid1",
                      training_frame = train,
                      validation_frame = valid,
                      ntrees = 200,
                      seed = 1,
                      hyper_params = gbm_params1)

# Get the grid results, sorted by validation AUC
gbm_gridperf1 <- h2o.getGrid(grid_id = "gbm_grid1",
                             sort_by = "auc",
                             decreasing = TRUE)

print(gbm_gridperf1)

model_gbm1 <- h2o.getModel("gbm_grid1_model_3")

h2o.performance(model_gbm1, newdata = test)

perf1 <- h2o.performance(model_gbm1, train = TRUE)
perf1
perf_valid1 <- h2o.performance(model_gbm1, valid = TRUE)
perf_valid1
perf_test1 <- h2o.performance(model_gbm1, newdata = test)
perf_test1

h2o.auc(perf1)
plot(perf_valid1, type = "roc")

test_data <- h2o.importFile("C:/Users/Vartotojas/Desktop/Projektas/test_data.csv")
h2o.performance(model_gbm1, newdata = test_data)

predictions1 <- h2o.predict(model_gbm1, test_data)

predictions1 %>%
  as_tibble() %>%
  mutate(id = row_number(), y = p0) %>%
  select(id, y) %>%
  write_csv("C:/Users/Vartotojas/Desktop/Projektas/predictions4.csv")

### ID, Y

h2o.saveModel(model_gbm1, "C:/Users/Vartotojas/Desktop/Projektas/7-model", filename = "my_model87")

model1 <- h2o.loadModel("C:/Users/Vartotojas/Desktop/Projektas/7-model/my_model87")
h2o.varimp_plot(model1)
