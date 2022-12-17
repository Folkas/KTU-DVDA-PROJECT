library(h2o)
library(tidyverse)
h2o.init()

# read.csv("/project/1-data/train_data.csv")
train_data_2

df <- h2o.importFile("1.KTU/Didziuju_duomenø_tyrybos_metodai/Daugela/train_data_2.csv")
df
df <- h2o.importFile("../../../project/1-data/train_data.csv")
df
class(df)
summary(df)

class(df)
class(data)
summary(df)

y <- "y"
x <- setdiff(names(df), c(y, "id"))
df$y <- as.factor(df$y)
summary(df)


splits <- h2o.splitFrame(df, c(0.6,0.2), seed=123)
train  <- h2o.assign(splits[[1]], "train") # 60%
valid  <- h2o.assign(splits[[2]], "valid") # 20%
test   <- h2o.assign(splits[[3]], "test")  # 20%

aml <- h2o.automl(x = x,
                  y = y,
                  training_frame = train,
                  validation_frame = valid,
                  max_runtime_secs = 60)

aml@leaderboard

model <- aml@leader

model <- h2o.getModel("GBM_1_AutoML_1_20221111_181631")

perf <- h2o.performance(model, train = TRUE)
perf
perf_valid <- h2o.performance(model, valid = TRUE)
perf_valid
perf_test <- h2o.performance(model, newdata = test)
perf_test

h2o.auc(perf)
plot(perf_valid, type = "roc")

test_data <- h2o.importFile("../../../project/1-data/test_data.csv")
h2o.performance(model, newdata = test_data)

predictions <- h2o.predict(model, test_data)

predictions %>%
  as_tibble() %>%
  mutate(id = row_number(), y = p0) %>%
  select(id, y) %>%
  write_csv("../5-predictions/predictions1.csv")

### ID, Y

h2o.saveModel(model, "../4-model/", filename = "my_model")

model <- h2o.loadModel("../4-model/my_model")
h2o.varimp_plot(model)


### deeplearning

dl_model <- h2o.deeplearning(x,
                             y,
                             training_frame = train,
                             validation_frame = valid,
                             activation = "Tanh",
                             hidden = c(20, 20),
                             epochs=5)

perf_test_dl <- h2o.performance(dl_model, newdata = test)
perf_test_dl


# gridsearch

dl_params1 <- list(hidden = list(10, c(10,10), c(10,10,10)))

dl_grid <- h2o.grid("deeplearning",
                    x = x,
                    y = y,
                    training_frame = train,
                    epochs = 3,
                    hyper_params = dl_params1)

h2o.getGrid(grid_id = dl_grid@grid_id,
            sort_by = "auc",
            decreasing = TRUE)


grid_dl <- h2o.getModel(dl_grid@model_ids[[1]])



# h2o.shutdown()

