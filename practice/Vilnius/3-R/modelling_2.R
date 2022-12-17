library(h2o)
library(tidyverse)
h2o.init()

df <- h2o.importFile("../../../project/1-data/train_data.csv")
df
class(df)
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

model <- h2o.getModel("GBM_1_AutoML_1_20221119_120146")
h2o.varimp_plot(model)
h2o.performance(model, train = TRUE)
perf_valid <- h2o.performance(model, valid = TRUE)
h2o.auc(perf_valid)
h2o.performance(model, newdata = test)

h2o.varimp_plot(model)

h2o.auc(perf_valid)
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



### deeplearning

dl_model <- h2o.deeplearning(x,
                             y,
                             training_frame = train,
                             validation_frame = valid,
                             activation = "Tanh",
                             hidden = c(20, 20),
                             epochs = 5)

perf_test_dl <- h2o.performance(dl_model, newdata = test)
perf_test_dl
h2o.varimp(model)


learn_rate=0.1,
max_depth=30,
sample_rate = [0.8, 1.0]
col_sample_rate= [0.8, 1.0]


# gridsearch
# https://docs.h2o.ai/h2o/latest-stable/h2o-docs/grid-search.html

dl_params1 <- list(hidden = list(50, c(50,50), c(50,50,50)))

dl_grid <- h2o.grid("deeplearning",
                    x = x,
                    y = y,
                    training_frame = train,
                    validation_frame = valid,
                    epochs = 3,
                    # parallelism = 3,
                    hyper_params = dl_params1)

h2o.getGrid(grid_id = dl_grid@grid_id,
            sort_by = "auc",
            decreasing = TRUE)

h2o.list_models()
grid_dl <- h2o.getModel(dl_grid@model_ids[[1]])

### GBM

gbm_params1 <- list(max_depth = c(3, 5, 9),
                    sample_rate = c(0.8, 1.0))

gbm_grid1 <- h2o.grid("gbm", 
                      x = x, 
                      y = y,
                      grid_id = "gbm_grid1",
                      training_frame = train,
                      validation_frame = valid,
                      ntrees = 50,
                      seed = 1,
                      hyper_params = gbm_params1)

# Get the grid results, sorted by validation AUC
gbm_gridperf1 <- h2o.getGrid(grid_id = "gbm_grid1",
                             sort_by = "auc",
                             decreasing = TRUE)
print(gbm_gridperf1)

model_gbm <- h2o.getModel("gbm_grid1_model_6")

h2o.performance(model_gbm, newdata = test)

### Random Forest

rf_model <- h2o.randomForest(x = x,
                             y = y,
                             training_frame = train,
                             validation_frame = valid)
rf_model

perf_test_rf <- h2o.performance(rf_model, newdata = test)
perf_test_rf

predictions_rf <- h2o.predict(rf_model, test_data) %>%
  as_tibble()

# h2o.shutdown()


