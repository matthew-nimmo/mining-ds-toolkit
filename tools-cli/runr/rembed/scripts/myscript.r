library(mypkg)

data_dir <- Sys.getenv("APP_DATA_DIR")
model_dir <- Sys.getenv("APP_MODEL_DIR")

print(paste("Data directory:", data_dir))
print(paste("Model directory:", model_dir))

mypkg::run_model(data_dir, model_dir)
