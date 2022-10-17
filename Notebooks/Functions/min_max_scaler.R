## 

min_max_scale = function(x){
  
  # This function recive a vector and scale it by min-max scaler
  
  # x = numeric vector
  
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

}