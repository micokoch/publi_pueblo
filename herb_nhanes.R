# Check out Herb's RNHANES package
# https://cran.r-project.org/web/packages/RNHANES/vignettes/introduction.html

library(RNHANES)
library(survey)

files <- nhanes_data_files() # Doesn't work :(
variables <- nhanes_variables()
nhanes_search(files, "environmental phenols")
