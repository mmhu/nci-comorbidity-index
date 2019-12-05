library(Matrix)

tidy <- function(x, code) {
  ### Upper case all codes
  x[[code]] <- toupper(x[[code]])
  
  ### Remove non-alphanumeric characters
  x[[code]] <- gsub(pattern = "[^[:alnum:]]", x = x[[code]], replacement = "")
  
  ### Return x
  return(x)
}

comorbidity_new <- function(x, icd = "icd10", tidy.codes = TRUE) {
  if(tidy.codes){
    x <-tidy(x = x, code = "code")
  }
  regex <- lofregex[["charlson"]][[icd]]
  
  ### Subset only 'id' and 'code' columns
  if (data.table::is.data.table(x)) {
    x <- x[, c("id", "code"), with = FALSE]
  } else {
    x <- x[, c("id", "code")]
  }
  
  ### Turn x into a DT
  data.table::setDT(x)
  
  loc <- sapply(regex, grep, unique(x[["code"]]), value = TRUE)
  loc <- utils::stack(loc)
  names(loc)[1] <- "code"
  
  ### Merge list with original data.table (data.frame)
  x <- merge(x, loc, all.x = TRUE, allow.cartesian = TRUE)
  x[["code"]] <- NULL
  x <- unique(x)
  
  ### Spread wide
  xin <- x[, c("id", "ind"), with = FALSE]
  xin[, value := 1L]
  x <- data.table::dcast.data.table(xin, stats::as.formula(paste("id", "~ ind")), fill = 0)
  x[["NA"]] <- NULL
  
  ### Add missing columns
  for (col in names(regex)) {
    if (is.null(x[[col]])) x[[col]] <- 0
  }
  data.table::setcolorder(x, c("id", names(regex)))
  
  ### Turn internal DT into a DF
  data.table::setDF(x)
  
  x_mat = Matrix(as.matrix(x[,2:ncol(x)]),sparse = T)
  weight_vec = as.matrix(weights$weight)
  x$wscore = as.vector(x_mat%*%weight_vec)
  
  return(x)
}

## For real data
load("/Users/naichen/final_project/sysdata.rda")
example<- read.csv("/Users/naichen/final_project/example-data.csv")
weights<-read.csv("/Users/naichen/final_project/weights.csv")
example <- example[order(example$id), ]

# compare the speed
system.time(cmb_ours <- comorbidity_new(example, icd = "icd10", tidy.codes = TRUE))
library(comorbidity)
system.time(cmb<-comorbidity(x = example, id = "id", code = "code", score = "charlson", icd = "icd10", assign0 = TRUE))


## For simulated data
# simu_data <- data.frame(
#   id = sample(1:100000, size = 2000000, replace = TRUE),
#   code = sample_diag(n = 2000000, version = "ICD10_2011"),
#   stringsAsFactors = FALSE
# )

##compare the speed
simu_data<- read.csv("/Users/naichen/final_project/simulated-data.csv")
simu_data <- simu_data[order(simu_data$id), ]
system.time(simu_cmb_ours <- comorbidity_new(simu_data, icd = "icd10", tidy.codes = TRUE))
system.time(simu_cmb<- comorbidity(x =simu_data, id = "id", code = "code", score = "charlson", icd = "icd10", assign0 = TRUE))