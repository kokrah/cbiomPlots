setwd("/Users/kokrah/Packages/cbiomPlots/")

# Load example data
load("inst/extdata/example_data.RData")
# Load functions
for (fct in list.files("R")) {
  source(paste0("R/", fct))
  print(fct)
}

# Grab factor_dataframe
dat = example_data[["panel_2"]][["factor_dataframe"]]
col_key_list = example_data[["panel_2"]][["color_key"]]

head(dat)
col_key_list

# 1. no error: perfect example
res = cat_df_to_num(dat, col_key_list)
primary_image(res$res, col = res$res_col)

dat[, "FACTOR-VAR1"] = "LEVEL A5"
dat[, "FACTOR-VAR3"] = "LEVEL C3"
dat[, "FACTOR-VAR4"] = "LEVEL D2"
dat[, "FACTOR-VAR5"] = "LEVEL E2"
dat[, "FACTOR-VAR2"] = "LEVEL B4"
res = cat_df_to_num(dat, col_key_list)
primary_image(res$res, col = res$res_col)



# 2. error: level in dat but not in col_key_list
dat[3, 3] = "Hello"
dat[5, 3] = "Kwame"
cat_df_to_num(dat, col_key_list)

# 3. no error: adding NAs
dat[3, 3] =  NA
dat[5, 3] =  NA
res = cat_df_to_num(dat, col_key_list)
primary_image(res$res, col = res$res_col)

# 4. no error: reducing levels in dat but not in col_key_list
dat[, "FACTOR-VAR4"][dat[, "FACTOR-VAR4"] %in% "LEVEL D5"] = "LEVEL D1"
res = cat_df_to_num(dat, col_key_list)
primary_image(res$res, col = res$res_col)

# 5. no error: reducing levels in dat but not in col_key_list
# (with NAs)
dat[, "FACTOR-VAR3"][dat[, "FACTOR-VAR3"] %in% "LEVEL C1"] = "LEVEL C5"
res = cat_df_to_num(dat, col_key_list)
primary_image(res$res, col = res$res_col)

# 6. no error: reducing levels in dat but not in col_key_list
# (one level per column)
dat[, "FACTOR-VAR1"] = "LEVEL A3"
dat[, "FACTOR-VAR5"] = "LEVEL E1"
res = cat_df_to_num(dat, col_key_list)
primary_image(res$res, col = res$res_col)
