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
primary_image()


# 2. error: level in dat but not in col_key_list
dat[3, 3] = "Hello"
dat[5, 3] = "Kwame"
cat_df_to_num(dat, col_key_list)

# 3. no error: adding NAs
dat[3, 3] =  NA
dat[5, 3] =  NA
cat_df_to_num(dat, col_key_list)

# 4. no error: reducing levels in dat but not in col_key_list
dat[, "FACTOR-000D"][dat[, "FACTOR-000D"] %in% "LEVEL D5"] = "LEVEL D1"
cat_df_to_num(dat, col_key_list)

# 5. no error: reducing levels in dat but not in col_key_list
# (with NAs)
dat[, "FACTOR-000C"][dat[, "FACTOR-000C"] %in% "LEVEL C1"] = "LEVEL C5"
cat_df_to_num(dat, col_key_list)

# 6. no error: reducing levels in dat but not in col_key_list
# (one level per column)
dat[, "FACTOR-000A"] = "LEVEL A1"
dat[, "FACTOR-000B"] = "LEVEL B3"
cat_df_to_num(dat, col_key_list)

