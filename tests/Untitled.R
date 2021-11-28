setwd("/Users/kokrah/Packages/cbiomPlots/")
# Load example data
load("inst/extdata/example_data.RData")
# Load functions
for (fct in list.files("R")) {
  source(paste0("R/", fct))
  print(fct)
}

# What is in the example data?
str(example_data, 1)
t(sapply(example_data, names))

dat_list = lapply(example_data, "[[", 1)
color_list = lapply(example_data, "[[", 2)


# Grab panel 1 data and color key
sel = "panel_1"
# data
p1_dat = dat_list[[sel]][1:50]
class(p1_dat)
mode(p1_dat)
head(p1_dat)
# color
p1_col = color_list[[sel]]
class(p1_col)
head(p1_col)


layout(matrix(c(1, 2, 2, 2, 2, 2), nrow = 1))

# Grab factor_dataframe
dat = example_data[["panel_2"]][["factor_dataframe"]][1:50, ]
col_key_list = example_data[["panel_2"]][["color_key"]]

head(dat)
col_key_list

# 1. no error: perfect example
res = cat_df_to_num(dat, col_key_list)
primary_image(t(res$res), col = res$res_col)
draw_border(t(res$res))

x = sort(abs(p1_dat))
barplot(x, horiz = TRUE, border = FALSE, yaxs = "i",
        names.arg = FALSE)

