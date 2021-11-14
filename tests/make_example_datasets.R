setwd("/Users/kokrah/Packages/cbiomPlots/")

# 1. Make a random numeric vector
set.seed(10)
n = 99
rnam = sprintf("%03d", 1:n)

x = rbeta(n, 2, 5)
num_vec = x * 200 / (max(x) - min(x)) - 100
num_vec[num_vec > 75] = -100.00
num_vec[abs(num_vec) < 5] = 10.00
num_vec = round(sort(num_vec), 2)
names(num_vec) = rnam

# Make color key
num_vec_col_key = ifelse(num_vec <= -100, "dodgerblue4",
                         ifelse(num_vec < -30, "dodgerblue",
                                ifelse(num_vec < 20, "lightsalmon", "firebrick3")))
names(num_vec_col_key) = names(num_vec)

# 2. Make a numeric annotation matrix
y = 3 * sin(x * 15)
y = ifelse(x < 0.10, 3,
          ifelse(x > 0.55, 3, y))
z = - 2.5 * scale(y ** 2, TRUE, TRUE)

annot_num_mat = cbind(replicate(3, rnorm(n, y, 1)),
                      replicate(3, rnorm(n, z, 1)),
                      replicate(3, 2 * rnorm(n)))

annot_num_mat = round(annot_num_mat, 3)

hm = heatmap(annot_num_mat)
annot_num_mat = annot_num_mat[hm$rowInd, hm$colInd]

rownames(annot_num_mat) = rnam
colnames(annot_num_mat) = paste0("NUMERIC-VAR", sprintf("%01d", 1:ncol(annot_num_mat)))
head(annot_num_mat)

# Make color key (function)
num_mat_col_fct = colorRampPalette(c("blue3", "blue", "white", "red", "red3"))

# 3. Make a categorical annotation matrix
annot_cat_mat = ifelse(annot_num_mat > 2, "MUTATION A",
                       ifelse(annot_num_mat > 1, "MUTATION B",
                              ifelse(annot_num_mat > 0, "MUTATION C",
                                     ifelse(annot_num_mat < -2, "MUTATION D", "MUTATION E"))))

for (i in 1:6) annot_cat_mat[,i] = sort(annot_cat_mat[,i])
for (i in 7:9) annot_cat_mat[,i] = rev(sort(annot_cat_mat[,i]))

colnames(annot_cat_mat) = paste0("GENE-SYMBL", sprintf("%01d", 1:ncol(annot_cat_mat)))
head(annot_cat_mat)

# Make color key
cat_mat_col_key = c("MUTATION A" = "darkorchid4",
                    "MUTATION B" = "darkseagreen",
                    "MUTATION C" = "burlywood1",
                    "MUTATION D" = "thistle3",
                    "MUTATION E" = "firebrick")

# 4. Make a data.frame annotation matrix
annot_cat_df = data.frame("FACTOR-VAR1" = sample(c("LEVEL A1", "LEVEL A2", "LEVEL A3", "LEVEL A4", "LEVEL A5"), n, TRUE),
                          "FACTOR-VAR2" = sample(c("LEVEL B1", "LEVEL B2", "LEVEL B3", "LEVEL B4", "LEVEL B5"), n, TRUE),
                          "FACTOR-VAR3" = sample(c("LEVEL C1", "LEVEL C2", "LEVEL C3", "LEVEL C4", "LEVEL C5"), n, TRUE),
                          "FACTOR-VAR4" = sample(c("LEVEL D1", "LEVEL D2", "LEVEL D3", "LEVEL D4", "LEVEL D5"), n, TRUE),
                          "FACTOR-VAR5" = sample(c("LEVEL E1", "LEVEL E2", "LEVEL E3", "LEVEL E4", "LEVEL E5"), n, TRUE),
                          check.names = FALSE)

for (i in 1:ncol(annot_cat_df)) annot_cat_df[,i] = sort(annot_cat_df[,i])
rownames(annot_cat_df) = rnam
head(annot_cat_df)

# Make color key (list)
cat_df_col_key = list("FACTOR-VAR1" = c("LEVEL A1" = "mediumturquoise",
                                        "LEVEL A2" = "lightgoldenrod1",
                                        "LEVEL A3" = "plum",
                                        "LEVEL A4" = "sienna1",
                                        "LEVEL A5" = "skyblue"),
                      "FACTOR-VAR2" = c("LEVEL B1" = "steelblue",
                                        "LEVEL B2" = "darkblue",
                                        "LEVEL B3" = "limegreen",
                                        "LEVEL B4" = "tomato",
                                        "LEVEL B5" = "gold3"),
                      "FACTOR-VAR3" = c("LEVEL C1" = "gray80",
                                        "LEVEL C2" = "gray60",
                                        "LEVEL C3" = "gray40",
                                        "LEVEL C4" = "gray20",
                                        "LEVEL C5" = "black"),
                      "FACTOR-VAR4" = c("LEVEL D1" = "red",
                                        "LEVEL D2" = "orange",
                                        "LEVEL D3" = "green",
                                        "LEVEL D4" = "blue",
                                        "LEVEL D5" = "violet"),
                      "FACTOR-VAR5" = c("LEVEL E1" = "royalblue",
                                        "LEVEL E2" = "purple4",
                                        "LEVEL E3" = "brown",
                                        "LEVEL E4" = "khaki",
                                        "LEVEL E5" = "pink"))

# 5. Save results
example_data = list() # a list ot hold the results

example_data[["panel_1"]] = list("numeric_vector" = num_vec,
                                 "color_key" = num_vec_col_key)
example_data[["panel_2"]] = list("factor_dataframe" = annot_cat_df,
                                 "color_key" = cat_df_col_key)
example_data[["panel_3"]] = list("charater_matrix" = annot_cat_mat,
                                 "color_key" = cat_mat_col_key)
example_data[["panel_4"]] = list("numeric_matrix" = annot_num_mat,
                                 "color_key" = num_mat_col_fct)

save(example_data, file = "inst/extdata/example_data.RData")
