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




#------------------- Compute plot layout
pncols = sapply(dat_list[2:4], ncol)

lo = rep(2:4, times = pncols)
lo = matrix(c(rep(1, length(lo)), lo), ncol = 1)
layout(lo)

#------------------- Specify margins
left_mar = 6.25
right_mar = 0.5
par(mar = c(2, left_mar, 1.5, right_mar))

#----------------------------------- 1.Panel 1

# Grab panel 1 data and color key
sel = "panel_1"
# data
p1_dat = dat_list[[sel]]
class(p1_dat)
mode(p1_dat)
head(p1_dat)
# color
p1_col = color_list[[sel]]
class(p1_col)
head(p1_col)

barplot_ylim = c(-100, 100)
barplot_main = "Panel 1: Put the title here (A simple barplot)"
barplot_xlab = "SLD % change from Baseline"

barplot_names = names(p1_dat)
barplot_col = p1_col

bp = barplot(p1_dat, main = barplot_main, ylim = barplot_ylim, col = barplot_col,
             xlab = "", ylab = "", axes = FALSE, border = "white", xaxs = "i",
             names.arg = FALSE)
  
abline(h = 0)
abline(h = c(-30, 20), lty = 3)
  
at = seq(-100, 100, 20)
axis(side = 2, at = at, labels = at, pos = -0.5, las = 2, cex.axis = 1)
mtext(barplot_xlab, side = 2, line = 3.5, cex = 0.75)

mtext(barplot_names, side = 1, at = bp[,1], cex = 0.6, las = 3, line = 0.2)

ucol = unique(barplot_col)
legend("topleft", pch = 19, col = ucol, legend = c("CR", "PR", "SD", "PD"),
       title = "Best Response", horiz = TRUE, cex = 0.8, pt.cex = 1.25)

#--------------------------------- 2. Panel 2
# Grab panel 2 data and color key
sel = "panel_2"
# data
p2_dat = dat_list[[sel]]
class(p2_dat)
mode(p2_dat)
head(p2_dat)
# color
p2_col = color_list[[sel]]
class(p2_col)
lapply(p2_col, head)

top_mar = 0
bottom_mar = 1.25
par(mar = c(bottom_mar, left_mar, top_mar, right_mar))

title = "Panel 2: This is a factor data.frame! (Each row has different levels)"
plot_image(p2_dat, p2_col, title = title)


#--------------------------------- 3. Panel 3
# Grab panel 3 data and color key
sel = "panel_3"
# data
p3_dat = dat_list[[sel]]
class(p3_dat)
mode(p3_dat)
head(p3_dat)
# color
p3_col = color_list[[sel]]
class(p3_col)
head(p3_col)

par(mar = c(bottom_mar, left_mar, top_mar, right_mar))

title = "Panel 3: This is a character matrix (The levels apply to the entire matrix)"
plot_image(dat = p3_dat, col_key = p3_col, title = title)

#------------------------------- 4. Panel 4
# Grab panel 4 data and color key
sel = "panel_4"
# data
p4_dat = dat_list[[sel]]
class(p4_dat)
mode(p4_dat)
head(p4_dat)
# color
p4_col = color_list[[sel]]
class(p4_col)

par(mar = c(bottom_mar, left_mar, top_mar, right_mar))

title = "Panel 4: This is a numerix matrix! (A simple heatmap)"
plot_image(dat = p4_dat, p4_col(100), title = title)

