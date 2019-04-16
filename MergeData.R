#read ZNO
df <- read.csv("/home/administrator/workplace/univ/OpenData2016.csv", header = TRUE, 
               sep=";", dec=",", fileEncoding="CP1251", stringsAsFactors=FALSE)
dim(df)

#choose neccessary names
df <- df[, c("Regname", "UkrBall100", "MathBall100")]

#delete null rows
df <- df[-(df[, "MathBall100"] == "null"), ]
df <- df[-(df[, "UkrBall100"] == "null"), ]

dim(df)
head(df, 2)

#rename columns 
colnames(df) <- c("HomeRegion", "Ukr", "Math")

#save results
write.csv(df, file = "/home/administrator/workplace/univ/zno_project/data/ZNO/2016results.csv",
          fileEncoding="CP1251", row.names = FALSE)

df[(df[, "MathBall100"] == "null"), "MathBall100"] <- "0,0"
df[(df[, "UkrBall100"] == "null"), "UkrBall100"] <- "0,0"


write.csv(df, file = "/home/administrator/workplace/univ/zno_project/data/ZNO/2016results_withNULL.csv",
          fileEncoding="CP1251", row.names = FALSE)


df <- read.csv("/home/administrator/workplace/univ/zno_project/data/ZNO/2016results.csv", 
               stringsAsFactors = FALSE)
head(df)
unique((df$HomeRegion))
dim(df)
remove(df)

#read Mother-tongue dataset 
mt <- read.csv("/home/administrator/workplace/univ/zno_project/data/mother_tongue/19A050501_02.csv",
               skip = 5, sep=",", fileEncoding="CP1251", stringsAsFactors=FALSE)
rbind(head(mt, 22), tail(mt, 10))

mt <- mt[-((nrow(mt) - 6) : nrow(mt)), ]
mt <- mt[-(1:20), ]
rbind(head(mt, 20), tail(mt, 21))

#delete Crimea
mt <- mt[-(1:21), ]
mt <- mt[-((nrow(mt) - 19):nrow(mt)), ]
rbind(head(mt, 20), tail(mt, 21))

#convinient rows numeration
rownames(mt) <- seq(1, nrow(mt), 1)
cbind(head(mt, 5), tail(mt, 5))

#convenient columns names
colnames(mt) <- c("HomeRegion", "language", "value")
head(mt)

#assign 0 value to not observed language 
mt[mt$value == "-" | mt$value == "", "value"] <- 0
rbind(head(mt, 41), tail(mt, 21))

# convert value to numeric
mt$value <- as.numeric(mt$value)


#duplicate region in HomeRegion properly and convert absolute language value to relative
hregion <- ""
total <- 0

for (i in 1:nrow(mt)) {
  if (mt[i, "HomeRegion"] != "" & mt[i, "HomeRegion"] != " ") {
    hregion <- mt[i, "HomeRegion"]
  }
  else {
    if (mt[i, "language"] == "Всього") {
      print("Change total")
      print(mt[i, ])
      total <- mt[i, "value"]
    }
    else {
      # print(mt[i, "value"])
      mt[i, "value"] <- mt[i, "value"] * 100 / total
    }
    mt[i, "HomeRegion"] <- hregion
  }
}
rbind(head(mt, 41), tail(mt, 21))

# delete rows with "Всього" and empty rows
mt <- mt[(mt$language != "Всього"), ] 
mt <- mt[((mt$language != "") & 
            (mt$language != " ") & 
            (mt$value != "") & (mt$language != " ")), ]
rbind(head(mt, 41), tail(mt, 21))



install.packages('reshape')
install.packages("plotly")
library(reshape)
library(plotly)

pivot_mt <- cast(mt, HomeRegion ~ language)
remove(mt)

write.csv(pivot_mt, 
          "/home/administrator/workplace/univ/zno_project/data/mother_tongue/pivot_mt.csv",
          row.names = FALSE)
head(pivot_mt)

n_col <- ncol(pivot_mt)
n_row <- nrow(pivot_mt)

summary(pivot_mt)

colors_ <- c("aliceblue", "antiquewhite", "aqua", "aquamarine", "azure",
             "beige", "bisque", "black", "blanchedalmond", "blue",
             "blueviolet", "brown", "burlywood", "cadetblue",
             "chartreuse", "chocolate", "coral", "cornflowerblue",
             "cornsilk", "crimson", "cyan", "darkblue", "darkcyan",
             "darkgoldenrod", "darkgray", "darkgrey", "darkgreen")

all_plots <- list()
for (i in 1:n_row) {
  all_plots[[i]] <- plot_ly(pivot_mt, 
                            x = colnames(pivot_mt)[-1], y = as.numeric(pivot_mt[i, 2:n_col]), 
                            type = "bar", name = pivot_mt[i, 1], marker = list(color = colors_[1:(n_col-1)]))
}

plotly::subplot(all_plots, nrows = 4, shareX = TRUE) %>% 
  layout(title="Distributions according to regions")


all_plots <- list()
for (i in 2:n_col) {
  all_plots[[i-1]] <- plot_ly(x = pivot_mt[, 1], y = pivot_mt[, i], 
                            type="bar", name = colnames(pivot_mt)[i])
}

plotly::subplot(all_plots, nrows = 4, shareX = TRUE) %>% 
  layout(title="Distributions of every language")


grouped_mt <- cbind(pivot_mt[, 1], pivot_mt[, c(19, 15, 11, 2)],
                    rowSums(pivot_mt[, -c(1, 19, 15, 11, 2)]))


rowSums(grouped_mt[, -1])

colnames(grouped_mt) <- c("HomeRegion", "ukr", "rus", "mold", "bilorus", "other")

write.csv(grouped_mt, 
          "/home/administrator/workplace/univ/zno_project/data/mother_tongue/grouped_mt.csv",
          row.names = FALSE)


# -------------------------------------------------------------
# make village-city dataset
# -------------------------------------------------------------

"%&%" <- function(x, y){paste(x, y, sep = "")}

make_dataset <- function(path_to_data, col_name_to_use){
  df <- read.csv(path_to_data,
                 skip = 5, sep = "\t", fileEncoding = "CP1251", stringsAsFactors = FALSE)
  df[, 1] <- NULL
  df <- df[-(28:nrow(df)), ]
  colnames(df) <- c("region", col_name_to_use)
  df <- df[-c(1, nrow(df)), ] 
  rownames(df) <- 1:nrow(df)
  if (df[df[, "region"] == "м. Київ", col_name_to_use] == "x") {
    df[df[, "region"] == "м. Київ", col_name_to_use] = 0
    }
  return(df)
}


make_vc_dataset <- function(path_to_village, path_to_city){
  city <- make_dataset(path_to_city, "city")
  village <- make_dataset(path_to_village, "village")
  vc <- merge(city, village, by = "region")
  return(vc)
}


vc2016 <- make_vc_dataset(path_to_village = "/home/administrator/workplace/univ/zno_project/data/admin_ter/2016_village.csv",
                          path_to_city = "/home/administrator/workplace/univ/zno_project/data/admin_ter/2016_city.csv")

write.csv(vc2016, "/home/administrator/workplace/univ/zno_project/data/admin_ter/2016village_city.csv",
          row.names = FALSE)

for (year in c("2016", "2017", "2018")){
  vc <- make_vc_dataset(path_to_village = "/home/administrator/workplace/univ/zno_project/data/admin_ter/" %&% year %&% "_village.csv",
                        path_to_city = "/home/administrator/workplace/univ/zno_project/data/admin_ter/" %&% year %&% "_city.csv")
  
  write.csv(vc, "/home/administrator/workplace/univ/zno_project/data/admin_ter/"%&% year %&%"village_city.csv",
            fileEncoding = "CP1251", row.names = FALSE)
}

vc2016 <- read.csv("/home/administrator/workplace/univ/zno_project/data/admin_ter/2016village_city.csv",
                   fileEncoding = "CP1251")

vc2017 <- read.csv("/home/administrator/workplace/univ/zno_project/data/admin_ter/2017village_city.csv",
                   fileEncoding = "CP1251")

vc2018 <- read.csv("/home/administrator/workplace/univ/zno_project/data/admin_ter/2018village_city.csv",
                   fileEncoding = "CP1251")

plot_ly(vc2016, x=~region, y=~village, type="bar", name="village", 
        marker=list(color="mediumseagreen", opacity=0.8)) %>%
  add_trace(vc2016, x=~region, y=~city, type="bar", name="city", 
            marker=list(color="darkslategrey", opacity=0.8)) %>%
  layout(title="Cities and villages distribution 2016", 
         xaxis=list(title=""), yaxis=list(title=""))

plot_ly(vc2017, x=~region, y=~village, type="bar", name="village", 
        marker=list(color="mediumseagreen", opacity=0.8)) %>%
  add_trace(vc2017, x=~region, y=~city, type="bar", name="city", 
            marker=list(color="darkslategrey", opacity=0.8)) %>%
  layout(title="Cities and villages distribution 2017", 
         xaxis=list(title=""), yaxis=list(title=""))

plot_ly(vc2018, x=~region, y=~village, type="bar", name="village", 
        marker=list(color="mediumseagreen", opacity=0.8)) %>%
  add_trace(vc2018, x=~region, y=~city, type="bar", name="city", 
            marker=list(color="darkslategrey", opacity=0.8)) %>%
  layout(title="Cities and villages distribution 2018", 
         xaxis=list(title=""), yaxis=list(title=""))

vc2016[, "2017-2016"] <- vc2017[, "village"] - vc2016[, "village"]
plot_ly(vc2016, x=vc2016[vc2016[, "2017-2016"] > 0, "region"], 
        y=vc2016[vc2016[, "2017-2016"] > 0, "2017-2016"], type="bar",
        marker=list(color="springgreen"), 
        name="increased") %>%
  add_trace(vc2016, x=vc2016[vc2016[, "2017-2016"] <= 0, "region"], 
            y=vc2016[vc2016[, "2017-2016"] <= 0, "2017-2016"], type="bar",
            marker=list(color="darkred"),
            name="decreased") %>%
  layout(title="Difference in villages 2016 & 2017")

vc2017[, "2018-2017"] <- vc2018[, "village"] - vc2017[, "village"]
plot_ly(vc2017, x=vc2017[vc2017[, "2018-2017"] > 0, "region"], 
        y=vc2017[vc2017[, "2018-2017"] > 0, "2018-2017"], type="bar",
        marker=list(color="springgreen"), 
        name="increased") %>%
  add_trace(vc2017, x=vc2017[vc2017[, "2018-2017"] <= 0, "region"], 
            y=vc2017[vc2017[, "2018-2017"] <= 0, "2018-2017"], type="bar",
            marker=list(color="darkred"),
            name="decreasedr") %>%
  layout(title="Difference in villages 2017 & 2018")

vc2016[, "2018-2016"] <- vc2018[, "village"] - vc2016[, "village"]

plot_ly(vc2016, x=vc2016[vc2016[, "2018-2016"] > 0, "region"], 
        y=vc2016[vc2016[, "2018-2016"] > 0, "2018-2016"], type="bar",
        marker=list(color="springgreen"), 
        name="increased") %>%
  add_trace(vc2016, x=vc2016[vc2016[, "2018-2016"] <= 0, "region"], 
            y=vc2016[vc2016[, "2018-2016"] <= 0, "2018-2016"], type="bar",
            marker=list(color="darkred"),
            name="decreased") %>%
  layout(title="Difference in villages 2016 & 2018")


