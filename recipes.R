download.file('https://s3-api.us-geo.objectstorage.softlayer.net/cf-courses-data/CognitiveClass/DS0103EN/labs/data/recipes.csv',
              destfile = 'C:/Users/Admin/Desktop/Riesgo_Maestría/recipes.csv', quiet = TRUE)


recipes <- read.csv('C:/Users/Admin/Desktop/Riesgo_Maestría/recipes.csv')
head(recipes)
ncol(recipes)
nrow(recipes)

# Sirve para encontrar palabras inmersas en una base de datos 
grep("rice", names(recipes), value = TRUE) # yes as rice
grep("wasabi", names(recipes), value = TRUE) # yes
grep("soy", names(recipes), value = TRUE) # yes as soy_sauce
grep("pepper", names(recipes), value = TRUE) # yes as pepper
grep("pork", names(recipes), value = TRUE) # yes as soy_sauce

base::table(recipes$country) # frequency table
colnames(recipes)[1] = "cuisine"
recipes$cuisine <- tolower(as.character(recipes$cuisine))

recipes

recipes$cuisine[recipes$cuisine == "austria"] <- "austrian"
recipes$cuisine[recipes$cuisine == "belgium"] <- "belgian"
recipes$cuisine[recipes$cuisine == "china"] <- "chinese"
recipes$cuisine[recipes$cuisine == "canada"] <- "canadian"
recipes$cuisine[recipes$cuisine == "netherlands"] <- "dutch"
recipes$cuisine[recipes$cuisine == "france"] <- "french"
recipes$cuisine[recipes$cuisine == "germany"] <- "german"
recipes$cuisine[recipes$cuisine == "india"] <- "indian"
recipes$cuisine[recipes$cuisine == "indonesia"] <- "indonesian"
recipes$cuisine[recipes$cuisine == "iran"] <- "iranian"
recipes$cuisine[recipes$cuisine == "israel"] <- "jewish"
recipes$cuisine[recipes$cuisine == "italy"] <- "italian"
recipes$cuisine[recipes$cuisine == "japan"] <- "japanese"
recipes$cuisine[recipes$cuisine == "korea"] <- "korean"
recipes$cuisine[recipes$cuisine == "lebanon"] <- "lebanese"
recipes$cuisine[recipes$cuisine == "malaysia"] <- "malaysian"
recipes$cuisine[recipes$cuisine == "mexico"] <- "mexican"
recipes$cuisine[recipes$cuisine == "pakistan"] <- "pakistani"
recipes$cuisine[recipes$cuisine == "philippines"] <- "philippine"
recipes$cuisine[recipes$cuisine == "scandinavia"] <- "scandinavian"
recipes$cuisine[recipes$cuisine == "spain"] <- "spanish_portuguese"
recipes$cuisine[recipes$cuisine == "portugal"] <- "spanish_portuguese"
recipes$cuisine[recipes$cuisine == "switzerland"] <- "swiss"
recipes$cuisine[recipes$cuisine == "thailand"] <- "thai"
recipes$cuisine[recipes$cuisine == "turkey"] <- "turkish"
recipes$cuisine[recipes$cuisine == "irish"] <- "uk-and-irish"
recipes$cuisine[recipes$cuisine == "uk-and-ireland"] <- "uk-and-irish"
recipes$cuisine[recipes$cuisine == "vietnam"] <- "vietnamese"

recipes

# sort the table of cuisines by descending order
t <- sort(base::table(recipes$cuisine), decreasing = T);t
# get cuisines with >= 50 recipes
filter_list <- names(t[t >= 50]);filter_list
before <- nrow(recipes) # number of rows of original dataframe
print(paste0("Number of rows of original dataframe is ", before))

recipes <- recipes[recipes$cuisine %in% filter_list,]

after <- nrow(recipes)
print(paste0("Number of rows of processed dataframe is ", after))

print(paste0(before - after, " rows removed!"))


recipes[,names(recipes)] <- lapply(recipes[,names(recipes)], as.factor);recipes
str(recipes)

check_recipes <- recipes[
  recipes$rice == "Yes" &
    recipes$soy_sauce == "Yes" &
    recipes$wasabi == "Yes" &
    recipes$seaweed == "Yes",
]

check_recipes


# sum the row count when the value of the row in a column is equal to "Yes" (value of 2)
ingred <- unlist(
  lapply( recipes[, names(recipes)], function(x) sum(as.integer(x) == 2))
)

# transpose the dataframe so that each row is an ingredient
ingred <- as.data.frame( t( as.data.frame(ingred) ))

ing_df <- data.frame("ingredient" = names(ingred), 
                     "count" = as.numeric(ingred[1,])
)[-1,]

ing_df


ing_df_sort <- ing_df[order(ing_df$count, decreasing = TRUE),]
rownames(ing_df_sort) <- 1:nrow(ing_df_sort)

ing_df_sort
head(ing_df_sort, 10)


# create a dataframe of the counts of ingredients by cuisine, normalized by the number of 
# recipes pertaining to that cuisine
by_cuisine_norm <- aggregate(recipes, 
                             by = list(recipes$cuisine), 
                             FUN = function(x) round(sum(as.integer(x) == 2)/
                                                       length(as.integer(x)),4))
# remove the unnecessary column "cuisine"
by_cuisine_norm <- by_cuisine_norm[,-2]

# rename the first column into "cuisine"
names(by_cuisine_norm)[1] <- "cuisine"

head(by_cuisine_norm)


for(nation in by_cuisine_norm$cuisine){
  x <- sort(by_cuisine_norm[by_cuisine_norm$cuisine == nation,][-1], decreasing = TRUE)
  cat(c(toupper(nation)))
  cat("\n")
  cat(paste0(names(x)[1:4], " (", round(x[1:4]*100,0), "%) "))
  cat("\n")
  cat("\n")
}






