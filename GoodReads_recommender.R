setwd("/Users/R_Playground/goodbooks-10k")
library(recommenderlab)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(DT)
library(reshape2)
library(knitr)
library(grid)
library(gridExtra)
library(corrplot)
library(qgraph)
library(methods)
library(Matrix)



##########################################################  CREATING DATATABLES ####################################################

ratings <- fread("ratings.csv")
books <- fread("books.csv")
book_tags <- fread("book_tags.csv")
tags <- fread("tags.csv")

knitr::opts_chunk$set(warning = FALSE, message = FALSE)


# CLEANING
################################################################### CLEANING DATATABLES ################################
#REMOVING DUPLICATE RATINGS ONLY
ratings[, N := .N, .(user_id, book_id)]
## corresponding dplyr code
# ratings %>% group_by(user_id, book_id) %>% mutate(n=n())
cat('Number of duplicate ratings: ', nrow(ratings[N > 1]))


ratings <- ratings[N == 1]

#let’s SEE AND REMOVE users who rated fewer than 3 books.
ratings[, N := .N, .(user_id)]
## corresponding dplyr code
# ratings %>% group_by(user_id) %>% mutate(n = n())
cat('Number of users who rated fewer than 3 books: ', uniqueN(ratings[N <= 2, user_id]))

ratings <- ratings[N > 2]
############################################################## SELECT A SUBSET OF USERS ######################################
#To reduce calculation times in this kernel, I select only a subset of users. (e.g., 20%)

set.seed(1)
user_fraction <- 0.2
users <- unique(ratings$user_id)
sample_users <- sample(users, round(user_fraction * length(users)))

cat('Number of ratings (before): ', nrow(ratings))
ratings <- ratings[user_id %in% sample_users]
cat('Number of ratings (after): ', nrow(ratings))

# EDA
################################################################### EDA ################################
#We see that people tend to give quite positive ratings to books. 
#Most of the ratings are in the 3-5 range, while very few ratings are in the 1-2 range.

#dsitribution of ratings

ratings %>% 
  ggplot(aes(x = rating, fill = factor(rating))) +
  geom_bar(color = "grey20") + scale_fill_brewer(palette = "YlGnBu") + guides(fill = FALSE)
#Most of the ratings are in the 3-5 range, while very few ratings are in the 1-2 range.

#Number of ratings per user
ratings %>% 
  group_by(user_id) %>% 
  summarize(number_of_ratings_per_user = n()) %>% 
  ggplot(aes(number_of_ratings_per_user)) + 
  geom_bar(fill = "cadetblue3", color = "grey20") + coord_cartesian(c(3, 50))

# we can also see that are some users with many ratings. 

#Distribution of mean user ratings
ratings %>% 
  group_by(user_id) %>% 
  summarize(mean_user_rating = mean(rating)) %>% 
  ggplot(aes(mean_user_rating)) +
  geom_histogram(fill = "cadetblue3", color = "grey20")
#sudden dips are observed as well

#Distribution of mean book ratings
ratings %>% 
  group_by(book_id) %>% 
  summarize(mean_book_rating = mean(rating)) %>% 
  ggplot(aes(mean_book_rating)) + geom_histogram(fill = "orange", color = "grey20") + coord_cartesian(c(1,5))
#We can see that in the subsetted dataset most books have around 18-20 ratings.

############################## GENRES DISTRIBUTION ##################

genres <- str_to_lower(c("Art", "Biography", "Business", "Chick Lit", "Children's", "Christian", "Classics", "Comics", "Contemporary",
                         "Cookbooks", "Crime", "Ebooks", "Fantasy", "Fiction", "Gay and Lesbian", "Graphic Novels",
                         "Historical Fiction", "History", "Horror", "Humor and Comedy", "Manga", "Memoir", "Music", 
                         "Mystery", "Nonfiction", "Paranormal", "Philosophy", "Poetry", "Psychology", "Religion",
                         "Romance", "Science", "Science Fiction", "Self Help", "Suspense", "Spirituality", "Sports",
                         "Thriller", "Travel", "Young Adult"))

exclude_genres <- c("fiction", "nonfiction", "ebooks", "contemporary")
genres <- setdiff(genres, exclude_genres)

available_genres <- genres[str_to_lower(genres) %in% tags$tag_name]
available_tags <- tags$tag_id[match(available_genres, tags$tag_name)]

tmp <- book_tags %>% 
  filter(tag_id %in% available_tags) %>% 
  group_by(tag_id) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(sumN = sum(n), percentage = n / sumN) %>%
  arrange(-percentage) %>%
  left_join(tags, by = "tag_id")

tmp %>% 
  ggplot(aes(reorder(tag_name, percentage), percentage, fill = percentage)) + geom_bar(stat = "identity") + 
  coord_flip() + scale_fill_distiller(palette = 'YlOrRd') + labs(y = 'Percentage', x = 'Genre')

###################################################### Different languages c

p1 <- books %>% 
  mutate(language = factor(language_code)) %>% 
  group_by(language) %>% 
  summarize(number_of_books = n()) %>% 
  arrange(-number_of_books) %>% 
  ggplot(aes(reorder(language, number_of_books), number_of_books, fill = reorder(language, number_of_books))) +
  geom_bar(stat = "identity", color = "grey20", size = 0.35) + coord_flip() +
  labs(x = "language", title = "english included") + guides(fill = FALSE)

p2 <- books %>% 
  mutate(language = factor(language_code)) %>% 
  filter(!language %in% c("en-US", "en-GB", "eng", "en-CA", "")) %>% 
  group_by(language) %>% 
  summarize(number_of_books = n()) %>% 
  arrange(-number_of_books) %>% 
  ggplot(aes(reorder(language, number_of_books), number_of_books, fill = reorder(language, number_of_books))) +
  geom_bar(stat = "identity", color = "grey20", size = 0.35) + coord_flip() +
  labs(x = "", title = "english excluded") + guides(fill = FALSE)

grid.arrange(p1,p2, ncol=2)

####################################################### Top 10 rated books ###################################################### 

books %>% 
  mutate(image = paste0('<img src="', small_image_url, '"></img>')) %>% 
  arrange(-average_rating) %>% 
  top_n(10,wt = average_rating) %>% 
  select(image, title, ratings_count, average_rating) %>% 
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))

####################################################### Top 10 popular books ###################################################### 

books %>% 
  mutate(image = paste0('<img src="', small_image_url, '"></img>')) %>% 
  arrange(-ratings_count) %>% 
  top_n(10,wt = ratings_count) %>% 
  select(image, title, ratings_count, average_rating) %>% 
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))


####################################################### What influences a book’s rating ##################################################### 



tmp <- books %>% 
  select(one_of(c("books_count","original_publication_year","ratings_count", "work_ratings_count", 
                  "work_text_reviews_count", "average_rating"))) %>% 
  as.matrix()

corrplot(cor(tmp, use = 'pairwise.complete.obs'), type = "lower")

######################################################################## COLLABORATIVE FILTERING ###############################################
dimension_names <- list(user_id = sort(unique(ratings$user_id)), book_id = sort(unique(ratings$book_id)))
ratingmat <- spread(select(ratings, book_id, user_id, rating), book_id, rating) %>% select(-user_id)

ratingmat <- as.matrix(ratingmat)
dimnames(ratingmat) <- dimension_names
ratingmat[1:5, 1:5]

######################################################################### Step 1: Find similar users
current_user <- "1339"
rated_items <- which(!is.na((as.data.frame(ratingmat[current_user, ]))))
selected_users <- names(which(apply(!is.na(ratingmat[ ,rated_items]), 1, sum) >= 2))
head(selected_users, 40)

#Typically cosine similarity or pearson’s correlation coefficient are used. 

# Below I do this for 2 users (user_ids: 1339 and 21877) for illustration. We can see that similarity is higher for user 1339 than user 21877
#1339
user1 <- data.frame(item=colnames(ratingmat),rating=ratingmat[current_user,]) %>% filter(!is.na(rating))
user2 <- data.frame(item=colnames(ratingmat),rating=ratingmat["1339",]) %>% filter(!is.na(rating))
tmp<-merge(user1, user2, by="item")
tmp

#Correlation function

cor(tmp$rating.x, tmp$rating.y, use="pairwise.complete.obs")

#21877
user2 <- data.frame(item = colnames(ratingmat), rating = ratingmat["21877", ]) %>% filter(!is.na(rating))
tmp <- merge(user1, user2, by="item")
tmp

#Correlation function
cor(tmp$rating.x, tmp$rating.y, use="pairwise.complete.obs")



#subtracting interindividual differences in mean
rmat <- ratingmat[selected_users, ]
user_mean_ratings <- rowMeans(rmat,na.rm=T)
rmat <- rmat - user_mean_ratings

#Finding the similarity
similarities <- cor(t(rmat[rownames(rmat)!=current_user, ]), rmat[current_user, ], use = 'pairwise.complete.obs')
sim <- as.vector(similarities)
names(sim) <- rownames(similarities)
res <- sort(sim, decreasing = TRUE)
head(res, 40)


#Visualizing similarities between users


sim_mat <- cor(t(rmat), use = 'pairwise.complete.obs')
random_users <- selected_users[1:20]
qgraph(sim_mat[c(current_user, random_users), c(current_user, random_users)], layout = "spring",
       vsize = 5, theme = "TeamFortress", labels = c(current_user, random_users))

######################################################################### Step 2: Get predictions for other books


similar_users <- names(res[1:4])

similar_users_ratings <- data.frame(item = rep(colnames(rmat), length(similar_users)), 
                                    rating = c(t(as.data.frame(rmat[similar_users,])))) %>% filter(!is.na(rating))

current_user_ratings <- data.frame(item = colnames(rmat), rating = rmat[current_user,]) %>% filter(!is.na(rating))

predictions <- similar_users_ratings %>% 
  filter(!(item %in% current_user_ratings$item)) %>% 
  group_by(item) %>% summarize(mean_rating = mean(rating))

predictions %>% 
  datatable(class = "nowrap hover row-border", options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))

######################################################################### Step 3: Recommend the best 5 predictions

predictions %>% 
  arrange(-mean_rating) %>% 
  top_n(5, wt = mean_rating) %>% 
  mutate(book_id = as.numeric(as.character(item))) %>% 
  left_join(select(books, authors, title, book_id), by = "book_id") %>% 
  select(-item) %>% 
  datatable(class = "nowrap hover row-border", options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))

######################################################################### USING RECOMMENDERLAB

#matrix in sparse format
ratingmat0 <- ratingmat
ratingmat0[is.na(ratingmat0)] <- 0
sparse_ratings <- as(ratingmat0, "sparseMatrix")
rm(ratingmat0)
gc()

#Recommenderlab uses as special variant of a sparse matrices, so we convert to this class first.

real_ratings <- new("realRatingMatrix", data = sparse_ratings)
real_ratings


######################################################################### UBCF
#Running an algorithm in Recommenderlab is really easy. All you have to do is call Recommender() and pass the data,
#select a method (“UBCF” - user-based collaborative filtering) 

model <- Recommender(real_ratings, method = "UBCF", param = list(method = "pearson", nn = 4))
#Making predictions 
prediction <- predict(model, real_ratings[current_user, ], type = "ratings")

#Let’s have a look at the best predictions for David:

as(prediction, 'data.frame') %>% 
  arrange(-rating) %>% .[1:5,] %>% 
  mutate(book_id = as.numeric(as.character(item))) %>% 
  left_join(select(books, authors, title, book_id), by = "book_id") %>% 
  select(-item) %>% 
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't',scrollX = TRUE, autoWidth = TRUE))




