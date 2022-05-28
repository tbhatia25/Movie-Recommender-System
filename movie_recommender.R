movies <- read.csv('C:/Users/Taranpreet/Desktop/movies.csv')
ratings <- read.csv('C:/Users/Taranpreet/Desktop/ratings.csv')

ratings_user_570 <- ratings[ratings$userId == 570,]
movie_ids <- movies[movies$movieId %in% c(18,231,592,914,1225,2000,3210,3801,54997,88812) ,]
#movie_ids <- movies[movies$movieId %in% c(5612, 943, 7358, 25, 2108, 8371, 2699, 8923, 2487, 102800) ,]

df_user_prof <- data.frame(matrix(ncol = 20, nrow = 0))
df_movie_prof <- data.frame(matrix(ncol = 20, nrow = 0))
df_similarity <- data.frame(matrix(ncol = 3, nrow = 0))

x <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime", "Documentary", "Drama", "Fantasy",
       "Film-Noir", "Horror", "IMAX", "Musical", "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western", "(no
genres listed)")
colnames(df_user_prof) <- x
colnames(df_movie_prof) <- x
x <- c("MovieId", "MovieName", "Similarity")
colnames(df_similarity) <- x

library(dplyr)
movies_user_570 <- movies %>%
  filter(movieId %in% ratings_user_570$movieId)
df_movies_gen <- data.frame(do.call("rbind",strsplit(movies_user_570$genres, "|",fixed = TRUE)))
df_movies_570_gen <- data.frame(do.call("rbind",strsplit(movie_ids$genres, "|",fixed = TRUE)))

x <- c(1:ncol(df_movies_gen))
y <- c(1:nrow(ratings_user_570))
vec <- numeric()

for(i in y){
  for(j in x){
    df_user_prof[i,df_movies_gen[i,j]] = 1
    df_movie_prof[1,df_movies_570_gen[1,j]] = 1
    df_movie_prof[2,df_movies_570_gen[2,j]] = 1
    df_movie_prof[3,df_movies_570_gen[3,j]] = 1
    df_movie_prof[4,df_movies_570_gen[4,j]] = 1
    df_movie_prof[5,df_movies_570_gen[5,j]] = 1
    df_movie_prof[6,df_movies_570_gen[6,j]] = 1
    df_movie_prof[7,df_movies_570_gen[7,j]] = 1
    df_movie_prof[8,df_movies_570_gen[8,j]] = 1
    df_movie_prof[9,df_movies_570_gen[9,j]] = 1
    df_movie_prof[10,df_movies_570_gen[10,j]] = 1
  }
}

df_movie_prof[is.na(df_movie_prof)] <- 0

colsum_user <- colSums(df_user_prof, na.rm = T)

vec <- append(vec,colsum_user/nrow(ratings_user1))

my.cosine <- function(x,y){
  sum(x*y)/(norm(x, type="2") * norm(y,type = "2"))
}
my.cosine(vec,df_movie_prof[1,])

for(i in 1:10){
  df_similarity[i,] <-  c(movie_ids[i,1],movie_ids[i,2], format(round(my.cosine(vec,df_movie_prof[i,]),4), nsmall = 4))
}

cat("User ID 570 chose the following 10 movies: 18,231,592,914,1225,2000,3210,3801,54997,88812\n")
cat("Of these, the following 5 movies are recommended:\n")
print(head(df_similarity[with(df_similarity, order(-xtfrm(Similarity))), ],n=5))