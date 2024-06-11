chooseCRANmirror(ind=1)  # ABD'deki CRAN aynasını seçmek için

# Gerekli paketlerin kurulması ve yüklenmesi
packages <- c("Matrix", "arules", "proxy", "reshape2", "stringr")
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

library(Matrix)
library(arules)
library(proxy)
library(reshape2)
library(stringr)

setwd("D:\\sytem\\bilgi_yonetim_sistemi\\htdocs\\webtasarim\\wp-content\\themes\\twentytwentyfour\\r dili analiz\\r_film_analiz")
movie_data <- read.csv("movies_cleaned.csv", stringsAsFactors = FALSE)
rating_data <- read.csv("ratings.csv")

# Film türlerini işleme
movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors = FALSE)
movie_genre2 <- as.data.frame(str_split(movie_genre[,1], '[|]', simplify = TRUE), stringsAsFactors = FALSE)
colnames(movie_genre2) <- c(1:10)

list_genre <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "Film-Noir", "Horror", "Musical", "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western")
genre_mat1 <- matrix(0, nrow(movie_data), length(list_genre))
colnames(genre_mat1) <- list_genre

for (index in 1:nrow(movie_genre2)) {
  for (col in 1:ncol(movie_genre2)) {
    gen_col = which(colnames(genre_mat1) == movie_genre2[index, col])
    if(length(gen_col) > 0) {
      genre_mat1[index, gen_col] <- 1
    }
  }
}

genre_mat2 <- as.data.frame(genre_mat1, stringsAsFactors = FALSE)
for (col in 1:ncol(genre_mat2)) {
  genre_mat2[, col] <- as.integer(genre_mat2[, col])
}

SearchMatrix <- cbind(movie_data[, 1:2], genre_mat2)
rownames(SearchMatrix) <- SearchMatrix$movieId
SearchMatrix <- SearchMatrix[, -1]

# Kosinüs benzerlik matrisini hesaplama
similarity_matrix <- as.matrix(dist(SearchMatrix[, -1], method = "cosine"))
similarity_matrix <- 1 - similarity_matrix
rownames(similarity_matrix) <- movie_data$title
colnames(similarity_matrix) <- movie_data$title

# Yakın eşleşen film başlığını bulma fonksiyonu
find_closest_movie_title <- function(input_title, movie_titles) {
  match_index <- agrep(input_title, movie_titles, ignore.case = TRUE, value = TRUE, max.distance = 0.1)
  if (length(match_index) > 0) {
    return(match_index[1])
  } else {
    stop("Girilen film başlığı için yakın bir eşleşme bulunamadı.")
  }
}

# Film öneri fonksiyonu
recommend_similar_movies <- function(input_title, num_recommendations = 5) {
  movie_title <- find_closest_movie_title(input_title, rownames(similarity_matrix))
  movie_similarities <- similarity_matrix[movie_title, ]
  movie_similarities <- sort(movie_similarities, decreasing = TRUE)
  similar_movies <- names(movie_similarities)[2:(num_recommendations + 1)]
  return(similar_movies)
}

# Kullanıcıdan film başlığı ve öneri sayısı
input_title <- commandArgs(trailingOnly = TRUE)  # Film başlığı girişi (kısmi veya büyük/küçük harf duyarsız eşleşme)
num_recommendations <- 5  # Öneri sayısı
similar_movies <- recommend_similar_movies(input_title, num_recommendations)
cat(paste(similar_movies, collapse = ", "))
