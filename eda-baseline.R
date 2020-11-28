# 라이브러리 로드
library(tidyverse)
library(data.table)

# 데이터 읽기
movies <- fread("./input/movies.csv")
ratings <- fread("./input/ratings.csv")
tags <- fread("./input/tags.csv")

format(object.size(movies), units = "auto")
format(object.size(ratings), units = "auto")
format(object.size(tags), units = "auto")

# 데이터가 어떻게 생겼는가
movies %>% head
ratings %>% head
tags %>% head

# 가장 많이 달린 tag는 SF, based on a book
tags %>%
  group_by(tag) %>%
  summarise(n_tag = n()) %>%
  arrange(desc(n_tag)) %>%
  head(10)

# 어떤 사람은 무려 2만개의 tag를 생성
tags %>%
  group_by(userId) %>%
  summarise(n_user = n()) %>%
  arrange(desc(n_user)) %>%
  summary

# 가장 많은 태그가 달린 영화는 1994개의 태그가 있음
tags %>%
  group_by(movieId) %>%
  summarise(n_movie = n()) %>%
  arrange(desc(n_movie)) %>%
  summary

# 총 1988개의 태그가 존재
tags %>%
  filter(movieId=='296') %>%
  select(tag) %>%
  unique

# 영화별 평균 평점을 계산
ratings %>%
  group_by(movieId) %>%
  summarise(avg_rating = mean(rating),
            n_ratings = n()) %>%
  arrange(desc(avg_rating)) -> avg_rating_movie

# 영화별 평균 평점의 summary 값 (평점이 5개 이상 달린 경우만)
avg_rating_movie %>%
  filter(n_ratings > 5) %>%
  select(avg_rating) %>%
  summary

# 영화별 평균 평점의 분포 (평점이 5개 이상 달린 경우만)
avg_rating_movie %>%
  filter(n_ratings > 5) %>%
  ggplot(aes(x = avg_rating)) +
  geom_histogram(binwidth = 0.05) +
  xlim(0.5, 5.5)

# 영화별로 평점은 평균적으로 얼마나 달렸을까
ratings %>%
  group_by(movieId) %>%
  summarise(n_rating = n()) %>%
  arrange(desc(n_rating)) -> n_rating_movie

n_rating_movie %>%
  select(n_rating) %>%
  summary
# 평균적으로 747개가 달렸지만 중위값인 18개를 보는 것이 타당해 보임
ggplot(n_rating_movie, aes(x = n_rating)) +
  geom_histogram(binwidth = 5) +
  xlim(1, 500)

# 영화 장르의 종류를 살펴보자
movies$genres %>%
  unique() %>%
  paste0(collapse = "|") %>%
  strsplit("\\|") %>%
  unlist %>%
  unique %>%
  sort -> unique_genre
# 총 20개의 영화장르가 존재

# 장르별로 유의마한 평균평점의 차이가 있는지
avg_rating_movie %>%
  left_join(movies) -> avg_rating_movie

for(genre in unique_genre){
  avg_rating_movie <- cbind(avg_rating_movie, str_detect(avg_rating_movie$genres, genre) %>% as.numeric)
}
colnames(avg_rating_movie) <- c(colnames(avg_rating_movie)[1:5], unique_genre)

avg_rating_genre <- data.table()
for(i in 6:25){
  avg_rating_movie %>%
    filter(.[,i] == 1) %>%
    summarise(avg_rating_genre = mean(avg_rating),
              n_movies = n()) %>%
    cbind(genre = colnames(avg_rating_movie)[i], .) -> tmp
  avg_rating_genre <- rbind(avg_rating_genre, tmp)
}
avg_rating_genre %>%
  arrange(desc(avg_rating_genre)) -> avg_rating_genre
# 느와르, 다큐멘터리, 전쟁 등의 영화의 평점이 비교적 높다.
# 반면 액션, 어린이, SF, 호러 영화는 평점이 비교적 낮다.

# 장르별로 평점의 BOX-PLOT을 그려보자
genre_rating <- data.table()
for(i in 1:20){
  genre_rating %>%
    rbind(cbind(avg_rating_movie[avg_rating_movie[,i+5]==1,1:4], unique_genre[i])) -> genre_rating
}
colnames(genre_rating)[5] = "genre"
ggplot(genre_rating, aes(x = reorder(genre, avg_rating, FUN = median), y = avg_rating)) +
  geom_boxplot(outlier.colour = 'lightpink', outlier.alpha = 0.5, outlier.size = 1.5) +
  stat_summary(fun.y = "mean", geom = 'point', fill = 'gray', shape = 22) + 
  xlab("Genre") + ylab("Average Rating")

# 개봉시기, 평가횟수, 평균평점, 장르를 통해 영화를 clustering

# 개봉시기 변수 추가
avg_rating_movie %>%
  mutate(year_opening = str_sub(str_trim(title), -5, -2)) -> avg_rating_movie

avg_rating_movie %>%
  select(year_opening) %>%
  unique %>%
  t %>% sort -> uniq_year

check <- uniq_year[-(6:123)]

avg_rating_movie %>%
  filter(year_opening %in% check) %>%
  select(title)
# 개봉시기가 확인이 안되는 영화는 어떻게 처리해야 하나 (총 22편)
# (1) 수작업으로 정보 입력 (2) 삭제 ..... => 삭제하는 방향으로!

avg_rating_movie %>%
  filter(!year_opening %in% check) -> avg_rating_movie

avg_rating_movie %>%
  mutate(decade = paste0(str_sub(year_opening, 1, 3), "0")) %>%
  select(-year_opening) -> avg_rating_movie

avg_rating_movie %>%
  #filter(count_rt > 100) %>%
  ggplot(aes(x = sort(decade))) +
  geom_bar() +
  xlab("decade") + ylab('# of movies')

# 연도별 평균 평점은?
avg_rating_movie %>%
  ggplot(aes(x = decade, y = avg_rating)) +
  geom_boxplot(outlier.colour = 'lightpink', outlier.alpha = 0.5, outlier.size = 1.5) +
  stat_summary(fun.y = "mean", geom = 'point', fill = 'gray', shape = 22) + 
  xlab("Decade") + ylab("Average Rating") 

## 추천시스템 (recomenderlab)을 위한 형태로 데이터 전환
# https://statkclee.github.io/parallel-r/recommendation-sys.html
# https://medium.com/@unfinishedgod/%EC%A0%9C%ED%92%88-%EC%B6%94%EC%B2%9C-%EC%8B%9C%EC%8A%A4%ED%85%9C-%EC%95%8C%EA%B3%A0%EB%A6%AC%EC%A6%98-%EC%98%88%EC%A0%9C-422a6165c419

library(recommenderlab)

ratings
movies
tags

# ratings 데이터만 이용한 baseline 모델
rating_matrix <- as(ratings[, -4], 'realRatingMatrix')

rating_matrix@data
rating_matrix@normalize

image(rating_matrix[1:20,1:20])
rowMeans(rating_matrix[1,])

ubcf_recommender <- Recommender(rating_matrix[1:50000], "UBCF")

# 사용자에게 추천할 예상 제품 리스트
return_recom <- function(user = 1000) {
  predict(ubcf_recommender, rating_matrix[user], n=10) %>%
    as('list') %>%
    as.data.frame %>%
    t %>%
    as.vector -> movie_id
  output <- movies[movies$movieId %in% movie_id]
  cat('------추천 리스트-------', "\n")
  print(output)
  cat('------해당 고객이 원래 좋아한 영화-------', '\n')
  ratings %>%
    filter(userId == user) %>%
    left_join(movies) %>%
    select(title, rating, genres) %>%
    arrange(desc(rating)) %>%
    head(10) -> tmp
  print(tmp)
}
return_recom(138492)
