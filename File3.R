install.packages("mixtools")
install.packages("caTools")
install.packages("recommenderlab")
install.packages("reshape2")

suppressMessages(library("dplyr"))
suppressMessages(library("ggplot2"))
suppressMessages(library("mixtools"))
suppressMessages(library("caTools"))
suppressMessages(library("recommenderlab"))
suppressMessages(library("reshape2"))

steam <- read.csv("C:\\Users\\SRIDHAR\\Downloads\\Videogames.csv", header = FALSE)[-5]
colnames(steam) <- c('user', 'game', 'purchase_play', 'hrs')
str(steam)
steam_clean <- steam

apply(steam_clean, 2, function(x) sum(is.na(x) ))

steam_clean$purchase <- sapply(steam_clean$purchase_play, function(x) as.numeric(x == 'purchase'))
steam_clean$play <- sapply(steam_clean$purchase_play, function(x) as.numeric(x == 'play'))
steam_clean$hrs <- steam_clean$hrs-steam_clean$purchase
steam_clean <- steam_clean[,-3]
steam_clean <- aggregate(. ~ user + game, data = steam_clean, FUN = 'sum')
head(steam_clean)

ngames <- length(unique(steam_clean$game))
nusers <- length(unique(steam_clean$user))
cat("There are", ngames, "games purchased by", nusers, "users")

game_total_hrs <- aggregate(hrs ~ game, data = steam_clean, FUN = 'sum')
game_total_hrs <- game_total_hrs[order(game_total_hrs$hrs, decreasing = TRUE),]
most_played_games <- head(game_total_hrs, 20)
most_played_games <- data.frame(game = most_played_games$game, hrs = most_played_games$hrs)
head(most_played_games, 20)

game_freq <- data.frame(sort(table(steam_clean$game), decreasing = TRUE))
colnames(game_freq) <- c("game", "nusers")
top20 <- merge(game_freq, game_total_hrs, by = 'game')
top20 <- head(top20[order(top20$nusers, decreasing = TRUE),], 20)
top20
ggplot(top20, aes(x = game, y = nusers, fill = hrs)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  labs(title = "Top 20 games with the most users", x = "Game", y = "Number of users")

purchased_not_played <- subset(steam_clean, purchase == 1 & play == 0)
nTransactions <-nrow(purchased_not_played)
nusers <- length(unique(purchased_not_played$user))
nPurchased <- nrow(subset(steam_clean, purchase == 1))
cat("There are", nTransactions, "games purchased out of", nPurchased, "that have not been played by", nusers, "users")

steam_clean$game1 <- gsub("[^a-zA-Z0-9]", "", steam_clean$game)
head(steam_clean)

game_hrs_density <- function(GAME, nclass, print_vals = TRUE){
  # subsetting data
  game_data <- subset(steam_clean, game1 == GAME & hrs > 2)
  game_data$loghrs <- log(game_data$hrs)
  
  # em algorithm
  mu.init <- seq(min(game_data$loghrs), max(game_data$loghrs), length = nclass)
  EM <- normalmixEM(game_data$loghrs, mu = mu.init, sigma=rep(1, nclass))
  
  # print results
  if(print_vals){
    cat(" lambda: ", EM$lambda, "\n mean  : ", EM$mu, "\n sigma : ", EM$sigma, "\n")
  }
  
  # building data frame for plotting
  x <- seq(min(game_data$loghrs), max(game_data$loghrs), 0.01)
  dens <- data.frame(x = x)
  for(k in 1:nclass){
    dens[,paste0('y', k)] <- EM$lambda[k]*dnorm(x, EM$mu[k], EM$sigma[k])
  }
  
  dens <- melt(dens, 'x', variable.name = 'gaussian')
  game_plt <- ggplot(game_data, aes(x = loghrs)) + 
    geom_histogram(aes(y = ..density..), bins = 25, colour = "black", alpha = 0.7, size = 0.1) +
    geom_area(data = dens, aes(x = x, y = value), alpha = 0.5, position = position_dodge()) +
    geom_density(linetype = 2, size = 0.1) + 
    ggtitle(game_data$game[1])
  
  return(game_plt)
}
game_hrs_density("TheWitcher3WildHunt", 5, print_vals = TRUE)


set.seed(0910)
game_freq$game1 <- gsub("[^a-zA-Z0-9]", "", game_freq$game)
game_users <- subset(game_freq, game_freq$nusers > 50)
steam_clean_pos <- subset(steam_clean, steam_clean$hrs > 2 & (steam_clean$game1 %in% game_users$game1))
steam_clean_pos$loghrs <- log(steam_clean_pos$hrs)

# make matrix
games <- data.frame(game1 = sort(unique(steam_clean_pos$game1)), game_id = 1:length(unique(steam_clean_pos$game1)))
users <- data.frame(user = sort(unique(steam_clean_pos$user)), user_id = 1:length(unique(steam_clean_pos$user)))
steam_clean_pos <- merge(steam_clean_pos, games, by = 'game1')
steam_clean_pos <- merge(steam_clean_pos, users, by = 'user')
ui_mat <- matrix(0, nrow = nrow(users), ncol = nrow(games), dimnames = list(user = paste0("u", sort(unique(steam_clean_pos$user))), 
                                                                            game = sort(unique(steam_clean_pos$game1))))
for(k in 1:nrow(steam_clean_pos)){
  ui_mat[steam_clean_pos$user_id[k], steam_clean_pos$game_id[k]] <- steam_clean_pos$loghrs[k]
}

# create training set i.e. suppress a tenth of the actual ratings
index <- sample.split(steam_clean_pos$user, SplitRatio = 0.9)
train <- steam_clean_pos[index,]
test <- steam_clean_pos[!index,]
ui_train <- ui_mat
for(k in 1:nrow(test)){
  ui_train[test$user_id[k], test$game_id[k]] <- 0
}


# root mean squared error function
rmse <- function(pred, test, data_frame = FALSE){
  test_pred <- rep(NA, nrow(test))
  for(k in 1:nrow(test)){
    test_pred[k] <- pred[test$user_id[k], test$game_id[k]]
  }
  if(data_frame){
    return(data.frame(test_pred, test$loghrs))
  }
  return(sqrt(1/(nrow(test)-1)*sum((test_pred - test$loghrs)^2)))
}

cat("Dimensions of training user-item matrix:", dim(ui_train))


Y <- ui_train

# mean impute
Y <- apply(Y, 2, function(x) ifelse(x == 0, mean(x), x))
Y_svd <- svd(Y)
U <- Y_svd$u
V <- Y_svd$v
D <- Y_svd$d
ggplot(data.frame(x = 1:length(D), y = D/sum(D)), aes(x = x, y = y)) + 
  geom_line() + 
  labs(x = "Leading component", y = "")

# take the first 15 leading components
lc <- 60
pred <- U[,1:lc] %*% diag(D[1:lc]) %*% t(V[,1:lc])

rmse(pred, test)
head(rmse(pred, test, TRUE))

leading_components <- 60
Y <- ui_train
I <- apply(Y, 2, function(x) ifelse(x>0, 1, 0))
U <- matrix(rnorm(nrow(Y)*leading_components, 0, 0.01), ncol = leading_components)
V <- matrix(rnorm(ncol(Y)*leading_components, 0, 0.01), ncol = leading_components)

# objective function
f <- function(U, V){
  return(sum(I*(U%*%t(V)-Y)^2))
}
dfu <- function(U){
  return((2*I*(U%*%t(V)-Y))%*%V)
}
dfv <- function(V){
  return(t(2*I*(U%*%t(V)-Y))%*%U)
}

# gradient descent
N <- 200
alpha <- 0.001
pred <- round(U%*%t(V), 2)
fobj <- f(U, V)
rmsej <- rmse(pred, test)
pb <- txtProgressBar(min = 0, max = N, style = 3)
start <- Sys.time()
for(k in 1:N){
  U <- U - alpha*dfu(U)
  V <- V - alpha*dfv(V)
  fobj <- c(fobj, f(U, V))
  pred <- round(U%*%t(V), 2)
  rmsej <- c(rmsej, rmse(pred, test))
  setTxtProgressBar(pb, k)
}
close(pb)
Sys.time()-start
path1 <- data.frame(itr = 1:(N+1), fobj, fobjp = fobj/max(fobj), rmse = rmsej, rmsep = rmsej/max(rmsej))
path1gg <- melt(path1[c("itr", "fobjp", "rmsep")], id.vars = "itr")
ggplot(path1gg, aes(itr, value, color = variable)) + geom_line()
dimnames(pred) <- list(user = rownames(ui_train), game = colnames(ui_train))

# printing final iteration
tail(path1, 1)

game_hrs_density_p <- function(pred, GAME = NULL, nclass, print_vals = TRUE){
  
  if(is.null(GAME)){
    GAME <- sample(colnames(pred), 1)
  }
  
  # subsetting data
  game_data <- subset(pred[,GAME], pred[,GAME] > 0) 
  
  # em algorithm
  mu.init <- seq(min(game_data), max(game_data), length = nclass)
  EM <- normalmixEM(game_data, mu = mu.init, sigma=rep(1, nclass), fast = TRUE)
  
  # print results
  if(print_vals){
    cat(" lambda: ", EM$lambda, "\n mean  : ", EM$mu, "\n sigma : ", EM$sigma, "\n")
  }
  
  # building data frame for plotting
  x <- seq(min(game_data), max(game_data), 0.01)
  dens <- data.frame(x = x)
  for(k in 1:nclass){
    dens[,paste0('y', k)] <- EM$lambda[k]*dnorm(x, EM$mu[k], EM$sigma[k])
  }
  
  dens <- melt(dens, 'x', variable.name = 'gaussian')
  game_plt <- ggplot(as.data.frame(game_data), aes(x = game_data)) + 
    geom_histogram(aes(y = ..density..), bins = 45, colour = "black", alpha = 0.7, size = 0.1) +
    geom_area(data = dens, aes(x = x, y = value, fill = gaussian), alpha = 0.5, position = position_dodge()) +
    geom_density(linetype = 2, size = 0.1) + 
    ggtitle(GAME)
  
  return(game_plt)
}
game_hrs_density_p(pred, "TheWitcher3WildHunt", 5)

pred_percentile <- apply(pred, 2, percent_rank)
top <- function(n, user = NULL){
  if(is.null(user)){
    user <- sample(rownames(pred_percentile), 1)
  }
  not_purchased <- (I-1)%%2
  top_games <- names(sort((pred_percentile*not_purchased)[user,], decreasing = TRUE))[1:n]
  cat("top", n, "recommended games for user", user, ":\n")
  for(k in 1:n){
    cat(k, ")", top_games[k], "\n")
  }
}
top(5)
