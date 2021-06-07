# かんたん感染症シミュレーション
# July 2, 2010 - ver.0

# 隣接行列をつくる関数
makematrix <- function(n){
x <- matrix(sample(c(0,1),n^2,replace=TRUE),n,n)
x[lower.tri(x,diag=TRUE)] <- 0
x <- x + t(x)
return(x)
}

###########
# simulation #
###########

# 個体数の定義
n <- 100

replicate = 100
total_time <- c()
total_infector <- c()
total_link <- c()
for (r in 1:replicate){

# 行列をつくる
x <- makematrix(100)
# リンクの数と感染率の定義
link <- apply(x,1,sum)
infection <- n / link ^ 2

infector <- rep(0,n)
initiator <- sample(1:100, 1, replace=TRUE)
infector[initiator] <- 1
# main loop
t <- 0
while (sum(infector)/n < 0.5){
	for (i in 1:n){
		for (j in 1:n){
			if (runif(1) < infector[j] * x[i,j] * infection[i]){
				infector[i] <- 1
			}
		}
	}
	t <- t + 1
}

total_infector <- c(total_infector, infector)
total_link <- c(total_link, link)
total_time <- c(total_time, t)

}

# 解析
y <- cbind(total_infector, total_link)
infected <- subset (y, y[,1]==1)
noinfected <- subset(y,y[,1]==0)
