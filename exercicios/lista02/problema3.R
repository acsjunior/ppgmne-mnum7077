library(lpSolve)

Z <- c(3,4,3)

r <- c(3,2,2,
       2,5,3,
       2,1,2)

const_matrix <- matrix(r, ncol = length(Z), byrow = T)
const_direct <- c(">=", ">=", ">=")
const_rhs <- c(13, 15, 9)

si <- lp(direction = "min",
         objective.in = Z,
         const.mat = const_matrix,
         const.dir = const_direct,
         const.rhs = const_rhs,
         int.vec = c(2,3))


si
si$solution
# Z = 17
# x1 - 2.33
# x2 = 1
# x3 = 2


# ------------------------------------------------------------------------------
# Camada 0

s0 <- lp(direction = "min",
         objective.in = Z,
         const.mat = const_matrix,
         const.dir = const_direct,
         const.rhs = const_rhs)

s0
s0$solution
# Z = 16.56
# x1 = 2,78
# x2 = 1,22
# x3 = 1,11


# ------------------------------------------------------------------------------
# Camada 1

# # Solução 1

# Incluir a restrição:
# x2 <= 1

r1 <- c(0,1,0)
d1 <- "<="
rhs1 <- 1

const_matrix1 <- rbind(const_matrix, r1)
const_direct1 <- append(const_direct, d1)
const_rhs1 <- append(const_rhs, rhs1)

s1 <- lp(direction = "min",
         objective.in = Z,
         const.mat = const_matrix1,
         const.dir = const_direct1,
         const.rhs = const_rhs1)

s1
s1$solution
# Z = 16.6
# x1 = 2.6
# x2 = 1
# x3 = 1.6


# # Solução 3

# Incluir a restrição:
# x3 <= 1

r3 <- c(0,0,1)
d3 <- "<="
rhs3 <- 1

const_matrix3 <- rbind(const_matrix, r3)
const_direct3 <- append(const_direct, d3)
const_rhs3 <- append(const_rhs, rhs3)

s3 <- lp(direction = "min",
         objective.in = Z,
         const.mat = const_matrix3,
         const.dir = const_direct3,
         const.rhs = const_rhs3)

s3
s3$solution
# Z = 16.625
# x1 = 2.875
# x2 = 1.25
# x3 = 1


# ------------------------------------------------------------------------------
# Camada 2


# # Solução 5

# Incluir a restrição:
# x3 <= 1

r5 <- c(0,0,1)
d5 <- "<="
rhs5 <- 1

const_matrix5 <- rbind(const_matrix1, r5)
const_direct5 <- append(const_direct1, d5)
const_rhs5 <- append(const_rhs1, rhs5)

s5 <- lp(direction = "min",
         objective.in = Z,
         const.mat = const_matrix5,
         const.dir = const_direct5,
         const.rhs = const_rhs5)

s5
s5$solution
# Z = 17.5
# x1 = 3.5
# x2 = 1
# x3 = 1