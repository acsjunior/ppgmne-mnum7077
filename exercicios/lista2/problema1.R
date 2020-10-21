library(lpSolve)

Z <- c(1,5,9,5)

r <- c(1,3,9,6,
       6,6,0,7,
       7,8,18,3)

const_matrix <- matrix(r, ncol = length(Z), byrow = T)
const_direct <- c("<=", "<=", "<=")
const_rhs <- c(16, 19, 44)

si <- lp(direction = "max",
         objective.in = Z,
         const.mat = const_matrix,
         const.dir = const_direct,
         const.rhs = const_rhs,
         int.vec = 1:4)

si
si$solution
# Z = 20
# x1 = 1
# x2 = 2
# x3 = 1
# x4 = 0

# ------------------------------------------------------------------------------
# Camada 0

s0 <- lp(direction = "max",
         objective.in = Z,
         const.mat = const_matrix,
         const.dir = const_direct,
         const.rhs = const_rhs)

s0
s0$solution
# Z = 22.33333
# x1 = 0
# x2 = 3.1666667
# x3 = 0.7222222
# x4 = 0

# Próximos passos:
# x2 <= 3 (s1)
# x2 >= 4 (s2)
# x3 <= 0 (s3)
# x3 >= 1 (s4)



# ------------------------------------------------------------------------------
# Camada 1

# Solução 1

# Incluir a restrição:
# x2 <= 3

r1 <- c(0,1,0,0)
d1 <- "<="
rhs1 <- 3

const_matrix1 <- rbind(const_matrix, r1)
const_direct1 <- append(const_direct, d1)
const_rhs1 <- append(const_rhs, rhs1)

s1 <- lp(direction = "max",
          objective.in = Z,
          const.mat = const_matrix1,
          const.dir = const_direct1,
          const.rhs = const_rhs1)

s1
s1$solution
# Z = 22
# x1 = 0
# x2 = 3
# x3 = 0.7777778
# x4 = 0


# Solução 3

# Incluir a restrição:
# x3 <= 0

r3 <- c(0,0,1,0)
d3 <- "<="
rhs3 <- 0

const_matrix3 <- rbind(const_matrix, r3)
const_direct3 <- append(const_direct, d3)
const_rhs3 <- append(const_rhs, rhs3)

s3 <- lp(direction = "max",
         objective.in = Z,
         const.mat = const_matrix3,
         const.dir = const_direct3,
         const.rhs = const_rhs3)

s3
s3$solution
# Z = 15,83
# x1 = 0
# x2 = 3.166667
# x3 = 0
# x4 = 0


# ------------------------------------------------------------------------------
# Camada 2

# Incluir a restrição:
# x3 <= 0

r7 <- c(0,1,0,0)
d7 <- "<="
rhs7 <- 3

const_matrix7 <- rbind(const_matrix3, r7)
const_direct7 <- append(const_direct3, d7)
const_rhs7 <- append(const_rhs3, rhs7)

s7 <- lp(direction = "max",
         objective.in = Z,
         const.mat = const_matrix7,
         const.dir = const_direct7,
         const.rhs = const_rhs7)

s7
s7$solution
