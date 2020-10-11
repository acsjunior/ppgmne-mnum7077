library(lpSolve)

# Min Z = 3x1 + 4x2 + 3x3
Z = c(3,4,3)

# S.a:
# 3x1 + 2x2 + 2x3 >= 13
# 2x1 + 5x2 + 3x3 >= 15
# 2x1 + x2 + 2x3 >= 9
# x2, x3 >= 0 e inteiras
# x1 >= 0

r <- c(3,2,2,
       2,5,3,
       2,1,2,
       1,0,0,
       0,1,0,
       0,0,1)

const_matrix <- matrix(r, ncol = length(Z), byrow = T)
const_direct <- c(">=", ">=", ">=", ">=", ">=", ">=")
const_rhs <- c(13, 15, 9, 0, 0, 0)

si <- lp(direction = "min",
         objective.in = Z,
         const.mat = const_matrix,
         const.dir = const_direct,
         const.rhs = const_rhs,
         int.vec = c(2,3))

si
si$solution
# Z = 17
# x1 = 2.333333
# x2 = 1
# x3 = 2

# ------------------------------------------------------------------------------
# Camada 0

# Solução 1
s1 <- lp(direction = "min",
         objective.in = Z,
         const.mat = const_matrix,
         const.dir = const_direct,
         const.rhs = const_rhs)

s1
s1$solution
# Z = 16.55556
# x1 = 2.777778
# x2 = 1.222222
# x3 = 1.111111


# Necessário resolver os problemas com as seguintes restrições (na próxima camada)
# x2 <= 1 (s1.1)
# x2 >= 2 (s1.2)
# x3 <= 1 (s1.3)
# x3 >= 2 (s1.4)


# ------------------------------------------------------------------------------
# Camada 1

# Solução 1.1

# Incluir a restrição:
# x2 <= 1

r11 <- c(0,1,0)
d11 <- "<="
rhs11 <- 1

const_matrix11 <- rbind(const_matrix, r11)
const_direct11 <- append(const_direct, d11)
const_rhs11 <- append(const_rhs, rhs11)

s11 <- lp(direction = "min",
          objective.in = Z,
          const.mat = const_matrix11,
          const.dir = const_direct11,
          const.rhs = const_rhs11)

s11
s11$solution
# Z = 16.6
# x1 = 2.6
# x2 = 1
# x3 = 1.6

# Necessário resolver os problemas com as seguintes restrições (na próxima camada)
# x3 <= 1 (s1.1.1)
# x3 >= 2 (s1.1.2)

# ------------------------------------------------------------------------------
# Camada 2

# Solução 1.1.2

# Incluir a restrição:
# x3 >= 2

r112 <- c(0,0,1)
d112 <- ">="
rhs112 <- 2

const_matrix112 <- rbind(const_matrix11, r112)
const_direct112 <- append(const_direct11, d112)
const_rhs112 <- append(const_rhs11, rhs112)

s112 <- lp(direction = "min",
          objective.in = Z,
          const.mat = const_matrix112,
          const.dir = const_direct112,
          const.rhs = const_rhs112)

s112
s112$solution
# Z = 16.63636 
# x1 = 2.4545455
# x2 = 0.8181818
# x3 = 2

# Necessário resolver os problemas com as seguintes restrições (na próxima camada)
# x2 = 0 (s1.1.2.1)
# x2 >= 1 (s1.1.2.2)

# ------------------------------------------------------------------------------
# Camada 3

# Solução 1.1.2.2

# Incluir a restrição:
# x2 >= 1

r1122 <- c(0,1,0)
d1122 <- ">="
rhs1122 <- 1

const_matrix1122 <- rbind(const_matrix112, r1122)
const_direct1122 <- append(const_direct112, d1122)
const_rhs1122 <- append(const_rhs112, rhs1122)

s1122 <- lp(direction = "min",
           objective.in = Z,
           const.mat = const_matrix1122,
           const.dir = const_direct1122,
           const.rhs = const_rhs1122)

s1122
s1122$solution
# Z = 17
# x1 = 2.333333
# x2 = 1
# x2 = 2

# Bound
