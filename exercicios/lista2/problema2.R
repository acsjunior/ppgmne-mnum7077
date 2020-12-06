library(lpSolve)

Z <- c(7,9,1,6)

r <- c(8,2,4,2,
       4,8,2,0,
       7,0,6,2)

const_matrix <- matrix(r, ncol = length(Z), byrow = T)
const_direct <- c("<=", "<=", "<=")
const_rhs <- c(16, 20, 11)

si <- lp(direction = "max",
         objective.in = Z,
         const.mat = const_matrix,
         const.dir = const_direct,
         const.rhs = const_rhs,
         int.vec = c(1,2,4))


si
si$solution
# 48.16667
# x1 = 0
# x2 = 2
# x3 = 0.1666667
# x4 = 5


# ------------------------------------------------------------------------------

# Solução 0

s1 <- lp(direction = "max",
         objective.in = Z,
         const.mat = const_matrix,
         const.dir = const_direct,
         const.rhs = const_rhs)


s1
s1$solution
# Z = 55.5
# x1 = 0
# x2 = 2.5
# x3 = 0
# x4 = 5.5




# Solução 0 com variáveis de folga


Z <- c(7,9,1,6,0,0,0)

r <- c(8,2,4,2,1,0,0,
       4,8,2,0,0,1,0,
       7,0,6,2,0,0,1)


const_matrix <- matrix(r, ncol = length(Z), byrow = T)
const_direct <- c("=", "=", "=")
const_rhs <- c(16, 20, 11)


s1 <- lp(direction = "max",
         objective.in = Z,
         const.mat = const_matrix,
         const.dir = const_direct,
         const.rhs = const_rhs)


s1
s1$solution
# Z = 55.5
# x1 = 0
# x2 = 2.5
# x3 = 0
# x4 = 5.5



# Solução corte 1:

Z <- c(7,9,1,6,0,0,0,0)

r <- c(8,2,4,2,1,0,0,0,
       4,8,2,0,0,1,0,0,
       7,0,6,2,0,0,1,0,
       0.5, 0, 0.25, 0, 0, 0.125, 0, -1)


const_matrix <- matrix(r, ncol = length(Z), byrow = T)
const_direct <- c("=", "=", "=", "=")
const_rhs <- c(16, 20, 11, 0.5)


s1 <- lp(direction = "max",
         objective.in = Z,
         const.mat = const_matrix,
         const.dir = const_direct,
         const.rhs = const_rhs)


s1
s1$solution
# Z = 51
# x1 = 0
# x2 = 2
# x3 = 0
# x4 = 5.5





# Solução corte 1 + corte 2:

Z <- c(7,9,1,6,0,0,0,0)

r <- c(8,2,4,2,1,0,0,0,
       4,8,2,0,0,1,0,0,
       7,0,6,2,0,0,1,0,
       0.5, 0, 0.25, 0, 0, 0.125, 0, -1)


const_matrix <- matrix(r, ncol = length(Z), byrow = T)
const_direct <- c("=", "=", "=", "=")
const_rhs <- c(16, 20, 11, 0.5)


s1 <- lp(direction = "max",
         objective.in = Z,
         const.mat = const_matrix,
         const.dir = const_direct,
         const.rhs = const_rhs)


s1
s1$solution
# Z = 51
# x1 = 0
# x2 = 2
# x3 = 0
# x4 = 5.5






# ------------------------------------------------------------------------------
# Resolução por B&B
# ------------------------------------------------------------------------------

# Camada 0

s1 <- lp(direction = "max",
         objective.in = Z,
         const.mat = const_matrix,
         const.dir = const_direct,
         const.rhs = const_rhs)

s1
s1$solution
# Z = 55.5
# x1 = 0
# x2 = 2.5
# x3 = 0
# x4 = 5.5

# Incluir as seguintes restrições nas próximas iterações:
# x2 <= 2 (s1.1)
# x2 >= 3 (s1.2)
# x4 <= 5 (s1.3)
# x4 >= 6 (s1.4)


# ------------------------------------------------------------------------------

# Camada 1

# Solução 1.1
# x2 <= 2

r11 <- c(0,1,0,0)
d11 <- "<="
rhs11 <- 2

const_matrix11 <- rbind(const_matrix, r11)
const_direct11 <- append(const_direct, d11)
const_rhs11 <- append(const_rhs, rhs11)

s11 <- lp(direction = "max",
         objective.in = Z,
         const.mat = const_matrix11,
         const.dir = const_direct11,
         const.rhs = const_rhs11)

s11
s11$solution
# Z = 5.5
# x1 = 0
# x2 = 2
# x3 = 0
# x4 = 5.5

# Incluir as seguintes restrições nas próximas iterações:
# x4 <= 5 (s1.1.1)
# x4 >= 6 (s1.1.2)


# ------------------------------------------------------------------------------

# Camada 2

# Solução 1.1.1
# x4 <= 5

r111 <- c(0,0,0,1)
d111 <- "<="
rhs111 <- 5

const_matrix111 <- rbind(const_matrix11, r111)
const_direct111 <- append(const_direct11, d111)
const_rhs111 <- append(const_rhs11, rhs111)

s111 <- lp(direction = "max",
          objective.in = Z,
          const.mat = const_matrix111,
          const.dir = const_direct111,
          const.rhs = const_rhs111)

s111
s111$solution
# Z = 49
# x1 0.1428571
# x2 = 2
# x3 = 0
# x4 = 5


# Incluir as seguintes restrições nas próximas iterações:
# x1 <= 0 (s1.1.1.1)
# x1 >= 1 (s1.1.1.2)


# ------------------------------------------------------------------------------

# Camada 3

# Solução 1.1.1.1
# x1 <= 0

r1111 <- c(1,0,0,0)
d1111 <- "<="
rhs1111 <- 0

const_matrix1111 <- rbind(const_matrix111, r1111)
const_direct1111 <- append(const_direct111, d1111)
const_rhs1111 <- append(const_rhs111, rhs1111)

s1111 <- lp(direction = "max",
           objective.in = Z,
           const.mat = const_matrix1111,
           const.dir = const_direct1111,
           const.rhs = const_rhs1111)

s1111
s1111$solution
# Z = 48.16667
# x1 = 0
# x2 = 2
# x3 = 0.1666667
# x4 = 5



# ------------------------------------------------------------------------------

# Solução 0 com variáveis de folga

Z = c(7,9,1,6,0,0,0,0)

r = c(8,2,4,2,1,0,0,0,
      4,8,2,0,0,1,0,0,
      7,0,6,2,0,0,1,0,
      0.5, 0, 0.25, 0, 0, 0.125, 0, -1)

const_matrix <- matrix(r, ncol = length(Z), byrow = T)
const_direct <- c("=", "=", "=", "=")
const_rhs <- c(16, 20, 11, 0.5)

s0 <- lp(direction = "max",
         objective.in = Z,
         const.mat = const_matrix,
         const.dir = const_direct,
         const.rhs = const_rhs)

s0
s0$solution







