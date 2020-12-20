library(lpSolve) 

# Max Z = 7x1 + 9x2 + x3 + 6x4
Z = c(7,9,1,6)

# S.a:
# 8x1 + 2x2 + 4x3 + 2x4 <= 16
# 4x1 + 8x2 + 2x3 + 0 <= 20
# 7x1 + 0 + 6x3 + 2x4 <= 11
# x1, x2, x4 >= 0 e inteiras
# x3 >= 0

r <- c(
  8,2,4,2,
  4,8,2,0,
  7,0,6,2)

const_matrix <- matrix(r, ncol = length(Z), byrow = T)
const_direct <- c("<=", "<=", "<=")
const_rhs <- c(16,20,11)


si <- lp(direction = "max",
         objective.in = Z,
         const.mat = const_matrix,
         const.dir = const_direct,
         const.rhs = const_rhs,
         int.vec = c(1,2,4))

si
si$solution
# Z = 48.16667
# x1 = 0
# x2 = 2
# x3 = 0.1666667
# x4 = 5

# ------------------------------------------------------------------------------
# Camada 0

# Solução 0
s0 <- lp(direction = "max",
         objective.in = Z,
         const.mat = const_matrix,
         const.dir = const_direct,
         const.rhs = const_rhs)

s0
s0$solution
# Z = 55.5
# x1 = 0
# x2 = 2.5
# x3 = 0
# x4 = 5.5

# Necessário resolver os problemas com as seguintes restrições (na próxima camada)
# x2 <= 2 (s1.1)
# x2 >= 3 (s1.2)
# x4 <= 5 (s1.3)
# x4 >= 6 (s1.4)


# ------------------------------------------------------------------------------
# Camada 1

# Solução 1.1

# Incluir a restrição:
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
# Z = 51
# x1 = 0
# x2 = 2
# x3 = 0
# x4 = 5.5

# Necessário resolver os problemas com as seguintes restrições (na próxima camada)
# x4 <= 5 (s1.1.1)
# x4 >= 6 (s1.1.2)


# Solução 1.2

# Incluir a restrição:
# x2 >= 3

r12 <- c(0,1,0,0)
d12 <- ">="
rhs12 <- 3

const_matrix12 <- rbind(const_matrix, r12)
const_direct12 <- append(const_direct, d12)
const_rhs12 <- append(const_rhs, rhs12)

s12 <- lp(direction = "max",
          objective.in = Z,
          const.mat = const_matrix12,
          const.dir = const_direct12,
          const.rhs = const_rhs12)

s12
s12$solution
# Inviável




# Solução 1.3

# Incluir a restrição:
# x4 <= 5

r13 <- c(0,0,0,1)
d13 <- "<="
rhs13 <- 5

const_matrix13 <- rbind(const_matrix, r13)
const_direct13 <- append(const_direct, d13)
const_rhs13 <- append(const_rhs, rhs13)

s13 <- lp(direction = "max",
          objective.in = Z,
          const.mat = const_matrix13,
          const.dir = const_direct13,
          const.rhs = const_rhs13)

s13
s13$solution
# Z = 52.85714
# x1 = 0.1428571
# x2 = 2.4285714
# x3 = 0
# x4 = 5

# Necessário resolver os problemas com as seguintes restrições (na próxima camada)
# x1 = 0 (s1.3.1)
# x1 >= 1 (s1.3.2)
# x2 <= 2 (s1.3.3)
# x2 >= 3 (s1.3.4)


# Solução 1.4

# Incluir a restrição:
# x4 >= 6

r14 <- c(0,0,0,1)
d14 <- ">="
rhs14 <- 6

const_matrix14 <- rbind(const_matrix, r14)
const_direct14 <- append(const_direct, d14)
const_rhs14 <- append(const_rhs, rhs14)

s14 <- lp(direction = "max",
          objective.in = Z,
          const.mat = const_matrix14,
          const.dir = const_direct14,
          const.rhs = const_rhs14)

s14
s14$solution
# Inviável



# ------------------------------------------------------------------------------
# Camada 2


# Solução 1.1.1

# Incluir a restrição:
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
# x1 = 0.1428571
# x2 = 2
# x3 = 0
# x4 = 5

# Necessário resolver os problemas com as seguintes restrições (na próxima camada)
# x1 = 0 (s1.1.1.1)
# x1 >= 1 (s1.1.1.2)


# Solução 1.1.2

# Incluir a restrição:
# x4 >= 6

r112 <- c(0,0,0,1)
d112 <- ">="
rhs112 <- 6

const_matrix112 <- rbind(const_matrix11, r112)
const_direct112 <- append(const_direct11, d112)
const_rhs112 <- append(const_rhs11, rhs112)

s112 <- lp(direction = "max",
           objective.in = Z,
           const.mat = const_matrix112,
           const.dir = const_direct112,
           const.rhs = const_rhs112)

s112
s112$solution
# Inviável



# Solução 1.3.1

# Incluir a restrição:
# x1 = 0

r131 <- c(1,0,0,0)
d131 <- "="
rhs131 <- 0

const_matrix131 <- rbind(const_matrix13, r131)
const_direct131 <- append(const_direct13, d131)
const_rhs131 <- append(const_rhs13, rhs131)

s131 <- lp(direction = "max",
           objective.in = Z,
           const.mat = const_matrix131,
           const.dir = const_direct131,
           const.rhs = const_rhs131)

s131
s131$solution
# Z = 52.5
# x1 = 0
# x2 = 2.5
# x3 = 0
# x4 = 5

# Necessário resolver os problemas com as seguintes restrições (na próxima camada)
# x2 <= 2 (s1.3.1.1)
# x2 >= 3 (s1.3.1.2)



# Solução 1.3.2

# Incluir a restrição:
# x1 >= 1

r132 <- c(1,0,0,0)
d132 <- ">="
rhs132 <- 1

const_matrix132 <- rbind(const_matrix13, r132)
const_direct132 <- append(const_direct13, d132)
const_rhs132 <- append(const_rhs13, rhs132)

s132 <- lp(direction = "max",
           objective.in = Z,
           const.mat = const_matrix132,
           const.dir = const_direct132,
           const.rhs = const_rhs132)

s132
s132$solution
# Valor de Z inferior ao da solução bound



# Solução 1.3.3

# Incluir a restrição:
# x2 <= 2

r133 <- c(0,1,0,0)
d133 <- "<="
rhs133 <- 2

const_matrix133 <- rbind(const_matrix13, r133)
const_direct133 <- append(const_direct13, d133)
const_rhs133 <- append(const_rhs13, rhs133)

s133 <- lp(direction = "max",
           objective.in = Z,
           const.mat = const_matrix133,
           const.dir = const_direct133,
           const.rhs = const_rhs133)

s133
s133$solution
# Z = 49
# x1 = 0.1428571
# x2 = 2
# x3 = 0
# x4 = 5

# Necessário resolver os problemas com as seguintes restrições (na próxima camada)
# x1 = 0 (s1.3.3.1)
# x1 >= 1 (s1.3.3.2)


# Solução 1.3.4

# Incluir a restrição:
# x2 >= 3

r134 <- c(0,1,0,0)
d134 <- ">="
rhs134 <- 3

const_matrix134 <- rbind(const_matrix13, r134)
const_direct134 <- append(const_direct13, d134)
const_rhs134 <- append(const_rhs13, rhs134)

s134 <- lp(direction = "max",
           objective.in = Z,
           const.mat = const_matrix134,
           const.dir = const_direct134,
           const.rhs = const_rhs134)

s134
s134$solution
# Inviável


# ------------------------------------------------------------------------------
# Camada 3

# Solução 1.1.1.1

# Incluir a restrição:
# x1 = 0

r1111 <- c(1,0,0,0)
d1111 <- "="
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

# Bound!



# Solução 1.1.1.2

# Incluir a restrição:
# x1 >= 1

r1112 <- c(1,0,0,0)
d1112 <- ">="
rhs1112 <- 1

const_matrix1112 <- rbind(const_matrix111, r1112)
const_direct1112 <- append(const_direct111, d1112)
const_rhs1112 <- append(const_rhs111, rhs1112)

s1112 <- lp(direction = "max",
            objective.in = Z,
            const.mat = const_matrix1112,
            const.dir = const_direct1112,
            const.rhs = const_rhs1112)

s1112
s1112$solution
#Z = 37
# x1 = 1
# x2 = 2
# x3 = 0
# x4 = 2

# Valor de Z inferior ao da solução bound


# Solução 1.3.1.1

# Incluir a restrição:
# x2 <= 2

r1311 <- c(0,1,0,0)
d1311 <- "<="
rhs1311 <- 2

const_matrix1311 <- rbind(const_matrix131, r1311)
const_direct1311 <- append(const_direct131, d1311)
const_rhs1311 <- append(const_rhs131, rhs1311)

s1311 <- lp(direction = "max",
            objective.in = Z,
            const.mat = const_matrix1311,
            const.dir = const_direct1311,
            const.rhs = const_rhs1311)

s1311
s1311$solution
# Z = 48.16667
# x1 = 0
# x2 = 2
# x3 = 0.1666667
# x4 = 5

# Bound


# Solução 1.3.1.2

# Incluir a restrição:
# x2 >= 3

r1312 <- c(0,1,0,0)
d1312 <- ">="
rhs1312 <- 3

const_matrix1312 <- rbind(const_matrix131, r1312)
const_direct1312 <- append(const_direct131, d1312)
const_rhs1312 <- append(const_rhs131, rhs1312)

s1312 <- lp(direction = "max",
            objective.in = Z,
            const.mat = const_matrix1312,
            const.dir = const_direct1312,
            const.rhs = const_rhs1312)

s1312
s1312$solution
# Inviável



# Solução 1.3.3.1

# Incluir a restrição:
# x1 = 0

r1331 <- c(1,0,0,0)
d1331 <- "="
rhs1331 <- 0

const_matrix1331 <- rbind(const_matrix133, r1331)
const_direct1331 <- append(const_direct133, d1331)
const_rhs1331 <- append(const_rhs133, rhs1331)

s1331 <- lp(direction = "max",
            objective.in = Z,
            const.mat = const_matrix1331,
            const.dir = const_direct1331,
            const.rhs = const_rhs1331)

s1331
s1331$solution
# Z = 48.16667
# x1 = 0
# x2 = 2
# x3 = 0.1666667
# x4 = 5

# Bound



# Solução 1.3.3.2

# Incluir a restrição:
# x1 >= 1

r1332 <- c(1,0,0,0)
d1332 <- ">="
rhs1332 <- 1

const_matrix1332 <- rbind(const_matrix133, r1332)
const_direct1332 <- append(const_direct133, d1332)
const_rhs1332 <- append(const_rhs133, rhs1332)

s1332 <- lp(direction = "max",
            objective.in = Z,
            const.mat = const_matrix1332,
            const.dir = const_direct1332,
            const.rhs = const_rhs1332)

s1332
s1332$solution
