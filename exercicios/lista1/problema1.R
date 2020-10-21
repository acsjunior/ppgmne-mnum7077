library(lpSolve) 

# Max Z = x1 + 5x2 + 9x3 + 5x4
Z = c(1,5,9,5)

# S.a:
# x1 + 3x2 + 9x3 + 6x4 <= 16
# 6x1 + 6x2 + 0 + 7x4 <= 19
# 7x1 + 8x2 + 18x3 + 3x4 <= 44
# x1, x2, x3, x4 >= 0 e inteiras

r <- c(1,3,9,6,
       6,6,0,7,
       7,8,18,3)

const_matrix <- matrix(r, ncol = length(Z), byrow = T)
const_direct <- c("<=", "<=", "<=")
const_rhs <- c(16,19,44)


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

# Solução 0
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

# Necessário resolver os problemas com as seguintes restrições (na próxima camada)
# x2 <= 3 (s1.1)
# x2 >= 4 (s1.2)
# x3 = 0 (s1.3)
# x3 >= 1 (s1.4)

# ------------------------------------------------------------------------------
# Camada 1


# Solução 1.1

# Incluir a restrição:
# x2 <= 3

r11 <- c(0,1,0,0)
d11 <- "<="
rhs11 <- 3

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
# Z = 22
# x1 = 0
# x2 = 3
# x3 = 0.78
# x4 = 0

# Necessário resolver os problemas com as seguintes restrições (na próxima camada)
# x3 = 0 (s1.1.3) ############### altera índice para (s1.1.1)
# x3 >= 1 (s1.1.4) ############# altera índice para (s1.1.2)



# Solução 1.2

# Incluir a restrição:
# x2 >= 4

r12 <- c(0,1,0,0)
d12 <- ">="
rhs12 <- 4

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
# solução inviável



# Solução 1.3

# Incluir a restrição:
# x3 = 0

r13 <- c(0,0,1,0)
d13 <- "="
rhs13 <- 0

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
# Z = 15.83333
# x1 = 0
# x2 = 3.166667
# x3 = 0
# x4 = 0

# O valor já é pior que o da solução ótima.



# Solução 1.4

# Incluir a restrição:
# x3 >= 1

r14 <- c(0,0,1,0)
d14 <- ">="
rhs14 <- 1

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
# Z = 20.66667 
# x1 = 0
# x2 = 2.333333
# x3 = 1
# x4 = 0

# Necessário resolver os problemas com as seguintes restrições (na próxima camada)
# x2 <= 2 (1.4.1)
# x2 >= 3 (1.4.2)


# ------------------------------------------------------------------------------
# Camada 2


# Solução 1.1.3

# Incluir a restrição:
# x3 = 0

r113 <- c(0,0,1,0)
d113 <- "="
rhs113 <- 0

const_matrix113 <- rbind(const_matrix11, r113)
const_direct113 <- append(const_direct11, d113)
const_rhs113 <- append(const_rhs11, rhs113)

s113 <- lp(direction = "max",
           objective.in = Z,
           const.mat = const_matrix113,
           const.dir = const_direct113,
           const.rhs = const_rhs113)

s113
s113$solution
# Z = 15.71429
# x1 = 0
# x2 = 3
# x3 = 0
# x4 = 0.1428571

# O valor é pior que o da solução ótima!


# Solução 1.1.4

# Incluir a restrição:
# x3 >= 1

r114 <- c(0,0,1,0)
d114 <- ">="
rhs114 <- 1

const_matrix114 <- rbind(const_matrix11, r114)
const_direct114 <- append(const_direct11, d114)
const_rhs114 <- append(const_rhs11, rhs114)

s114 <- lp(direction = "max",
           objective.in = Z,
           const.mat = const_matrix114,
           const.dir = const_direct114,
           const.rhs = const_rhs114)

s114
s114$solution
# Z = 20.66667 
# x1 = 0
# x2 = 2.333333
# x3 = 1
# x4 = 0

# Necessário resolver os problemas com as seguintes restrições (na próxima camada)
# x2 <= 2 (s.1.1.4.1)
# x2 >= 3 (s1.1.4.2)



# Solução 1.4.1

# Incluir a restrição:
# x2 <= 2

r141 <- c(0,1,0,0)
d141 <- "<="
rhs141 <- 2

const_matrix141 <- rbind(const_matrix14, r141)
const_direct141 <- append(const_direct14, d141)
const_rhs141 <- append(const_rhs14, rhs141)

s141 <- lp(direction = "max",
          objective.in = Z,
          const.mat = const_matrix141,
          const.dir = const_direct141,
          const.rhs = const_rhs141)

s141
s141$solution
# Z = 20 
# x1 = 0
# x2 = 2
# x3 = 1.111111
# x4 = 0

# Na próxima camada Z será <= 20, portanto, a solução bound irá permanecer como a melhor


# Solução 1.4.2

# Incluir a restrição:
# x2 >= 3

r142 <- c(0,1,0,0)
d142 <- ">="
rhs142 <- 3

const_matrix142 <- rbind(const_matrix14, r142)
const_direct142 <- append(const_direct14, d142)
const_rhs142 <- append(const_rhs14, rhs142)

s142 <- lp(direction = "max",
           objective.in = Z,
           const.mat = const_matrix142,
           const.dir = const_direct142,
           const.rhs = const_rhs142)

s142
s142$solution
# Inviável


# ------------------------------------------------------------------------------
# Camada 3


# Solução 1.1.4.1

# Incluir a restrição:
# x2 <= 2

r1141 <- c(0,1,0,0)
d1141 <- "<="
rhs1141 <- 2

const_matrix1141 <- rbind(const_matrix114, r1141)
const_direct1141 <- append(const_direct114, d1141)
const_rhs1141 <- append(const_rhs114, rhs1141)

s1141 <- lp(direction = "max",
           objective.in = Z,
           const.mat = const_matrix1141,
           const.dir = const_direct1141,
           const.rhs = const_rhs1141)

s1141
s1141$solution
# Z = 20
# x1 = 0
# x2 = 2
# x3 = 1.111111 
# x4 = 0

# Na próxima camada Z será <= 20, portanto, a solução bound irá permanecer como a melhor #########
###########??????????????????###################


# Solução 1.1.4.2

# Incluir a restrição:
# x2 >= 3

r1142 <- c(0,1,0,0)
d1142 <- ">="
rhs1142 <- 3

const_matrix1142 <- rbind(const_matrix114, r1142)
const_direct1142 <- append(const_direct114, d1142)
const_rhs1142 <- append(const_rhs114, rhs1142)

s1142 <- lp(direction = "max",
            objective.in = Z,
            const.mat = const_matrix1142,
            const.dir = const_direct1142,
            const.rhs = const_rhs1142)

s1142
s1142$solution
# Inviável



# ------------------------------------------------------------------------------
# Camada 4


# Solução 1.1.4.1.1

# Incluir a restrição:
# x3 <= 1

r11411 <- c(0,0,1,0)
d11411 <- "<="
rhs11411 <- 1

const_matrix11411 <- rbind(const_matrix1141, r11411)
const_direct11411 <- append(const_direct1141, d11411)
const_rhs11411 <- append(const_rhs1141, rhs11411)

s11411 <- lp(direction = "max",
            objective.in = Z,
            const.mat = const_matrix11411,
            const.dir = const_direct11411,
            const.rhs = const_rhs11411)

s11411
s11411$solution
# Z = 20
# x1 = 1
# x2 = 2
# x3 = 1
# x4 = 0


# Solução 1.1.4.1.2

# Incluir a restrição:
# x3 >= 2

r11412 <- c(0,0,1,0)
d11412 <- ">="
rhs11412 <- 2

const_matrix11412 <- rbind(const_matrix1141, r11412)
const_direct11412 <- append(const_direct1141, d11412)
const_rhs11412 <- append(const_rhs1141, rhs11412)

s11412 <- lp(direction = "max",
             objective.in = Z,
             const.mat = const_matrix11412,
             const.dir = const_direct11412,
             const.rhs = const_rhs11412)

s11412
s11412$solution
# Z = 20
# x1 = 1
# x2 = 2
# x3 = 1
# x4 = 0