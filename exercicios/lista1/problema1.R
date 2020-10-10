library(lpSolve) 

# Max Z = x1 + 5x2 + 9x3 + 5x4
Z = c(1,5,9,5)

# S.a:
# x1 + 3x2 + 9x3 + 6x4 <= 16
# 6x1 + 6x2 + 0 + 7x4 <= 19
# 7x1 + 8x2 + 18x3 + 3x4 <= 44
# x1, x2, x3, x4 >= 0

ra <- c(1,3,9,6)
rb <- c(6,6,0,7)
rc <- c(7,8,18,3)
rd <- c(1,0,0,0)
re <- c(0,1,0,0)
rf <- c(0,0,1,0)
rg <- c(0,0,0,1)

const_matrix <- matrix(c(ra,rb,rc,rd,re,rf,rg), ncol = length(Z), byrow = T)
const_direct <- c("<=", "<=", "<=", ">=", ">=", ">=", ">=")
const_rhs <- c(16,19,44,0,0,0,0)


si <- lp(direction = "max",
         objective.in = Z,
         const.mat = const_matrix,
         const.dir = const_direct,
         const.rhs = const_rhs,
         int.vec = 1:4)

si
si$solution

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
# x1 = 0.1666667
# x2 = 3
# x3 = 0.7592593
# x4 = 0

# Necessário resolver os problemas com as seguintes restrições (na próxima camada)
# x1 = 0 (s1.1.1)
# x1 >= 1 (s1.1.2)
# x3 = 0 (s1.1.3)
# x3 >= 1 (s1.1.4)



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


# Solução 1.1.1

# Incluir a restrição:
# x1 = 0

r111 <- c(1,0,0,0)
d111 <- "="
rhs111 <- 0

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
# Z = 22
# x1 = 0
# x2 = 3
# x3 = 0.7777778
# x4 = 0

# Necessário resolver os problemas com as seguintes restrições (na próxima camada)
# x3 = 0 (s1.1.1.1)
# x3 >= 1 (s1.1.1.2)


# Solução 1.1.2

# Incluir a restrição:
# x1 >= 1

r112 <- c(1,0,0,0)
d112 <- ">="
rhs112 <- 1

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
# Z = 20.33333
# x1 = 1
# x2 = 2.1666667
# x3 = 0.9444444
# x4 = 0

# Necessário resolver os problemas com as seguintes restrições (na próxima camada)
# x2 <= 2 (s1.1.2.1)
# x2 >= 3 (s1.1.2.2)
# x3 = 0 (s1.1.2.3)
# x3 >= 1 (s1.2.2.4)


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



# Solução 1.1.1.1

# Incluir a restrição:
# x3 = 0

r1111 <- c(0,0,1,0)
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
# Z = 15.71429
# x1 = 0
# x2 = 3
# x3 = 0
# x4 = 0.1428571

# Valor de Z inferior que o da solução ótima


# Solução 1.1.1.2

# Incluir a restrição:
# x3 >= 1

r1112 <- c(0,0,1,0)
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
# Z = 20.66667
# x1 = 0
# x2 = 2.333333
# x3 = 1
# x4 = 0

# Necessário resolver os problemas com as seguintes restrições (na próxima camada)
# x2 <= 2 (s1.1.1.2.1)
# x2 >= 3 (s1.1.1.2.2)



# Solução 1.1.2.1

# Incluir a restrição:
# x2 <= 2

r1121 <- c(0,1,0,0)
d1121 <- "<="
rhs1121 <- 2

const_matrix1121 <- rbind(const_matrix112, r1121)
const_direct1121 <- append(const_direct112, d1121)
const_rhs1121 <- append(const_rhs112, rhs1121)

s1121 <- lp(direction = "max",
           objective.in = Z,
           const.mat = const_matrix1121,
           const.dir = const_direct1121,
           const.rhs = const_rhs1121)

s1121
s1121$solution
# Z = 20
# x1 = 1.1666667
# x2 = 2
# x3 = 0.9814815
# x4 = 0

# Na próxima camada Z será <= 20, portanto, a solução bound irá permanecer como a melhor



# Solução 1.1.2.2

# Incluir a restrição:
# x2 >= 3

r1122 <- c(0,1,0,0)
d1122 <- ">="
rhs1122 <- 3

const_matrix1122 <- rbind(const_matrix112, r1122)
const_direct1122 <- append(const_direct112, d1122)
const_rhs1122 <- append(const_rhs112, rhs1122)

s1122 <- lp(direction = "max",
            objective.in = Z,
            const.mat = const_matrix1122,
            const.dir = const_direct1122,
            const.rhs = const_rhs1122)

s1122
s1122$solution
# Inviável


# Solução 1.1.2.3

# Incluir a restrição:
# x3 = 0

r1123 <- c(0,0,1,0)
d1123 <- "="
rhs1123 <- 0

const_matrix1123 <- rbind(const_matrix112, r1123)
const_direct1123 <- append(const_direct112, d1123)
const_rhs1123 <- append(const_rhs112, rhs1123)

s1123 <- lp(direction = "max",
            objective.in = Z,
            const.mat = const_matrix1123,
            const.dir = const_direct1123,
            const.rhs = const_rhs1123)

s1123
s1123$solution
# Z = 11.83333
# x1 = 1
# x2 = 2.166667
# x3 = 0
# x4 = 0

# Valor de Z inferior que o da solução ótima


# Solução 1.1.2.4

# Incluir a restrição:
# x3 >= 1

r1124 <- c(0,0,1,0)
d1124 <- ">="
rhs1124 <- 1

const_matrix1124 <- rbind(const_matrix112, r1124)
const_direct1124 <- append(const_direct112, d1124)
const_rhs1124 <- append(const_rhs112, rhs1124)

s1124 <- lp(direction = "max",
            objective.in = Z,
            const.mat = const_matrix1124,
            const.dir = const_direct1124,
            const.rhs = const_rhs1124)

s1124
s1124$solution
# Z = 20
# x1 = 1
# x2 = 2
# x3 = 1
# x4 = 0

# bound!


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

# Na próxima camada Z será <= 20, portanto, a solução bound irá permanecer como a melhor


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


# Solução 1.1.1.2.1

# Incluir a restrição:
# x2 <= 2

r11121 <- c(0,1,0,0)
d11121 <- "<="
rhs11121 <- 2

const_matrix11121 <- rbind(const_matrix1112, r11121)
const_direct11121 <- append(const_direct1112, d11121)
const_rhs11121 <- append(const_rhs1112, rhs11121)

s11121 <- lp(direction = "max",
            objective.in = Z,
            const.mat = const_matrix11121,
            const.dir = const_direct11121,
            const.rhs = const_rhs11121)

s11121
s11121$solution
# Z = 20
# x1 = 0
# x2 = 2
# x3 = 1
# x4 = 0

# Na próxima camada Z será <= 20, portanto, a solução bound irá permanecer como a melhor


# Solução 1.1.1.2.2

# Incluir a restrição:
# x2 >= 3

r11122 <- c(0,1,0,0)
d11122 <- ">="
rhs11122 <- 3

const_matrix11122 <- rbind(const_matrix1112, r11122)
const_direct11122 <- append(const_direct1112, d11122)
const_rhs11122 <- append(const_rhs1112, rhs11122)

s11122 <- lp(direction = "max",
             objective.in = Z,
             const.mat = const_matrix11122,
             const.dir = const_direct11122,
             const.rhs = const_rhs11122)

s11122
s11122$solution
# Inviável
