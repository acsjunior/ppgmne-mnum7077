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


# Solução 1.2

# Incluir a restrição:
# x2 >= 2

r12 <- c(0,1,0)
d12 <- ">="
rhs12 <- 2

const_matrix12 <- rbind(const_matrix, r12)
const_direct12 <- append(const_direct, d12)
const_rhs12 <- append(const_rhs, rhs12)

s12 <- lp(direction = "min",
          objective.in = Z,
          const.mat = const_matrix12,
          const.dir = const_direct12,
          const.rhs = const_rhs12)

s12
s12$solution
# Z = 18.5
# x1 = 3.5
# x2 = 2
# x3 = 0


# Valor de Z pior que o da solução bound



# Solução 1.3

# Incluir a restrição:
# x3 <= 1

r13 <- c(0,0,1)
d13 <- "<="
rhs13 <- 1

const_matrix13 <- rbind(const_matrix, r13)
const_direct13 <- append(const_direct, d13)
const_rhs13 <- append(const_rhs, rhs13)

s13 <- lp(direction = "min",
          objective.in = Z,
          const.mat = const_matrix13,
          const.dir = const_direct13,
          const.rhs = const_rhs13)

s13
s13$solution
# Z = 16.625
# x1 = 2.875
# x2 = 1.250
# x3 = 1

# Necessário resolver os problemas com as seguintes restrições (na próxima camada)
# x2 <= 1 (s1.3.1)
# x2 >= 2 (s1.3.2)


# Solução 1.4

# Incluir a restrição:
# x3 >= 2

r14 <- c(0,0,1)
d14 <- ">="
rhs14 <- 2

const_matrix14 <- rbind(const_matrix, r14)
const_direct14 <- append(const_direct, d14)
const_rhs14 <- append(const_rhs, rhs14)

s14 <- lp(direction = "min",
          objective.in = Z,
          const.mat = const_matrix14,
          const.dir = const_direct14,
          const.rhs = const_rhs14)

s14
s14$solution
# Z = 16.63636
# x1 = 2.4545455
# x2 = 0.8181818
# x3 = 2

# Necessário resolver os problemas com as seguintes restrições (na próxima camada)
# x2 = 0 (s1.4.1)
# x2 >= 1 (s1.4.2)



# ------------------------------------------------------------------------------
# Camada 2


# Solução 1.1.1

# Incluir a restrição:
# x3 <= 1

r111 <- c(0,0,1)
d111 <- "<="
rhs111 <- 1

const_matrix111 <- rbind(const_matrix11, r111)
const_direct111 <- append(const_direct11, d111)
const_rhs111 <- append(const_rhs11, rhs111)

s111 <- lp(direction = "min",
           objective.in = Z,
           const.mat = const_matrix111,
           const.dir = const_direct111,
           const.rhs = const_rhs111)

s111
s111$solution
# Z = 17.5
# x1 = 3.5
# x2 = 1
# x3 = 1

# Valor de Z pior que o da solução bound



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



# Solução 1.3.1

# Incluir a restrição:
# x2 <= 1

r131 <- c(0,1,0)
d131 <- "<="
rhs131 <- 1

const_matrix131 <- rbind(const_matrix13, r131)
const_direct131 <- append(const_direct13, d131)
const_rhs131 <- append(const_rhs13, rhs131)

s131 <- lp(direction = "min",
           objective.in = Z,
           const.mat = const_matrix131,
           const.dir = const_direct131,
           const.rhs = const_rhs131)

s131
s131$solution
# Z = 17.5
# x1 = 3.5
# x2 = 1
# x3 = 1

# Valor de Z pior que o da solução bound



# Solução 1.3.2

# Incluir a restrição:
# x2 >= 2

r132 <- c(0,1,0)
d132 <- ">="
rhs132 <- 2

const_matrix132 <- rbind(const_matrix13, r132)
const_direct132 <- append(const_direct13, d132)
const_rhs132 <- append(const_rhs13, rhs132)

s132 <- lp(direction = "min",
           objective.in = Z,
           const.mat = const_matrix132,
           const.dir = const_direct132,
           const.rhs = const_rhs132)

s132
s132$solution
# Z = 18.5
# x1 = 3.5
# x2 = 2
# x3 = 0

# Valor de Z pior que o da solução bound



# Solução 1.4.1

# Incluir a restrição:
# x2 = 0

r141 <- c(0,1,0)
d141 <- "="
rhs141 <- 0

const_matrix141 <- rbind(const_matrix14, r141)
const_direct141 <- append(const_direct14, d141)
const_rhs141 <- append(const_rhs14, rhs141)

s141 <- lp(direction = "min",
           objective.in = Z,
           const.mat = const_matrix141,
           const.dir = const_direct141,
           const.rhs = const_rhs141)

s141
s141$solution
# Z = 16.8
# x1 = 1.8
# x2 = 0
# x3 = 3.8

# Necessário resolver os problemas com as seguintes restrições (na próxima camada)
# x3 <= 3 (s1.4.1.1)
# x3 >= 4 (s1.4.1.2)



# Solução 1.4.2

# Incluir a restrição:
# x2 >= 1

r142 <- c(0,1,0)
d142 <- ">="
rhs142 <- 1

const_matrix142 <- rbind(const_matrix14, r142)
const_direct142 <- append(const_direct14, d142)
const_rhs142 <- append(const_rhs14, rhs142)

s142 <- lp(direction = "min",
           objective.in = Z,
           const.mat = const_matrix142,
           const.dir = const_direct142,
           const.rhs = const_rhs142)

s142
s142$solution
# Z = 17
# x1 = 2.333333
# x2 = 1
# x2 = 2

# Bound

# ------------------------------------------------------------------------------
# Camada 3


# Solução 1.1.2.1

# Incluir a restrição:
# x2 = 0

r1121 <- c(0,1,0)
d1121 <- "="
rhs1121 <- 0

const_matrix1121 <- rbind(const_matrix112, r1121)
const_direct1121 <- append(const_direct112, d1121)
const_rhs1121 <- append(const_rhs112, rhs1121)

s1121 <- lp(direction = "min",
            objective.in = Z,
            const.mat = const_matrix1121,
            const.dir = const_direct1121,
            const.rhs = const_rhs1121)

s1121
s1121$solution
# Z = 16.8
# x1 = 1.8
# x2 = 0
# x3 = 3.8

# Necessário resolver os problemas com as seguintes restrições (na próxima camada)
# x3 <= 3 (s1.1.2.1.1)
# x3 >= 4 (s1.1.2.1.2)




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


# ------------------------------------------------------------------------------
# Camada 4


# Solução 1.1.2.1.1

# Incluir a restrição:
# x3 <= 3

r11211 <- c(0,0,1)
d11211 <- "<="
rhs11211 <- 3

const_matrix11211 <- rbind(const_matrix1121, r11211)
const_direct11211 <- append(const_direct1121, d11211)
const_rhs11211 <- append(const_rhs1121, rhs11211)

s11211 <- lp(direction = "min",
            objective.in = Z,
            const.mat = const_matrix11211,
            const.dir = const_direct11211,
            const.rhs = const_rhs11211)

s11211
s11211$solution
# Z = 18
# x1 = 3
# x2 = 0
# x3 = 3

# Z pior que o da solução bound


# Solução 1.1.2.1.2

# Incluir a restrição:
# x3 >= 4

r11212 <- c(0,0,1)
d11212 <- ">="
rhs11212 <- 4

const_matrix11212 <- rbind(const_matrix1121, r11212)
const_direct11212 <- append(const_direct1121, d11212)
const_rhs11212 <- append(const_rhs1121, rhs11212)

s11212 <- lp(direction = "min",
             objective.in = Z,
             const.mat = const_matrix11212,
             const.dir = const_direct11212,
             const.rhs = const_rhs11212)

s11212
s11212$solution

# Bound
