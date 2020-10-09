library(lpSolve)

# Max Z = 5x1 + 7x2
Z <- c(5,7)

# Sujeito a:
# 12x1 + 7x2 <= 84
# 5x1 + 7x2 >= 35
# x2 <= 7
# x1, x2 >= 0 e inteiros

# ---------------------- Camada 1 ----------------------
sa <- matrix(c(12, 7,
               5, 7,
               0, 1,
               1, 1), ncol = 2, byrow = T)

direct <- c("<=", ">=", "<=", ">=")
limits <- c(84, 35, 7, 0)


# Solução 0
s0 <- lp(direction = "max",
         objective.in = Z,
         const.mat = sa,
         const.dir = direct,
         const.rhs = limits)

s0 
s0$solution
# Z = 63.58333
# x1 = 2.916667
# x2 = 7

# necessário resolver o problema com as seguintes restrições:
# x1 <= 2 (s1)
# x2 >= 3 (s2)

# ---------------------- Camada 2 ----------------------
# Solução 1
# Incluir a restrição:
# x1 <= 2

sa1 <- matrix(c(12, 7,
                5, 7,
                0, 1,
                1, 1,
                1, 0), ncol = 2, byrow = T)

direct1 <- c("<=", ">=", "<=", ">=", "<=")
limits1 <- c(84, 35, 7, 0, 2)

s1 <- lp(direction = "max",
         objective.in = Z,
         const.mat = sa1,
         const.dir = direct1,
         const.rhs = limits1)

s1 
s1$solution
# Z = 59
# x1 = 2
# x2 = 7
# bound!


# Solução 2
# Incluir a restrição:
# x1 >= 3

sa2 <- matrix(c(12, 7,
                5, 7,
                0, 1,
                1, 1,
                1, 0), ncol = 2, byrow = T)

direct2 <- c("<=", ">=", "<=", ">=", ">=")
limits2 <- c(84, 35, 7, 0, 3)

s2 <- lp(direction = "max",
         objective.in = Z,
         const.mat = sa2,
         const.dir = direct2,
         const.rhs = limits2)

s2 
s2$solution
# Z = 63
# x1 = 3
# x2 = 6.857143

# Necessário incluir as restrições:
# x2 <= 6 (s3)
# x2 >= 7 (s4)

# ---------------------- Camada 3 ----------------------

# Solução 3
# Incluir a restrição:
# x2 <= 6

sa3 <- matrix(c(12, 7,
                5, 7,
                0, 1,
                1, 1,
                1, 0,
                0, 1), ncol = 2, byrow = T)

direct3 <- c("<=", ">=", "<=", ">=", ">=", "<=")
limits3 <- c(84, 35, 7, 0, 3, 6)

s3 <- lp(direction = "max",
         objective.in = Z,
         const.mat = sa3,
         const.dir = direct3,
         const.rhs = limits3)

s3 
s3$solution
# Z = 59.5
# x1 = 3.5
# x2 = 6

# Necessário incluir as restrições:
# x1 <= 3 (s5)
# x2 >= 4 (s6)


# Solução 4
# Incluir a restrição:
# x2 >= 7

sa4 <- matrix(c(12, 7,
                5, 7,
                0, 1,
                1, 1,
                1, 0,
                0, 1), ncol = 2, byrow = T)

direct4 <- c("<=", ">=", "<=", ">=", ">=", ">=")
limits4 <- c(84, 35, 7, 0, 3, 7)

s4 <- lp(direction = "max",
         objective.in = Z,
         const.mat = sa4,
         const.dir = direct4,
         const.rhs = limits4)

s4 
s4$solution
# Solução inviável

# ---------------------- Camada 4 ----------------------

# Solução 5
# Incluir a restrição:
# x1 <= 3

sa5 <- matrix(c(12, 7,
                5, 7,
                0, 1,
                1, 1,
                1, 0,
                0, 1,
                1, 0), ncol = 2, byrow = T)

direct5 <- c("<=", ">=", "<=", ">=", ">=", "<=", "<=")
limits5 <- c(84, 35, 7, 0, 3, 6, 3)

s5 <- lp(direction = "max",
         objective.in = Z,
         const.mat = sa5,
         const.dir = direct5,
         const.rhs = limits5)

s5 
s5$solution
# Z = 57
# x1 = 3
# x2 = 6

# A solução 1 apresentou Z melhor


# Solução 6
# Incluir a restrição:
# x1 >= 4

sa6 <- matrix(c(12, 7,
                5, 7,
                0, 1,
                1, 1,
                1, 0,
                0, 1,
                1, 0), ncol = 2, byrow = T)

direct6 <- c("<=", ">=", "<=", ">=", ">=", "<=", ">=")
limits6 <- c(84, 35, 7, 0, 3, 6, 4)

s6 <- lp(direction = "max",
         objective.in = Z,
         const.mat = sa6,
         const.dir = direct6,
         const.rhs = limits6)

s6 
s6$solution
# Z = 57
# x1 = 4
# x2 = 5.142857


# -------------------------------------------------------------
# Escolhida a solução 1:
# Z = 59
# x1 = 2
# x2 = 7





