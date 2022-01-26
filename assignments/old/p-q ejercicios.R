# Encontrar probabilidad

# Normal 
# pnorm nos ayuda a calcular la probabilidad acumulada de la normal 
pnorm(1.30, lower.tail = T) # 0.9031995
pnorm(1.30, lower.tail = F) # 0.09680048

# Chi-squared
# qchisq nos ayuda calcular la probabilidad acumulada de la chi-squared
pchisq(3.32, df = 9, lower.tail = T) # 0.04975337
pchisq(3.32, df = 9, lower.tail = F) # 0.9502466

# t-student
# t-student nos ayuda calcular la probabilidad acumulada de la t-student
pt(1.30, df = 9, lower.tail = T) # 0.8870468
pt(1.30, df = 9, lower.tail = F) # 0.1129532

# F distribution
pf(2.44, df1 = 9, df2 = 9, lower.tail = T) # 0.8999659
pf(2.44, df1 = 9, df2 = 9, lower.tail = F) # 0.1000341

# Encontrar quantil
# Normal 
# pnorm nos ayuda a calcular la probabilidad acumulada de la normal 
qnorm(0.9031995, lower.tail = T) # 1.3
qnorm(0.09680048, lower.tail = F) # 1.3

# Chi-squared
# qchisq nos ayuda calcular la probabilidad acumulada de la chi-squared
qchisq(0.04975337, df = 9, lower.tail = T) # 3.32
qchisq(0.9502466, df = 9, lower.tail = F) # 3.32

# t-student
# t-student nos ayuda calcular la probabilidad acumulada de la t-student
qt(0.8870468, df = 9, lower.tail = T) # 1.3
qt(0.1129532, df = 9, lower.tail = F) # 1.3

# F distribution
qf(0.8999659, df1 = 9, df2 = 9, lower.tail = T) # 2.44
qf(0.1000341, df1 = 9, df2 = 9, lower.tail = F) # 2.44


