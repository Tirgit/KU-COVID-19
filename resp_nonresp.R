
# LIFELINES
# education
x <- c(20949,23550,23536,35291,15515,29964)
tbl <- as.table(t(matrix(x,ncol=3)))
chisq.test(tbl) 

# sex
x <- c(23950,39438,37290,52050)
tbl <- as.table(t(matrix(x,ncol=2)))
chisq.test(tbl) 


# CONSTANCES
# education
x <- c(13373,3811,15249,4750,10671,4130)
tbl <- as.table(t(matrix(x,ncol=3)))
chisq.test(tbl) 

# sex
x <- c(19429,6597,20259,6271)
tbl <- as.table(t(matrix(x,ncol=2)))
chisq.test(tbl) 


# TEMPO
# education
x <- c(342,159,310,219,76,88)
tbl <- as.table(t(matrix(x,ncol=3)))
chisq.test(tbl) 

# sex
x <- c(257,184,472,284)
tbl <- as.table(t(matrix(x,ncol=2)))
chisq.test(tbl) 


