CAIV <- function (L, E = diag(1, dim(L)[1], dim(L)[1]), normE = TRUE)
{
    if (dim(L)[1] != dim(E)[1])
        return("Error in matrix dimension (row number)")
    for (i in 1:dim(L)[1]) for (j in 1:dim(L)[2]) if (L[i, j] <
                                                      0)
        return("Table L must contain non-negative numbers")
    res <- alist()
    Di <- apply(L, 1, sum)
    Di <- diag(Di/(sum(L)), dim(L)[1], dim(L)[1])
    if (sum(diag(Di) == 0) > 0)
        return("Error : row total=0")
    Dj <- apply(L, 2, sum)
    Dj <- diag(Dj/sum(L), dim(L)[2], dim(L)[2])
    if (sum(diag(Dj) == 0) > 0)
        return("Error : column total=0")
    DjInv <- diag((1/diag(Dj)), dim(L)[2], dim(L)[2])
    Centr <- function(Tab, Weights) {
        return(Tab - sum(Weights %*% Tab))
    }
    Ecentr <- apply(E, 2, Centr, Weights = Di)
    Norm <- function(Tab, Weights) {
        return(Tab/sqrt(t(Tab) %*% (Weights) %*% Tab))
    }
    if (normE == TRUE)
        Ecentr <- apply(Ecentr, 2, Norm, Weights = Di)
    T0 <- DjInv %*% (t(L)/sum(L)) %*% Ecentr
    covE <- t(Ecentr) %*% Di %*% Ecentr
    Esvd <- svd(covE)
    qrE <- qr(covE)
    rankE <- qrE$rank
    SemiCovEInv <- Esvd$u[, 1:rankE] %*% diag(Esvd$d[1:rankE]^(-0.5),
                                              rankE, rankE)
    triplet <- svd(t(SemiCovEInv) %*% t(T0) %*% Dj %*% T0 %*%
                   SemiCovEInv)
    res$ev <- triplet$d
    res$B <- SemiCovEInv %*% triplet$u
    res$D <- Esvd$u[, 1:rankE] %*% diag(Esvd$d[1:rankE]^(0.5),
                                        rankE, rankE) %*% triplet$u
    res$R <- Ecentr %*% res$B
    res$F <- T0 %*% res$B
    if (dim(E)[2] == dim(L)[1]) {
        if (sum(E == diag(1, dim(L)[1], dim(L)[1])) == dim(L)[1] *
            dim(L)[1])
            return(res[c("ev", "R", "F")])
    }
    return(res)
}
