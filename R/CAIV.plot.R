CAIV.plot <-
    function (obj, x = 1, y = 2, add.row = TRUE, add.col = TRUE,
              add.var = FALSE, row.names = "", col.names = "",
              var.names = "")
{
    opar <- par(mai = par("mai"))
    on.exit(par(opar))
    par(mai = c(1, 1, 0.5, 0.5))
    xmin <- min(obj$R[, x], obj$B[, x], obj$F[, x])
    xmax <- max(obj$R[, x], obj$B[, x], obj$F[, x])
    ymin <- min(obj$R[, y], obj$B[, y], obj$F[, y])
    ymax <- max(obj$R[, y], obj$B[, y], obj$F[, y])
    xet <- c(1.1 * xmin, 1.1 * xmax)
    yet <- c(1.1 * ymin, 1.1 * ymax)
    plot(obj$R[, x], obj$R[, y], type = "n", asp = 1, xlim = xet,
         ylim = yet, xlab = paste("A", x), ylab = paste("A", y),
         font.lab = 2)
    if (add.row == TRUE & row.names == "")
        text(obj$R[, x], obj$R[, y], paste("R", 1:length(obj$R[, x])),
             cex = 0.75, col = "blue")
    if (add.row == TRUE & row.names != "")
        text(obj$R[, x], obj$R[, y], row.names, cex = 0.75, col = "blue")
    if (add.col == TRUE & col.names == "")
        text(obj$F[, x], obj$F[, y], paste("C", 1:length(obj$F[, x])),
             cex = 0.75, col = "red")
    if (add.col == TRUE & col.names != "")
        text(obj$F[, x], obj$F[, y], col.names, cex = 0.75, col = "red")
    if (add.var == TRUE & var.names == "") {
        text(obj$B[, x], obj$B[, y], paste("Var", 1:length(obj$B[, x])),
             cex = 0.75, pos = 3)
        arrows(0, 0, obj$B[, x], obj$B[, y], length = 0.1)
    }
    if (add.var == TRUE & var.names != "") {
        text(obj$B[, x], obj$B[, y], var.names, cex = 0.75, pos = 3)
        arrows(0, 0, obj$B[, x], obj$B[, y], length = 0.1)
    }
}
