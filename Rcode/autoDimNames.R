autoColNames <- function(mat,prefix="col") {
  colnames(mat)=colnames(mat,do.NULL,prefix=prefix)
}
autoRowNames <- function(mat,prefix="row") {
  rownames(mat)=rownames(mat,do.NULL,prefix=prefix)
}
autoDimNames <- function(mat,prefixr="row",prefixc="col") {
  dimanes(mat)=dimnames(mat) = list(rownames(mat, do.NULL = FALSE, prefix = prefixr),
                            colnames(mat, do.NULL = FALSE, prefix = prefixc))
}