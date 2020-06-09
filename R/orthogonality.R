#' @keywords internal
is_orthogonal_mat <- function(mat) {
    # is_orthogonal_mat_vec(mat[,2:ncol(mat), drop=FALSE], mat[1,])
    result <- unlist(sapply(1:(ncol(mat)-1), function(col1) {
        sapply((col1+1):ncol(mat), function(col2) {
            is_orthogonal(mat[,col1], mat[,col2])
        })
    }))
    all(result)
}

#' @keywords internal
is_orthogonal <- function(v, u) {
    all.equal(sum(v*u), 1e-8) == TRUE
}

#' @keywords internal
is_orthogonal_mat_vec <- function(mat, v) {
    all(apply(mat, 2, is_orthogonal, v))
}

#' @keywords internal
orth_compl_proj <- function(mat, v) {
    v - orth_proj(mat, v)
}

#' @keywords internal
orth_proj <- function(mat, v) {
    proj_single <- function(t_i, s_i) {
        sum(t_i*s_i) / sum(t_i*t_i) * t_i
    }
    rowSums(apply(mat, 2, proj_single, v))
}

#' @keywords internal
normalize_vec <- function(v) {
    v / sqrt(sum(v^2))
}

#' @keywords internal
create_base <- function(mat) {
    if (is.vector(mat)) {
        mat <- matrix(mat, nrow=length(mat))
    }

    base <- diag(nrow(mat))

    needed <- nrow(mat) - ncol(mat)

    index <- 1
    while(Matrix::rankMatrix(mat) < nrow(mat)) {
        v <- base[,index, drop=FALSE]
        if (Matrix::rankMatrix(cbind(mat, v)) > Matrix::rankMatrix(mat)) {
            mat <- cbind(mat, v)
        }
        index <- index + 1
    }
    # candidates <- which(apply(base, 2, function(v, mat) Matrix::rankMatrix(cbind(mat, v)) == ncol(mat) + 1, mat))
    # mat <- cbind(mat, base[, candidates[1:needed]])

    return(mat)
}

#' @keywords internal
gram_schmidt <- function(mat) {
    for (t in 2:ncol(mat)) {
        mat[,t] <- orth_compl_proj(mat[, 1:(t-1), drop=FALSE], mat[,t])
    }
    dimnames(mat) <- NULL
    return(mat)
}

#' @keywords internal
pairwise_orthogonal <- function(mat) {
    sapply(1:ncol(mat),
           function(x) sapply(1:ncol(mat),
                              function(a,b) is_orthogonal(mat[,a], mat[,b]),
                              x)
           )
}

#' @keywords internal
fill_C_matrix_cols <- function(mat) {
    mat_new <- create_base(mat)
    mat_new <- gram_schmidt(mat_new)
    mat_new[,1:ncol(mat)] <- mat
    return(mat_new)
}

#' @keywords internal
fill_C_matrix_rows <- function(mat) {
    mat <- t(mat)
    mat <- fill_C_matrix_cols(mat)
    mat <- t(mat)
    return(mat)
}

# C_orig <- matrix(c(1,0,
#                    1,1,
#                    1,1,
#                    0,1),
#                  nrow = 4, byrow=T)

# round(fill_C_matrix_cols(C_orig),3)

