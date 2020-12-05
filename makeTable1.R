#! make table 1 function !#
makeTable1 <- function(variables,
                       data,
                       strata = NULL,
                       overall = FALSE,
                       overall.name = "Overall",
                       overall.first = FALSE,
                       quan.stats = c("mean", "sd"),
                       quan.digits = 2,
                       qual.digits = 1,
                       mi = FALSE,
                       impvar = ".imp",
                       idvar = ".id",
                       remove.impvars = TRUE,
                       ignore.vars = NULL) {
    quan.vars <- variables[sapply(X = variables,
                                 FUN = function(x) !is.factor(data[, x]))]
    qual.vars <- variables[sapply(X = variables,
                                  FUN = function(x) is.factor(data[, x]))]
    functions <- c(
        n = function(x) {
            length(x)
        },
        mean = function(x) {
            mean(x)
        },
        sd = function(x) {
            sd(x)
        },
        median = function(x) {
            median(x)
        },
        iqr = function(x) {
            quantile(x)[c(2,4)]
        },
        range = function(x) {
            range(x)
        }
    )
    fmt <- function(x, digits) {
        return(paste(sapply(x,
                            round,
                            digits = quan.digits),
                     collapse = "-"))
    }
    mp <- function(x, digits) {
        return(round(x * 100,
                     digits = digits))
    }
    get.np <- function (level,
                        mi,
                        functions,
                        y,
                        data,
                        impvar = NULL,
                        digits) {
        if (!mi) {
            n <- functions[["n"]](y[y == level])
            np <- paste(n,
                        " (",
                        mp(n/nrow(data), digits),
                        ")",
                        sep = "")
        } else {
            i <- data[, impvar]
            n <- sapply(levels(i), function(x) {
                new.y <- y[i == x]
                return(functions[["n"]](new.y[new.y == level]))

            })
            p <- mp(n/(length(i)/length(levels(i))), digits)
            np <- paste(round(median(n), digits),
                        " (",
                        round(median(p), digits),
                        ")",
                        sep = "")
        }
        return(np)
    }
    if (!is.null(strata)) {
        mdfl <- function(strata, data) {
            dfl <- list()
            levels <- levels(as.factor(data[, strata]))
            levels.length <- length(levels)
            for (i in 1:levels.length) {
                level <- levels[i]
                dfl[[level]] <- data[data[, strata] == level, ]
            }
            return(dfl)
        }
        dfl <- mdfl(strata, data)
        variables <- variables[- which(variables == strata)]
    } else {
        dfl <- NULL
    }
    if (is.null(dfl) | overall) dfl[[overall.name]] <- data
    if (mi) variables <- variables[-sapply(c(impvar, idvar), function(x) which(variables == x))]
    if (!is.null(ignore.vars)) variables <- variables[-sapply(c(ignore.vars), function(x) which(variables == x))]
    variables.length <- length(variables)
    dfs.names <- names(dfl)
    dfs.length <- length(dfs.names)
    table.list <- list()
    for (d in 1:dfs.length) {
        data <- dfl[[d]]
        to.table.list <- list()
        for (i in 1:variables.length) {
            variable <- variables[i]
            y <- data[, variable]
            if (variable %in% quan.vars) {
                col2 <- sapply(quan.stats,
                               function(x) {
                                   fmt(functions[[x]](y), digits = quan.digits)
                               })
                list <- lapply(list(quan.stats, col2),
                               function(x) {
                                   paste(x[1],
                                         " (",
                                         paste(x[-1],
                                               collapse = ", "),
                                         ")",
                                         sep = "")}); list
                row.name <- paste(variable,
                                  ", ",
                                  list[1],
                                  sep = "")
                to.table.list[[row.name]] <- list[[2]]
            } else if (variable %in% qual.vars) {
                row.name <- paste(variable,
                                  ", n (%)",
                                  sep = "")
                to.table.list[[row.name]] <- ""
                levels <- levels(y)
                list <- lapply(levels,
                               get.np,
                               mi,
                               functions,
                               y,
                               data,
                               impvar,
                               qual.digits)
                names(list) <- levels
                to.table.list <- c(to.table.list, list)
            }
        }
        table.list[[dfs.names[d]]] <- to.table.list
    }
    return (do.call(cbind, table.list))
}
#! end
