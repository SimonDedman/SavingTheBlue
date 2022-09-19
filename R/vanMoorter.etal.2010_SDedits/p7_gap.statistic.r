library(clusterSim) #the package with the original "index.Gap" function #needs rgl: sudo apt install libglu1-mesa-dev freeglut3-dev mesa-common-dev

###modified gap function, it determines for one vector clall the GAP and its corresponding "s"
###see the help for the original function regarding the meaning of the different parameters for the function
###Modifications to the original function:
###     1. it uses k-means as a default (and all other changes are made with k-means in mind)
###     2. it returns values when only one large cluster is made
###     3. instead of calculating GAP-differences, it now returns the original GAP-value
index.Gap.modif <- function(x, clall, reference.distribution = "unif", B = 10,
                            method = "k-means", d = NULL, centrotypes = "centroids")
{
    GAP <- function(X, cl, referenceDistribution, B, method,
                    d, centrotypes) {
        simgap <- function(Xvec) {
            ma <- max(Xvec)
            mi <- min(Xvec)
            Xout <- runif(length(Xvec), min = mi, max = ma)
            return(Xout)
        } #end simgap function
        pcsim <- function(X, d, centrotypes) {
            if (centrotypes == "centroids") {
                Xmm <- apply(X, 2, mean)
            }
            else {
                Xmm <- .medoid(x, d)
            }
            for (k in (1:dim(X)[2])) {
                X[, k] <- X[, k] - Xmm[k]
            }
            ss <- svd(X)
            Xs <- X %*% ss$v
            Xnew <- apply(Xs, 2, simgap)
            Xt <- Xnew %*% t(ss$v)
            for (k in (1:dim(X)[2])) {
                Xt[, k] <- Xt[, k] + Xmm[k]
            }
            return(Xt)
        } #end pcsim function
        if (is.null(dim(x))) {
            dim(x) <- c(length(x), 1)
        }
        ClassNr <- max(cl)
        Wk0 <- 0
        WkB <- matrix(0, 1, B)
        for (bb in (1:B)) { #start bootstrap loop
            if (reference.distribution == "unif")
                Xnew <- apply(X, 2, simgap)
            else if (reference.distribution == "pc")
                Xnew <- pcsim(X, d, centrotypes)
            else stop("Wrong reference distribution type")
            if (bb == 1) {
                pp <- cl
                if (ClassNr == length(cl))
                    pp2 <- 1:ClassNr
                else if (method == "pam")
                    pp2 <- pam(Xnew, ClassNr)$cluster
                else if (method == "k-means") {#it is here that a problem occurs with ClassNr==1
                    if (ClassNr == 1) {pp2 <- rep(1, length(cl))}
                    #                    if (ClassNr>1) {pp2 <- kmeans(Xnew, ClassNr, 100, iter.max=100, algorithm="Lloyd")$cluster}}
                    if (ClassNr > 1) {pp2 <- kmeans(Xnew, ClassNr, iter.max = 100, nstart = 100)$cluster}}
                #                    pp2 <- kmeans(Xnew, ClassNr, 100)$cluster
                else if (method == "diana")
                    pp2 <- cutree(as.hclust(diana(dist(Xnew))),
                                  k = ClassNr)
                else if (method == "single" || method == "complete" ||
                         method == "average" || method == "ward" ||
                         method == "mcquitty" || method == "median" ||
                         method == "centroid")
                    pp2 <- cutree(hclust(dist(Xnew), method = method),
                                  ClassNr)
                else stop("Wrong clustering method")
                if (ClassNr > 1) {
                    for (zz in (1:ClassNr)) {
                        Xuse <- X[pp == zz, ] # subsets pp vector to those which match zz's, i.e. first pick the 1s, then the 2s... Might be only 1 row
                        if (!is.null(nrow(Xuse))) { # if Xuse has null rows ie has dropped to a vector as it only has 1 row, it'll kill the run. If so, don't run it
                            #CAVEAT IF LINE ADDED BY SD 2020.05.08####
                            Wk0 <- Wk0 + # initially 0 then gets larger per iteration of zz, 1:4 (e.g.)
                                sum(diag(var(Xuse))) * # Xuse is 2col numeric df of eg StepLengthBL & TurnAngleRelDeg. Diag var is 2x2 grid of var, function of size of number ranges, especially steplength?
                                #ERROR HERE####
                            # if Xuse is 1 row (i.e. only 1 entry in class), var of those 2 values can be massive & diag increases that further
                            # ClassNr from p&_analysis_suppl_BL.r but feels like this could happen with most files & ClassNr sizes
                            (length(pp[pp == zz]) - 1) / # 1/2/3/4 subset length, -1
                                (dim(X)[1] - ClassNr) # x length - n of classes
                        } # close isnull nrow xuse
                        Xuse2 <- Xnew[pp2 == zz, ] # slightly different Xuse 2col df of similar looking values. if getting Error: cannot allocate vector of size X Gb still, add if else per Xuse

                        if (!is.null(nrow(Xuse2))) { # per above
                            WkB[1, bb] <- WkB[1, bb] + # initially 0, adds value to bb'th entry of 50 (B) loop
                                sum(diag(var(Xuse2))) * # same as above, single number, product of variance of 2 cols
                                (length(pp2[pp2 == zz]) - 1) / # similar to above, subset length
                                (dim(X)[1] - ClassNr) # same as above
                        } # close isnull nrow Xuse2
                    }
                }
                if (ClassNr == 1) {
                    Wk0 <- sum(diag(var(X)))
                    WkB[1, bb] <- sum(diag(var(Xnew)))
                }
            } #end first bootstrap loop
            if (bb > 1) {
                if (ClassNr == length(cl))
                    pp2 <- 1:ClassNr
                else if (method == "pam")
                    pp2 <- pam(Xnew, ClassNr)$cluster
                else if (method == "k-means") {#it is here that a problem occurs with ClassNr==1
                    if (ClassNr == 1) {pp2 <- rep(1, length(cl))}
                    #                    if (ClassNr>1) {pp2 <- kmeans(Xnew, ClassNr, 100, iter.max=100, algorithm="Lloyd")$cluster}}
                    if (ClassNr > 1) {pp2 <- kmeans(Xnew, ClassNr, iter.max = 100, nstart = 100)$cluster}}
                #                    pp2 <- kmeans(Xnew, ClassNr, 100)$cluster
                else if (method == "diana")
                    pp2 <- cutree(as.hclust(diana(dist(Xnew))),
                                  k = ClassNr)
                else if (method == "single" || method == "complete" ||
                         method == "average" || method == "ward" ||
                         method == "mcquitty" || method == "median" ||
                         method == "centroid")
                    pp2 <- cutree(hclust(dist(Xnew), method = method),
                                  ClassNr)
                else stop("Wrong clustering method")
                if (ClassNr > 1) {
                    for (zz in (1:ClassNr)) {
                        Xuse2 <- Xnew[pp2 == zz, ]
                        #CAUSES ERROR####
                        #Error: cannot allocate vector of size 108.5 Gb
                        if (!is.null(nrow(Xuse2))) { # per above
                            WkB[1, bb] <- WkB[1, bb] +
                                sum(diag(var(Xuse2))) *
                                length(pp2[pp2 == zz]) /
                                (dim(X)[1] - ClassNr)
                        } # close isnull nrow Xuse2
                    }
                }
                if (ClassNr == 1) {
                    WkB[1, bb] <- sum(diag(var(Xnew)))
                }
            }
        } #end bootstrap loop
        Sgap <- mean(log(WkB[1, ])) - log(Wk0)
        Sdgap <- sqrt(1 + 1/B) * sqrt(var(log(WkB[1, ]))) * sqrt((B -
                                                                      1)/B)
        resul <- list(Sgap = Sgap, Sdgap = Sdgap, Wo = log(Wk0), We = mean(log(WkB[1, ])))
        resul
    } #end GAP function
    if (sum(c("centroids", "medoids") == centrotypes) == 0)
        stop("Wrong centrotypes argument")
    if ("medoids" == centrotypes && is.null(d))
        stop("For argument centrotypes = 'medoids' d cannot be null")
    if (!is.null(d)) {
        if (!is.matrix(d)) {
            d <- as.matrix(d)
        }
        row.names(d) <- row.names(x)
    }
    X <- as.matrix(x)
    gap1 <- GAP(X, clall[, 1], reference.distribution, B, method,
                d, centrotypes)
    gap <- gap1$Sgap
    #   gap2 <- GAP(X, clall[, 2], reference.distribution, B, method,
    #       d, centrotypes)
    #   diffu <- gap - (gap2$Sgap - gap2$Sdgap)
    #   resul <- list(gap = gap, diffu = diffu)
    resul <- list(gap = gap, s = gap1$Sdgap, Wo = gap1$Wo, We = gap1$We)
    resul
}
