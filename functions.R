
brier_score <- function(pred, obs, scale=FALSE){
    ## Calculate Brier score
    bs <- (pred - obs)^2
    ## Max score for null model
    max_bs <- mean((mean(obs) - obs)^2)
    ## scaled Brier score
    bs_scaled <- 1 - (bs / max_bs)
    
    ifelse(scale, return(bs_scaled), return(bs))
}


invlogit <- function(x){
    ## inverse logit transform for binary GLM models
    1/(1 + exp(-x))
}


overfit <- function(){
    ## generate simulated data
    x1 <- runif(100)
    x2 <- runif(100)
    x3 <- runif(100)
    x4 <- runif(100)
    y <- as.factor(rep(c("Safe", "Toxic"), each=50))

    d <- data.frame(y, x1, x2, x3, x4)

    ## fit model
    mod <- rpart(y ~ ., data=d, method="class",
                 control=list(minsplit=20, minbucket=5, xval = 0, maxdepth=4))

    ## training accuracy
    xtabs( ~ y + predict(mod, type="class")) %>%
        diag() %>%
        sum() / 100 ->
        accuracy

    return(accuracy)
}



extract_parms <- function(x){
    ## extract means, variances, number of clusters
    n_clusters <- ncol(x$parameters$mean)
    means  <- as.list(1:n_clusters)
    covs <- as.list(1:n_clusters)

    for (i in 1:n_clusters){
        means[[i]] <- x$parameters$mean[, i]
        covs[[i]] <- x$parameters$variance$sigma[, , i]
    }
    return(list(n_clusters=n_clusters, means=means, covs=covs))
 }


calc_mahalanobis <- function(parms, newdata){
    ## calculate prob of point under each cluster
    ps <- as.list(1:parms$n_clusters)

    ## center data
    for (i in 1:parms$n_clusters){

        ## calc Mahalanobis distance
        ps[[i]]  <- mahalanobis(newdata, parms$means[[i]], parms$covs[[i]])
    }

    ## combind into matrix
    do.call("rbind", ps) %>%
        t() ->
        res
    
    colnames(res) <- paste0("C", 1:parms$n_clusters)
    return(res)
 }
