# 0 - empty
# 2 - first agent type color
# 4 - second agent type color

# initialize simulation
# size      - square size
# perc.full - percentage of lots to be occupied
init <- function(side, perc.full) {
    size <- floor(side ^ 2 * perc.full / 2)
    state <- matrix(0, side, side)
    occupied <- sample(side ^ 2, 2 * size)
    state[occupied] <- c(2,4)
    return(state)
}

# plot simulation state
# state - simulation state
# i     - simulation iteration
do.plot <- function(state, i) {
    side <- dim(state)[1]
    x <- rep(1:side, side)
    y <- rep(1:side, each = side)
    par(fin=c(4,4), fig=c(0,1,0,1))
    plot(x , y, axes = F, xlab="", ylab="", col = state,
         main = paste("Step", i), pch = 19, cex = 40 / side)
}

# perform one step of simulation
# state     - simulation state
# threshold - percent of required agents of the same color
#             in neighborhood
# radius    - neighborhood radius
sim.step <- function(state, threshold, radius) {
    mod.1 <- function(a, b) { 1 + ((a - 1) %% b) }
    div.1 <- function(a, b) { 1 + ((a - 1) %/% b) }
    
    unhappy <- rep(NA, length(state))
    side <- dim(state)[1]
    check <- (-radius):(radius)
    
    #find unhappy agents
    for (n in which(state > 0)) {
        x <- div.1(n, side)
        y <- mod.1(n, side)
        x.radius <- mod.1(check + x, side)
        y.radius <- mod.1(check + y, side)
        region <- state[y.radius, x.radius]
        similar <- sum(region == state[n]) - 1
        total <- sum(region > 0) - 1
        unhappy[n] <- (similar < total * threshold)
    }
    vunhappy <- which(unhappy)
    
    # move unhappy agents
    vunhappy <- vunhappy[sample.int(length(vunhappy))]
    empty <- which(state == 0)
    for (n in vunhappy) {
        move.idx <- sample.int(length(empty), 1)
        state[empty[move.idx]] <- state[n]
        state[n] <- 0
        empty[move.idx] <- n
    }
    return(state)
}

library(animation)

# simple wrapper for animation plotting
go <- function() {
    s <- init(51, 0.75)
    for (i in 1:50) {
        do.plot(s, i)
        last.s <- s
        s <- sim.step(s, 0.6, 1)
        if (identical(last.s, s)) { break }
    }
    for (j in 1:4) {
        do.plot(s, i)
    }
    ani.options(interval = 5 / (i + 2))
}

saveGIF(go())