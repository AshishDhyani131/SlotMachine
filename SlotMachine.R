getSymbols <- function(){
    wheel <- c("DD","7","BBB","BB","B","C","0")
    sample(wheel,size = 3,replace =TRUE,prob = c(0.03,0.03,0.06,0.1,0.25,0.01,0.52))
}
PayOutRateCalculator <- function(){
    wheel <- c("DD","7","BBB","BB","B","C","0")
    wheelroll <- expand.grid(wheel,wheel,wheel,stringsAsFactors = FALSE)
    probabilities <- c("DD"=0.03,"7"=0.03,"BBB"=0.06,"BB"=0.1,"B"=0.25,"C"=0.01,"0"=0.52)
    wheelroll$prob1 = probabilities[wheelroll$Var1]
    wheelroll$prob2 <- probabilities[wheelroll$Var2]
    wheelroll$prob3 <- probabilities[wheelroll$Var3]
    wheelroll$prob  <- wheelroll$prob1*wheelroll$prob2*wheelroll$prob3
    wheelroll$prize <- NA
    for(t in 1:nrow(wheelroll)){
        symbols <- c(wheelroll[t,1],wheelroll[t,2],wheelroll[t,3])
        wheelroll$prize[t] <- score(symbols)
    }
    sum(wheelroll$prize*wheelroll$prob)
    
}
wildCardSwap <- function(symbols,diamonds){
    cherries <- sum(symbols == "C")
    if(diamonds != 0 & cherries != 0){
        symbols[symbols == "DD"] <- "C"
    }
    else if(diamonds == 1){
        symbols[symbols == "DD"]<- symbols[1]
        symbols[1] <- symbols[2]
    }
    else if(diamonds ==2){
        symbols[symbols == "DD"] <- symbols[symbols != "DD"]
    }
    #print(symbols)
    symbols
}
score<-function(symbols){
    #Calculate prize
    diamonds <- sum(symbols == "DD")
    symbols <- wildCardSwap(symbols,diamonds)
    if(all(symbols == symbols[1])){
        symbol <- symbols[1]
        payouts <- c("DD"=100,"7"=80,"BBB"=40,"BB"=25,"B"=10,"C"=10,"0"=0)
        prize <- unname(payouts[symbol])
    }
    else if(all(symbols %in%c("B","BB","BBB"))){
        prize <- 5
    }
    else{
       cherries <- sum(symbols =="C")#calculate the no of cherries present in symbols
       cherries <- cherries + 1
       prize <- c(0,2,5)[cherries]
    }
    # Calculate prize after counting number of diamonds

    prize*2^(diamonds)
    
    
    # diamonds <- sum(symbols == "DD")
    # cherries <- sum(symbols == "C")
    # # identify case  # since diamonds are wild, only nondiamonds  # matter for three of a kind and all bars
    # slots <- symbols[symbols != "DD"]
    # same <- length(unique(slots)) == 1
    # bars <- slots %in% c("B", "BB", "BBB")
    # # assign prize
    # if (diamonds == 3) {
    #     prize <- 100
    # }
    # else if (same) {
    #     payouts <- c("7" = 80, "BBB" = 40, "BB" = 25,"B" = 10, "C" = 10, "0" = 0)
    #     prize <- unname(payouts[slots[1]])
    # }
    # else if (all(bars)) {
    #     prize <- 5
    # }
    # else if (cherries > 0) {
    #     # diamonds count as cherries
    #     # so long as there is one real cherry
    #     prize <- c(0, 2, 5)[cherries + diamonds + 1]
    # }
    # else {
    #     prize <- 0
    # }
    # # double for each diamond
    # prize * 2^diamonds
}
print.slots <- function(x,...){
    slot_display(x)
}
slot_display <- function(prize){
    #Extract symbols attribute
    symbols <- attr(prize,"symbols")
    symbols <- paste(symbols ,collapse = " ")
    string <- paste(symbols,prize,sep = "\n$")
    cat(string)
}
play<-function(){
    symbols <- getSymbols()
    structure(score(symbols),symbols = symbols,class = "slots" )
}