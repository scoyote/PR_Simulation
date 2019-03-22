library(ggplot2)

skillA_range <- 10
skillB_range <- 10
dicesides <- 10
simulateEncounters <- 50
simulateBattle <- 1000

df <- data.frame(encounter=integer(),battle=integer(),
                 player1_skillA=integer(),player2_skillA=integer(),
                 player1_skillB=integer(),player2_skillB=integer(),
                 player1_score=integer(),player2_score=integer(),
                 stringsAsFactors=FALSE)

for (encounter in 1:simulateEncounters){
  player1_skillA <- floor(runif(1,1,skillA_range))
  player1_skillB <- floor(runif(1,1,skillB_range))
  player2_skillA <- floor(runif(1,1,skillA_range))
  player2_skillB <- floor(runif(1,1,skillB_range))
  for (battle in 1:simulateBattle){
    player1_diceroll <- floor(runif(1,1,dicesides)) + player1_skillA + player1_skillB
    player2_diceroll <- floor(runif(1,1,dicesides)) + player2_skillA + player2_skillB
    df <- rbind(df,data.frame(encounter,battle,player1_skillA,player1_skillB,battle,player2_skillA,player2_skillB,player1_diceroll,player2_diceroll))
  }
}

df_f <- data.frame(factor(df$encounter),factor(df$battle)
                   ,factor(df$player1_skillA),factor(df$player2_skillA)
                   ,factor(df$player1_skillB),factor(df$player2_skillB)
                   ,df$player1_diceroll,df$player2_diceroll)
colnames(df_f) <- c('enc','bat','p1A','p2A','p1B','p2B','scoreA','scoreB')
df_f <- transform(df_f,result = scoreB-scoreA, setup=paste("A(",p1A,",",p1B,") B(",p2A,",",p2B,")",sep=""))
#cyl_table <- table(mtcars$cyl)
#cyl_levels <- names(cyl_table)[order(cyl_table)]
#mtcars$cyl2 <- factor(mtcars$cyl, levels = cyl_levels)
#mtcars$cyl3 <- with(mtcars, reorder(cyl, cyl, function(x) -length(x)))
#ggplot(mtcars, aes(cyl3)) + geom_bar()

scores_mean <- aggregate(list(df_f$result), 
                         by=list(df_f$setup), 
                         FUN=mean)
colnames(scores_mean) <- c('setup','meanscore')
so <- names(scores_mean)[order(df_f[,-1])]
so <- order(scores_mean$meanscore)

ggplot(data=df_f) + geom_boxplot(aes(factor(setup,levels=scores_mean[so,1], order=T),result)) +
  ylab("PlayerB-PlayerA: Score Neg=A wins, Pos=B wins, ~0 = toss up") +
  xlab("Battle Setup Player A(skill1,skill2) B(skill1, skill2)") +
  geom_hline(yintercept =  0,  color='blue') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 







scores_mean <- aggregate(list(df_f$scoreA,df_f$scoreB), 
                    by=list(df_f$enc,df_f$p1A,df_f$p2A,df_f$p1B,df_f$p2B), 
                    FUN=mean)
colnames(scores_mean) <- c('enc','p1a','p2a','p1b','p2b','p1s_mean','p2s_mean')
scores_sd <- aggregate(list(df_f$scoreA,df_f$scoreB), 
                    by=list(df_f$enc,df_f$p1A,df_f$p2A,df_f$p1B,df_f$p2B), 
                    FUN=sd)
colnames(scores_sd) <- c('enc','p1a','p2a','p1b','p2b','p1s_sd','p2s_sd')

scores <- cbind(scores_mean,scores_sd$p1s_sd,scores_sd$p2s_sd)

colnames(scores) <- c('enc','p1a','p2a','p1b','p2b','p1s_mean','p2s_mean','p1s_sd','p2s_sd')

scores <- transform(scores,p1 = p1a+p1b, p2=p2a+p2b)

ggplot(data=scores) + geom_point(aes(x=p1a, y=p2a))
