
library(nFactors)

ddd <- dplyr::select(df, dm_cog_eng, dm_beh_eng, dm_aff_eng)
ddd <- ddd[complete.cases(ddd), ]
ev <- eigen(cor(ddd)) # get eigenvalues
ap <- parallel(subject=nrow(ddd),var=ncol(ddd),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

library(psych)
fit <- fa(as.matrix(ddd), nfactors=3, rotate = "oblimin")
fit # print results