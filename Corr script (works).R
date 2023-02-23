r=read.csv("HK (Actg1, Actb normalised) Maldi 18mo data.csv")
row.names(r)=r$TargetName
r<- r[,-1]
r=r[,-18]
dim(r)
r2=t(r)
score=read.csv("Plaque Age.csv")
data1=cbind(score, r2)

zx=apply(data1[, -1], 2,cor.test , data1$Age, method="pearson")

corrFunc <- function(var1, var2, data1) {
  result = cor.test(data1[,var1], data1[,var2])
  data.frame(var1, var2, result[c("estimate","p.value","statistic","method")], 
             stringsAsFactors=FALSE)
}

## Pairs of variables for which we want correlations
vars = data.frame(v1=names(data1)[1], v2=names(data1)[-1])

# Apply corrFunc to all rows of vars
corrs = do.call(rbind, mapply(corrFunc, vars[,1], vars[,2], MoreArgs=list(data=data1), 
                              SIMPLIFY=FALSE))

dim(corrs)
summary(corrs)

write.csv(corrs, "HK corr check 2.csv")


