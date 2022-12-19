### just two functions for xtable


construct_header <- function(df, grp_names, span, align = "c", draw_line = T) {
  if (length(align) == 1) align <- rep(align, length(grp_names))
  if (!all.equal(length(grp_names), length(span), length(align)))
    stop("grp_names and span have to have the same length!")

  if (ncol(df) < sum(span)) stop("Span has to be less or equal to the number of columns of df") 

  header <- mapply(function(s, a, grp) sprintf("\\multicolumn{%i}{%s}{%s}", s, a, grp),
                   span, align, grp_names)
  header <- paste(header, collapse = " & ")
  header <- paste0(header, " \\\\")

  if (draw_line) {
    # where do we span the lines:
    min_vals <- c(1, 1 + cumsum(span)[1:(length(span) - 1)])
    max_vals <- cumsum(span)
    line <- ifelse(grp_names == "", "", 
                   sprintf("\\cmidrule(lr){%i-%i}", min_vals, max_vals))
    line <- paste(line[line != ""], collapse = " ")

    header <- paste0(header, "  ", line, "\n  ")
  }

  addtorow <- list(pos = list(-1, -1, nrow(df)),
                   command = c("\\hline\n  ", header, "\\hline\n  "))
  return(addtorow)
}

##################### BEGIN data preprocessing for final data
load("moviedataRAW.rda")
str(data)
summary(data)

v1 <- c("Budget")
data[, v1] <- log(data[, v1])
v2 <- c("VOL..T.21..27","T.14..20.1","T.7..13.1","T.4..6.1","T.1..3.1")
data[,v2] <- log(data[,v2])
colnames(data) <- c(colnames(data)[1:19],"S-21-27","S-14-20","S-7-13","S-4-6","S-1-3","Vol-21-27","Vol-14-20","Vol-7-13","Vol-4-6","Vol-1-3")
summary(data)
colnames(data)[2]<-"Action"

data.std <- data.raw <- data

vmetr<- c("Budget", "Weeks", "Screens", "S-21-27","S-14-20","S-7-13","S-4-6","S-1-3",
                            "Vol-21-27","Vol-14-20","Vol-7-13","Vol-4-6","Vol-1-3")
data.std[,vmetr]<-scale(data[,vmetr])
summary(data.std[,vmetr])

vbin<- c( "Action","Adventure","Animation", "Comedy","Crime", "Drama", "Family",
         "Fantasy", "Mystery", "Romance", "Sci.Fi", "Thriller" ,"PG","PG13","R")  
summary(data[,vbin])   
data.std[,vbin]<-scale(data[,vbin],scale=FALSE)
#summary(data.std[,vbin])

data <- data.std
data.raw <- readxl::read_xls("moviedata.xls", sheet = "openbox")

save(data.raw, data, file="moviedata.rda") ## We will only provide moviedata.rda for book 
## END data preprocessing
##########################  END CODE for final data

### packages  and code needed
library("magrittr")
require("BMS")
source("helpers.R")
require("xtable")
require("MASS")
require("bayesreg")
SEED <- 2312
set.seed(SEED)

## load data
load("moviedata.rda")


############################################################################################################################
############################# 1.)  Data description for Table 1 and Table 2  ###############################################

## openbox <- readxl::read_xls("moviedata.xls", sheet = "openbox")
openbox <- data.raw
openbox <- openbox %>%
    dplyr::select(-title) %>%
    dplyr::rename(Action = `GENRE: Action`)
## Inspect size:
## 94 observations and 29 variables
dim(openbox)

genres <- c("Action", "Adventure", "Animation", "Comedy", "Crime",
            "Drama", "Family", "Fantasy", "Mystery", "Romance",
            "Sci-Fi", "Thriller")
mpaa <- c("PG", "PG13", "R")

## Number of genres per movie:
table(rowSums(openbox[, genres]))
paste0(round(prop.table(table(rowSums(openbox[, genres]))) * 100), "%")

## Table 1
Genres <- openbox[, genres] %>%
    reshape2::melt(id.var = NULL, variable.name = "Genre") %>%
    dplyr::group_by(Genre) %>%
    dplyr::summarise(Perc. = round(mean(value) * 100, digits = 1))

MPAA <- openbox[, mpaa] %>%
    dplyr::mutate(G = 1 - PG - PG13 - R) %>%
    reshape2::melt(id.var = NULL, variable.name = "MPAA Rating") %>%
    dplyr::mutate(`MPAA Rating` = factor(`MPAA Rating`, c("G", "PG", "PG13", "R"))) %>%
    dplyr::group_by(`MPAA Rating`) %>%
    dplyr::summarise(Perc. = round(mean(value) * 100, digits = 1)) %>%
    dplyr::mutate(`MPAA Rating` = as.character(`MPAA Rating`))   
Metrics <- openbox %>% 
    dplyr::select(`Open Box Office`, Budget, Weeks, Screens) %>%
    dplyr::mutate(Screens = Screens / 100) %>%
    dplyr::rename(`Open Box` = `Open Box Office`) %>%
    reshape2::melt(id.var = NULL, variable.name = "Variable") %>%
    dplyr::group_by(Variable) %>%
    dplyr::summarise(Mittwelwert = mean(value),
                     SD = sd(value),
                     Median = median(value),
                     Q1 = quantile(value, 0.25),
                     Q3 = quantile(value, 0.75))

tab.data1 <- xtable::xtable(cbind(rbind(Metrics,
                                 setNames(as.data.frame(c(list(""), as.list(rep(NA_real_, 5)))), names(Metrics)),
                                 setNames(as.data.frame(c(list(""), as.list(rep(NA_real_, 5)))), names(Metrics))),
                           rbind(MPAA, c("", NA_real_), c("", NA_real_)),
                           Genres[1:6, ], Genres[7:12, ]),
                     digits = 1)


print(tab.data1, type='latex', file='figures/table-dataDescr1.tex', include.rownames=FALSE)


## Table 2
sentiment <- openbox[, 20:24] %>%
    reshape2::melt(id.var = NULL, variable.name = "Variable") %>%
    dplyr::mutate(Variable = gsub("\\.\\..*", "", Variable)) %>%
    dplyr::mutate(Variable = gsub("^.*: ", "", Variable)) %>%
    dplyr::mutate(Variable = factor(Variable, c("T-1/-3", "T-4/-6", "T-7/-13", "T-14/-20", "T-21/-27"))) %>%
    dplyr::group_by(Variable) %>%
    dplyr::summarise(Mean = mean(value),
                     SD = sd(value),
                     Median = median(value),
                     Q1 = quantile(value, 0.25),
                     Q3 = quantile(value, 0.75))

volume <- (openbox[, 25:29] * 100) %>%
    reshape2::melt(id.var = NULL, variable.name = "Variable") %>%
    dplyr::mutate(Variable = gsub("\\.\\..*", "", Variable)) %>%
    dplyr::mutate(Variable = gsub("^.*: ", "", Variable)) %>%
    dplyr::mutate(Variable = factor(Variable, c("T-1/-3", "T-4/-6", "T-7/-13", "T-14/-20", "T-21/-27"))) %>%
    dplyr::group_by(Variable) %>%
    dplyr::summarise(Mean = mean(value),
                     SD = sd(value),
                     Median = median(value),
                     Q1 = quantile(value, 0.25),
                     Q3 = quantile(value, 0.75))

tab.data2 <- xtable::xtable(cbind(sentiment, volume),
                            digits = 1)

print(tab.data2,type='latex', file='figures/table-dataDescr2.tex', include.rownames = FALSE)

##########################################################################################################################
########################################## 2.) BAYESIAN LINEAR REGRESSION ##################################################

print("BMA estimation")
# data X and y are used for all estimations, i.e. BMA, Spike & SLab !
n<-dim(data)[1]
X=cbind(rep(1,n),as.matrix(data[,-1]))
colnames(X)[1] <- "Intercept"
K<-dim(X)[2]
y <- data$Open.Box.Office



# prior
B0=5 # prior variance for regression effects

# posterior parameters
# for beta
Xy <-crossprod(X,y)
Bn.inv <- crossprod(X)+diag(c(0,rep(1/B0,K-1)))
Bn <- solve(Bn.inv) 
bn <- Bn%*%crossprod(X,y)

# for sigma2
sn <- n/2
Sn <- (crossprod(y) - t(Xy)%*%Bn%*%Xy)/2

# posterior sampling
M <- 10000
beta.post<-matrix(NA,ncol=K, nrow=M)

sgma2.post <- 1/rgamma(M,sn,rate=Sn)

for (m in 1:M){
     beta.post[m,] <-mvrnorm(1,bn,sgma2.post[m]*Bn)
}

ew.post=colMeans(beta.post)
#ppos=apply(beta.post>0, 2,mean)
psc.blm=apply(sign(beta.post)==t(matrix(rep(sign(ew.post),M), ncol=M)),2,mean)


alpha<-0.05
hlow <- M*alpha/2
hup<- M*(1-alpha/2)
beta.post <- apply(beta.post,2,sort)

movie.blm <- cbind(ew.post, apply(beta.post, 2,sd),beta.post[hlow,],beta.post[hup,], 
              psc.blm )
colnames(movie.blm) <- c("PM" ,"PSD",  "Q2.5% ", "Q97.5%",  "PSC")
rownames(movie.blm) <- c("Intercept",  names(data.std)[-1])

tab.blm<-xtable(movie.blm,
       caption="Filmdaten: Ergebnisse des Bayesianischen Linearen Regressionsmodells mit Unabhängigkeitsprioriverteilung  ",
       align="l|cc|cc|c", digits=3,hline.after = c(-1,0),
       label="tab:resblm")


## gray rows
rws <- c(3,4,13,18,19,29)-1
col <- rep("\\rowcolor[gray]{0.8}", length(rws))
print(tab.blm, add.to.row = list(pos = as.list(rws), command = col), 
          type='latex', file='figures/table-BLM.tex',size="\\footnotesize")




############################################################################################################################
######################################### 3.)  BAYESIAN MODEL AVERAGING #########################################################

bmsfilename <- c("./results/bms_results.rda")
bms_res <- vector(mode = "list", length = 2)
names(bms_res) <- c("Binomal", "Beta-Binomial")
mpriors <- c("fixed", "random")
for(i in 1:length(mpriors)){
    mprior <- mpriors[i]
    kbar <- 14
    ## bms settings
    burnin <- 1e+05
    iter <- 1e+05
    start.v <- 1:(ncol(data)-1)

    #data.bma <- cbind(Open.Box.Office = y, X[,-1]) kann man auch aus helpers mal my.bms rausgeben
    ## vorher my.bms wegen y-mean(y)
    bms_res[[i]] <- bms(data, start.value = start.v,
                           burn = burnin, iter = iter, nmodel = iter, 
                           mprior = mprior, mprior.size = kbar,
                           g = "UIP", user.int = FALSE)
        
}

save(bms_res, file = bmsfilename)

################################
## Figure: Comparision of Priors for BMA
################################

K <- c(28) 
kbar <- 14

a <- 1
k0 <- 28
priors <- do.call("rbind", lapply(K, function(k) {
    b <- (k - kbar) / kbar
    data.frame(K = paste0("Anzahl potentieller Kovariaten = ", k),
               k = 0:k0 + rep(c(-0.2, 0.2), each = k0 + 1),
               prior = rep(c("Binomial", "Beta-binomial"), each = k0 + 1),
               y = c(dbinom(0:28, prob = kbar / k, size = k),
                     choose(k, 0:k0) * beta(a + 0:k0, b + k - 0:k0) / beta(a, b)))
}))
           
figpriors <- ggplot2::ggplot(priors, ggplot2::aes(k, xend = k, y = 0, yend = y)) +
    ggplot2::facet_grid(~ K) +
    ggplot2::geom_segment(ggplot2::aes(linetype = prior)) +
    ggplot2::geom_point(ggplot2::aes(k, y, shape = prior)) +
    ggplot2::theme_bw() +
    ggplot2::guides(linetype = FALSE, shape = FALSE) +
    ggplot2::ylab("prior")

ggplot2::ggsave("./figures/priors.pdf", plot = figpriors,
                width = 8, height = 4)



###########################################
##  BMA results
###########################################

summary.table <- do.call(rbind, lapply(bms_res, summary))
#summary.table <- data.frame(set, summ.bms.table)
rownames(summary.table) <- paste0("M-", 1:nrow(summary.table))
summary.table.2 <- summary.table[, c(11,12,1,5,7,13)]
colnames(summary.table.2) <- gsub("Shrinkage.Stats", "Shrinkage (Av, Stdev)", colnames(summary.table.2))
summary.table.2[, "Shrinkage (Av, Stdev)"] <- gsub("Av=", "",  summary.table.2[, "Shrinkage (Av, Stdev)"])
summary.table.2[, "Shrinkage (Av, Stdev)"] <- gsub("Stdev=", "",  summary.table.2[, "Shrinkage (Av, Stdev)"])

### This is table1 of summary statistics for paper
tab.bma <- xtable::xtable(summary.table.2,caption = "summary statistics of the different BMA specifications")
print(tab.bma, type='latex', file='figures/table-BMA1.tex',size="\\footnotesize")


## Prior Posterior for Binommial
MS_M1 <- plotModelsize(bms_res[[1]], ksubset=0:25)
##
## #MS_M11 <- plotModelsize(bms_res[[11]])
priors.post <- priors
priors.post$K <- rep("Binomial Modellprior und Posterioriverteilung (K=28)",nrow(priors.post))
priors.post$prior <- rep(c("Binom.Prior", "Binom.Posterior"), each = k0 + 1)
priors.post$y[30:nrow(priors.post)] <- MS_M1$dens 
##take only those smaller than 15
ind <- which(priors.post$k<28.5)
priors.post <- priors.post[ind,]

           
figpriorspost.binom <- ggplot2::ggplot(priors.post, ggplot2::aes(k, xend = k, y = 0, yend = y)) +
    ggplot2::scale_x_continuous(breaks = 0:28) + 
    ggplot2::facet_grid(~ K) +
    ggplot2::geom_segment(ggplot2::aes(linetype = prior)) +
    ggplot2::geom_point(ggplot2::aes(k, y, shape = prior)) +
    ggplot2::theme_bw() +
    ggplot2::guides(linetype = FALSE, shape = FALSE) +
    ggplot2::ylab("Dichte") +
    ggplot2::xlab("Anzahl der Kovariaten im Modell")

ggplot2::ggsave("./figures/priorpostbinom.pdf", plot = figpriorspost.binom,
                width = 8, height = 4)


## Prior Posterior for Beta-Binomial
MS_M2 <- plotModelsize(bms_res[[2]])
priors.post <- priors
priors.post$K <- rep("Beta-Binomial Modellprior und Posterioriverteilung (K=28)",nrow(priors.post))
priors.post$prior <- rep(c("Beta-Binom.Prior", "Beta-Binom.Posterior"), each = k0 + 1)
priors.post$y[1:29] <- priors.post$y[30:nrow(priors.post)] 
priors.post$y[30:nrow(priors.post)]  <- MS_M2$dens
##take only those smaller than 15
ind <- which(priors.post$k<28.5)
priors.post <- priors.post[ind,]

           
figpriorspost.beta <- ggplot2::ggplot(priors.post, ggplot2::aes(k, xend = k, y = 0, yend = y)) +
    ggplot2::scale_x_continuous(breaks = 0:28) + 
    ggplot2::facet_grid(~ K) +
    ggplot2::geom_segment(ggplot2::aes(linetype = prior)) +
    ggplot2::geom_point(ggplot2::aes(k, y, shape = prior)) +
    ggplot2::theme_bw() +
    ggplot2::guides(linetype = FALSE, shape = FALSE) +
    ggplot2::ylab("Dichte") +
    ggplot2::xlab("Anzahl der Kovariaten im Modell")

ggplot2::ggsave("./figures/priorpostbetabinom.pdf", plot = figpriorspost.beta,
                width = 8, height = 4)



## create Table of coefficients
M1 <- coef(bms_res[[1]], include.constant=T, order.by.pip=F)[,1:4 ]
#M2 <- cbind(coef(bms_res[[2]], include.constant=T, order.by.pip=F)[,1:4 ], Rang=1:29)
M2 <- coef(bms_res[[2]], include.constant=T, order.by.pip=F)[,1:4 ]



## estimate sign certainty not positive sign certainty :-)
S <- as.numeric(which(sign(M1[,"Post Mean"])<0))
M1[S,"Cond.Pos.Sign"] <- 1-M1[S,"Cond.Pos.Sign"]

S <- as.numeric(which(sign(M2[,"Post Mean"])<0))
M2[S,"Cond.Pos.Sign"] <- 1-M2[S,"Cond.Pos.Sign"]

#PIPs  <- PIPs[order(PIPs$PIP.x, decreasing=TRUE),]
## rownames(PIPs) <- PIPs[,1]
## PIPs <- PIPs[,-1]

PIPs  <- cbind(M1,M2)
PIPs <- PIPs[c(nrow(PIPs),1:(nrow(PIPs)-1)),]
rws <- c(3,4,13,18,19,29)-1
col <- rep("\\rowcolor[gray]{0.8}", length(rws))
colnames(PIPs) <- rep(c("PIP", "PM", "PSD","PSC"),2)
tab.bma2 <- xtable::xtable(PIPs,digits=3, caption="BMA Koeffizienten f\"ur die zwei betrachtene Modellprioris")
print(tab.bma2,only.contents=TRUE, add.to.row = list(pos = as.list(rws), command = col), type='latex', file='figures/table-BMA2.tex')


## ## Figure 4 Posterior inclusion probabilities for models M-1 (Binom), M-2 (Beta-Binom) 
## pdf("./figures/pipComp.pdf", width=11, height=5)
## BMS::plotComp(M1=bms_res[[1]], M2=bms_res[[2]])
## dev.off()

## ## Table 5
## print(xtable::xtable(coef(bms_res[[1]])[1:20,1:4 ], digits=4, caption="BMA coefficient results for model M-1 (Binomial)"))

## ## Table 6
## print(xtable::xtable(coef(bms_res[[2]])[1:20,1:4 ], digits=4, caption="BMA coefficient results for model M-2 (Beta-Binomial)"))

## PIPS1 <- coef(bms_res[[1]])
## rownames(PIPS1) <- gsub("\\..", "-", rownames(PIPS1))


#############################################################################################################################
######################################   Spike & Slab  #####################################################################

## the following source codes are needed
source("Reg_with_GPrior.R")
source("Reg_with_NMIG.R")
source("Reg_with_SSVS.R")


#-----2) VS using Stochastic Search Variable selection (SSVS)

k <- ncol(X)
n <- nrow(X)
tau2=100
c=1/(10000)
m=10000

set.seed(2312)
reg=regSSVS(y,X[,-1],m,tau2,c)
b=reg$b
s2=reg$s2
ny=reg$ny
w=reg$w
mu.b <- colMeans(b) 
psc.SSVS=apply(sign(b)==sign(matrix(rep(mu.b,m), byrow=TRUE, ncol=k)),2,mean)

#p_bgreater0 = apply(b>0, 2,mean)

alpha<-0.05
hlow <- m*alpha/2
hup<- m*(1-alpha/2)
tab.SSVS=data.frame(c(1,colMeans(ny)),colMeans(b),apply(b,2,sd),psc.SSVS) #,coef(mod_lm),,b[hlow,],b[hup,],
colnames(tab.SSVS)=c("PIP","mean" ,"sd",  "PSC") #,"OLS","Q2.5% ", "Q97.5%", 
rownames(tab.SSVS)=colnames(X)
print(tab.SSVS)


#-----3) VS with NMIG prior

m=5000

apsi0=5
bpsi0=200
s0=0.01  #0.000025
s1=1

set.seed(123)
reg=regNMIG(y,X[,-1],m,apsi0,bpsi0,s0,s1)

b=reg$b
s2=reg$s2
ny=reg$ny_w
psi2=reg$psi2
w=reg$w


mu.b <- colMeans(b) 
psc.NMIG=apply(sign(b)==sign(matrix(rep(mu.b,m), byrow=TRUE, ncol=k)),2,mean)

alpha<-0.05
hlow <- m*alpha/2
hup<- m*(1-alpha/2)

tab.NMIG=data.frame(c(1,colMeans(ny)),colMeans(b),apply(b,2,sd),psc.NMIG) #,coef(mod_lm),,b[hlow,],b[hup,],
colnames(tab.NMIG)=c("PIP","mean" ,"sd",  "PSC") #,"OLS","Q2.5% ", "Q97.5%", 
rownames(tab.NMIG)=colnames(X)
print(tab.NMIG)



## merge both tabs to one table
tab <- cbind(tab.SSVS, tab.NMIG)
colnames(tab) <- rep(c("PIP", "PM", "PSD", "PSC"),2)


ugly_header <- construct_header(tab, c("SSVS", "NMIG"), c(4, 4), c("c", "c"))

rws <- c(3,4,13,18,19,29)-1
col <- rep("\\rowcolor[gray]{0.8}", length(rws))


result.table = xtable(tab,
                      caption="Filmdaten. Schaetzung mit NMIG-Priori, psi2 sim Gamma{-1}(5,200),s0=0.01.",
                      align="l|cccc||cccc|", digits=3,hline.after = c(-1,0),label="tab:nmigprior")

print(result.table,only.contents=TRUE, add.to.row = list(pos = as.list(rws), command = col), type='latex', file='figures/table-SpSl.tex',size="\\small")


#############################################################################################################################
#########################################################  LASSO
#-------------------------------------------------------------------------------
# Bayesian linear regression 
# Lasso Prior

k <- ncol(X)
n <- nrow(X)
reg.lasso <- bayesreg(Open.Box.Office~.,data=data,prior="lasso", 
                   model="Gaussian",thin=1, n.samples=M)  
mean(reg.lasso$sigma2)
res.lasso <- summary(reg.lasso)

psc.lasso=apply(sign(reg.lasso$beta)==matrix(rep(sign(reg.lasso$mu.beta),M), ncol=M),1,mean)

movie.lasso <- cbind(res.lasso$mu.coef, res.lasso$se.coef,res.lasso$CI.coef,c(psc.lasso,1))
movie.lasso<-movie.lasso[c(k,1:(k-1)), ]
colnames(movie.lasso) <- c("PM" ,"PSD",  "Q2.5% ", "Q97.5%", "PSC")

rownames(movie.lasso)[1] <- "Intercept"

## tab.lasso<-xtable(movie.lasso,
##            caption="Filmdaten: Ergebnisse des Bayesianischen Linearen Regressionsmodells mit Lasso-Prioriverteilung ",
##            align="l|cc|cc|c", digits=3,hline.after = c(-1,0),label="tab:res-lasso")

## print(tab.lasso, type='latex', file='figures/table-lasso.tex',size="\\small")


#---- Horseshoe Priori
thn=10
reg.hs <- bayesreg(Open.Box.Office~.,data=data,prior="hs", 
                   thin=thn, n.samples=M*thn)  # niedrige Efficiency ohne thinning 
mean(reg.hs$sigma2)
res.hs <- summary(reg.hs)

psc.hs=apply(sign(reg.hs$beta[,seq(from=1, by=thn, to=M*thn)])==matrix(rep(sign(reg.hs$mu.beta),M), ncol=M),1,mean)

movie.hs <- cbind(res.hs$mu.coef, res.hs$se.coef,res.hs$CI.coef, c(psc.hs,1) )
movie.hs<-movie.hs[c(k,1:(k-1)), ] # oder andere Ordnung
colnames(movie.hs) <- c("PM" ,"PSD",  "Q2.5% ", "Q97.5%", "PSC")


## combine to one table
tab.shrink <- cbind(movie.lasso[,c(1,2,5)],movie.hs[,c(1,2,5)])

ugly_header <- construct_header(tab.shrink, c("LASSO", "HORSESHOE"), c(3, 3), c("c", "c"))
result.table.shrink = xtable(tab.shrink,
                      caption="Filmdaten: Ergebnisse des Bayesianischen Linearen Regressionsmodells mit Horseshoe-Prioriverteilung",
                      align="l|ccc||ccc|", digits=3,hline.after = c(-1,0),label="tab:nmigprior")

print(result.table.shrink,only.contents=TRUE, add.to.row = list(pos = as.list(rws), command = col), type='latex', file='figures/table-LaHo.tex',size="\\small")




## tab.hs<-xtable(movie.hs,
##        caption="Filmdaten: Ergebnisse des Bayesianischen Linearen Regressionsmodells mit Horseshoe-Prioriverteilung ",
##        align="l|cc|cc|c", digits=3,hline.after = c(-1,0), label="tab:res-hs")
## print(tab.hs, type='latex', file='figures/table-hs.tex',size="\\small")











