# # util.R
# 
# # Collection of useful stuff.
# 
# library(formatR)
# library(xtable)
# #library(Hmisc)
# library(dplyr)
# library(RJSONIO)
# library(jsonlite)
# library(metaSEM)
# library(metafor)
# library(foreign)
# library(OpenMx)
# library(psych)
# ##library(gdata)
# ##library(ordinal)
# library(MASS)
# library(Matrix)
# library(lpSolve)
# ##library(mediation)
# library(irr)
# library(lme4)
# library(car)
# library(ggplot2)
# ##library(metafor)
# library(MAd)
# library(scales)
# library(memisc)
# library(plotrix)
# library(broom)
# library(lattice)
# library(vcd)
# #library(ROCR)
# library(dplyr)
# library(lm.beta)
# 
# 
# #lm(churchat11r~church,data=dat.i)
# #m1<-(glm(churchat11r~church,data=dat.i,family="binomial"))
# #m1
# #log(exp(coef(m1))) #odds ratio
# #exp(cbind(OR = coef(m1), confint(m1))) #odds ratio and 95% CI
# 
# 
# # cleans up coding style; by default from the clipboard
# # 1. copy ugly code to clipboard
# # 2. call util.tidy() in console
# # 3. paste outputted code to editor
# util.tidy <- function(source = "clipboard") {
#   tidy_source(source=source, arrow = TRUE, width.cutoff = 78)
# }
# 
# # Syntactic sugar for string concatenation, Example:
# # > "hello" %+% "world"
# # [1] "helloworld"
# "%+%" <- function (x, y) { paste(x, y, sep="") }
# 
# #   STRING CLEANING ####
# util.strip_special_characters <- function(x){ gsub("[^0-9A-Za-z]","",x) }
# 
# util.clean_str <- function(x){
#   x %>%
#     tolower() %>%
#     util.strip_special_characters() %>%
#     gsub("^ +| +$", "", .) %>%  # leading/trailing spaces
#     gsub(" +", " ", .)          # extra space
# }
# util.clean_string <- util.clean_str
# # util.clean_str(" D* ") == "d"
# 
# util.reverse_likert <- function(v, scale_levels) {
#   # Require that responses are numeric so that we can do arithmetic
#   reversed_data <- as.numeric(v)
#   # Change 6's to 1's and 2's to 5's, etc.
#   return(scale_levels - reversed_data + 1)
# }
# 
# # HTML PRINTING####
# 
# # maybe we should remove this function
# util.html_table <- function(df, ...) {
#   if(any(class(df) %in% c("grouped_df", "tbl_df"))){
#     df <- data.frame(ungroup(df))
#   }
#   if(all(class(df) %in% c("psych","alpha"))){
#     # print just the alpha coefficients
#     df <- df$total
#   }
#   if( ! interactive() ){
#     if( any( class(df) %in% c("lmerMod","lm","aov","glm","glmerMod") ) ){
#       stargazer(df, type="html", 
#                 star.cutoffs = c(.05, .01, .001),
#                 notes        = "", 
#                 notes.label = "1 star p<.05; 2 stars p<.01; 3 stars p<.001",
#                 notes.append = FALSE,
#                 single.row=TRUE
#       )
#     }
#     else{
#       print(xtable(df, ...), 
#             type="html",
#             html.table.attributes = 
#               getOption("xtable.html.table.attributes", "border=0, class='xtable'"), ...)
#     }
#   }
#   else{
#     if( any( class(df) %in% c("lmerMod","lm","aov","glm","glmerMod") ) ){
#       print(summary(df))
#     }else{
#       print(df)
#     }
#   }
# }
# 
# # prints to html as it would to console (preformatted html)
# util.print_pre <- function(x){
#   if(interactive()) return(x) 
#   capture.output(print(x)) %>%
#     paste(collapse="\n") %>%
#     paste("<pre>",.,"</pre>") %>%
#     cat()
# }
# 
# util.warn <- function(message) {
#   if (interactive()) {
#     warning(message)
#   } else {
#     paste0("<div class='warning'>", message, "</div>") %>% cat()
#   }
# }
# 
# util.caution <- function(message) {
#   if (interactive()) {
#     message(message)
#   } else {
#     paste0("<div class='caution'>", message, "</div>") %>% cat()
#   }
# }
# 
# util.passed <- function(message) {
#   if (interactive()) {
#     cat(message)
#   } else {
#     paste0("<div class='pass'>", message, "</div>") %>% cat()
#   }
# } 
# 
# 
# # Basic Data Transformation####
# util.z_score <- function(x){
#   if(class(x) %in% "data.frame"){
#     do.call(cbind,lapply(x, util.z_score))
#   }
#   else{ ( x - mean(x,na.rm=T) ) / sd(x,na.rm=T) }
# }
# 
# util.is_blank <- function(x){
#   is.na(x) | grepl("^[ \t]*$", x)
# }
# 
# # round all numeric columns####
# util.round_df <- function(DF, digits=2, show_caution=TRUE){
#   vec_list <- lapply( DF, function(vec){
#     if( is.numeric(vec) ){
#       vec <- round(vec,digits)
#     }
#     else{ 
#       if(show_caution){
#         util.caution("Any characters will make all columns characters.")
#       }
#     }
#     return( vec )
#   })
#   do.call( cbind, vec_list ) %>%
#     as.data.frame()
# }
# 
# # reverse coding
# util.reverse_likert <- function(v, scale_levels) {
#   # Require that responses are numeric so that we can do arithmetic
#   reversed_data <- as.numeric(v)
#   # Change 6's to 1's and 2's to 5's, etc.
#   return(scale_levels - reversed_data + 1)
# }
# 
# # group center
# group.center <- function(var,grp) {
#   return(var-ave(var,grp,FUN=function(x) mean(x,na.rm=T)))
# }
# 
# group.z <- function(var,grp) {
#   return(
#     (var-ave(var,grp,FUN=function(x) mean(x,na.rm=T)))/ave(var,grp,FUN=function(x) sd(x,na.rm=T))
#   )
# }
# 
# #round all numeric values in a data frame####
# round_df <- function(x, digits) {
#   # round all numeric variables
#   # x: data frame 
#   # digits: number of digits to round
#   numeric_columns <- sapply(x, class) == 'numeric'
#   x[numeric_columns] <-  round(x[numeric_columns], digits)
#   x
# }
# 
# #change a bunch to factor or numeric
# #cars[, 1:2] <- sapply(cars[, 1:2], as.factor)
# #merge7[, c(2:25)] <- sapply(merge7[, c(2:25)], as.numeric)
# 
# rounded_mean_original <- function (v) {
#   round(mean(as.numeric(v[!is.na(v)])), digits = 2)
# }
# 
# # Robust standard errors####
# 
# summaryR.lm <- function(model, type=c("hc3", "hc0", "hc1", "hc2", "hc4"), ...){
#   
#   if (!require(car)) stop("Required car package is missing.")
#   
#   type <- match.arg(type)
#   V <- hccm(model, type=type)
#   sumry <- summary(model)
#   table <- coef(sumry)
#   table[,2] <- sqrt(diag(V))
#   table[,3] <- table[,1]/table[,2]
#   table[,4] <- 2*pt(abs(table[,3]), df.residual(model), lower.tail=FALSE)
#   
#   sumry$coefficients <- table
#   p <- nrow(table)
#   hyp <- cbind(0, diag(p - 1))
#   sumry$fstatistic[1] <- linearHypothesis(model, hyp,white.adjust=type)[2,"F"]
#   
#   print(sumry)
#   cat("Note: Heteroscedasticity-consistent standard errors using adjustment", type, "\n")
#   
# }
# 
# summaryR2.lm <- function(model, type=c("hc3", "hc0", "hc1", "hc2", "hc4"), ...){
#   
#   if (!require(car)) stop("Required car package is missing.")
#   
#   type <- match.arg(type)
#   V <- hccm(model, type=type)
#   sumry <- summary(model)
#   table <- coef(sumry)
#   table[,2] <- sqrt(diag(V))
#   table[,3] <- table[,1]/table[,2]
#   table[,4] <- 2*pt(abs(table[,3]), df.residual(model), lower.tail=FALSE)
#   
#   sumry$coefficients <- table
#   p <- nrow(table)
#   hyp <- cbind(0, diag(p - 1))
#   sumry$fstatistic[1] <- linearHypothesis(model, hyp,white.adjust=type)[2,"F"]
#   
#   return(sumry)
#   cat("Note: Heteroscedasticity-consistent standard errors using adjustment", type, "\n")
#   
# }
# 
# # Calculate correlation coefficient from t values
# r_from_t2 <- function(t, n) {
#   t/sqrt(t*t+n-2)
# }
# 
# # Make all the names lower cases 
# lower <- function (df) { #names lower case
#   names(df) <- tolower(names(df))
#   df
# }
# 
# na.zero <- function (x) {
#   x[is.na(x)] <- 0
#   return(x)
# }
# 
# nna.one <- function (x) {
#   x[is.na(x)==FALSE] <- 1
#   return(x)
# }
# 
# #x<-c(1,2,NA)
# #y<-nna.one(x)
# #na.zero(y)
# 
# 
# #t test based on mean and variance####
# # m1, m2: the sample means
# # s1, s2: the sample standard deviations
# # n1, n2: the same sizes
# # m0: the null value for the difference in means to be tested for. Default is 0. 
# # equal.variance: whether or not to assume equal variance. Default is FALSE.
# t.test2 <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
# {
#   if( equal.variance==FALSE ) 
#   {
#     se <- sqrt( (s1^2/n1) + (s2^2/n2) )
#     # welch-satterthwaite df
#     df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
#   } else
#   {
#     # pooled standard deviation, scaled by the sample sizes
#     se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
#     df <- n1+n2-2
#   }      
#   t <- (m1-m2-m0)/se 
#   dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))    
#   names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
#   return(dat) 
# }
# 
# 
# 
# ## FUNCTION ####
# #covariate5 correspond to outcome
# #covariate6 correspond to treatment
# lm.glm <- function(a_model_spec,b_model_spec,
#                    c_outcome_spec,d_treatment_spec,
#                    e_covariate1_spec,f_covariate2_spec,
#                    g_covariate3_spec,h_covariate4_spec,i_covariate5_correspond,
#                    j_covariate6_correspond,
#                    file_name){
#   b_value<-c()
#   p_value<-c()
#   model <- c()
#   link <-c()
#   outcome <- c()
#   missing <- c()
#   treatment <- c()
#   covariate1 <- c()
#   covariate2 <- c()
#   covariate3 <- c()
#   covariate4 <- c()
#   covariate5 <- c()
#   covariate6 <- c()
#   
#   for (a in c(1:length(a_model_spec)))
#     for (b in c(1:length(b_model_spec)))
#       for (c in c(1:length(c_outcome_spec)))
#         for (d in c(1:length(d_treatment_spec)))
#           for (e in c(1:length(e_covariate1_spec)))
#             for (f in c(1:length(f_covariate2_spec)))
#               for (g in c(1:length(g_covariate3_spec)))
#                 for (h in c(1:length(h_covariate4_spec))){{{{{{{{
#                   formula<-as.formula(paste(c_outcome_spec[c]," ~ ",
#                                             d_treatment_spec[d],
#                                             e_covariate1_spec[e],
#                                             f_covariate2_spec[f],
#                                             g_covariate3_spec[g],
#                                             h_covariate4_spec[h],
#                                             i_covariate5_correspond[c],
#                                             j_covariate6_correspond[d]))
#                   b_value.dat<-ifelse(a_model_spec[a]=="glm"& b_model_spec[b]=="logit",
#                                       summary(glm(formula,data=data,family=binomial(link = 'logit')))$coefficients[2,1],
#                                       ifelse(a_model_spec[a]=="glm"& b_model_spec[b]=="probit",
#                                              summary(glm(formula,data=data,family=binomial(link = 'probit')))$coefficients[2,1],
#                                              ifelse(a_model_spec[a]=="lm",
#                                                     summary(lm(formula,data=data))$coefficients[2,1],NA)))
#                   b_value<-c(b_value,b_value.dat)
#                   p_value.dat<-ifelse(a_model_spec[a]=="glm"& b_model_spec[b]=="logit",
#                                       summary(glm(formula,data=data,family=binomial(link = 'logit')))$coefficients[2,4],
#                                       ifelse(a_model_spec[a]=="glm"& b_model_spec[b]=="probit",
#                                              summary(glm(formula,data=data,family=binomial(link = 'probit')))$coefficients[2,4],
#                                              ifelse(a_model_spec[a]=="lm",
#                                                     summary(lm(formula,data=data))$coefficients[2,4],NA)))
#                   p_value<-c(p_value,p_value.dat)
#                   
#                   model <- c(model, a_model_spec[a])
#                   link <- c(link,b_model_spec[b])
#                   outcome <- c(outcome, c_outcome_spec[c])
#                   treatment <- c(treatment, d_treatment_spec[d])
#                   covariate1 <- c(covariate1, e_covariate1_spec[e])
#                   covariate2 <- c(covariate2, f_covariate2_spec[f])
#                   covariate3 <- c(covariate3, g_covariate3_spec[g])
#                   covariate4 <- c(covariate4, h_covariate4_spec[h])
#                   covariate5 <- c(covariate5, i_covariate5_correspond[c])
#                   covariate6 <- c(covariate6, j_covariate6_correspond[d])
#                 }}}}}}}}
#   
#   output_dat<-as.data.frame(cbind(model, link, outcome, treatment, covariate1,
#                                   covariate2, covariate3, covariate4,
#                                   covariate5,covariate6,
#                                   b_value,p_value))
#   
#   
#   output_dat$covariate1<-gsub("[+]","",output_dat$covariate1)
#   output_dat$covariate2<-gsub("[+]","",output_dat$covariate2)
#   output_dat$covariate3<-gsub("[+]","",output_dat$covariate3)
#   output_dat$covariate4<-gsub("[+]","",output_dat$covariate4)
#   output_dat$covariate5<-gsub("[+]","",output_dat$covariate5)
#   output_dat$covariate6<-gsub("[+]","",output_dat$covariate6)
#   
#   
#   write.csv(output_dat, paste0(file_name[1],Sys.Date(),".csv"))
#   
# }
# 
# 
# #a_model_spec <- c("glm")  #options: "glm", "lm"
# #b_model_spec <- c("logit","probit") #options: "logit", "probit"
# #c_outcome_spec <- c("contft", "fallft")
# #d_treatment_spec <- c("anytreatment", "belonging", "belongingnomindset", "belongonlyvscontrol")
# #e_covariate1_spec <- c("+year", "")
# #f_covariate2_spec <- c("+sat_missing +sat_dummy","")
# #g_covariate3_spec <- c("+gpa_missing +gpa_dummy", "")
# #h_covariate4_spec <- c("+as.factor(schoolname)", "")
# #i_covariate5_correspond <- c("+Bcontft", "+Bfallft")
# #j_covariate6_correspond <- c("+banytreatment", "+bbelonging", "+bbelongingnomindset", "+bbelongonlyvscontrol")
# #file_name<-c("Histogram of ps ","study1models ","study1sigprop ")
# 
# 
# #lm.glm(a_model_spec,b_model_spec,
# #       c_outcome_spec,d_treatment_spec,
# #       e_covariate1_spec,f_covariate2_spec,
# #       g_covariate3_spec,h_covariate4_spec,
# #       i_covariate5_correspond,j_covariate6_correspond,file_name)
# 
# 
# format_pval <- function(x){
#   library(broman) # for myround function
#   if (x < .001) return(paste('<', '.001'))
#   #if (x > .250) return(paste('>', '.250'))
#   paste('=', myround(x, 3))   # 3 = no. of digits to round p value to if .001 < p < .250.
# }
# 
# 
# format_bp_value <- function(bvalue,pvalue){
#   library(broman) # for myround function
#   x<-c()
#   for (a in c(1:length(bvalue))){
#   mystars <- ifelse(pvalue[a] < .001, "***",
#                     ifelse(pvalue[a] < .01, "** ", 
#                            ifelse(pvalue[a] < .05, "* ",
#                                   ifelse(pvalue[a] < .10, "+", " "))))
#   x <- rbind(x, cbind((paste(myround(bvalue[a], 2), mystars,sep="")), 
#               myround(pvalue[a],2)))
#   
#   }
#   colnames(x)<- c("b_value","p_value") 
#   return(x)
#   }
# 
# 
# 
# #Correlation table
# #generate function
# corstars <- function(x){ 
#   require(Hmisc) 
#   x <- as.matrix(x) 
#   R <- rcorr(x)$r 
#   p <- rcorr(x)$P 
#   ## define notions for significance levels; spacing is important.
#   mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", ifelse(p < .10, "+", " "))))
#   ## trunctuate the matrix that holds the correlations to two decimal
#   R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
#   ## build a new matrix that includes the correlations with their appropriate stars 
#   Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
#   diag(Rnew) <- paste(diag(R), " ", sep="") 
#   rownames(Rnew) <- colnames(x) 
#   colnames(Rnew) <- paste(colnames(x), "", sep="") 
#   ## remove upper triangle
#   Rnew <- as.matrix(Rnew)
#   Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
#   Rnew <- as.data.frame(Rnew) 
#   ## remove last column and return the matrix (which is now a data frame)
#   Rnew <- cbind(Rnew[1:length(Rnew)-1])
#   return(Rnew) 
# }
# 
# #create table
# #print(xtable(corstars(mtcars)), comment=F, scalebox=.95, latex.environments = "centering")
# 
# 
# 
# ## FUNCTION ####
# lm.glm.0 <- function(data,a_model_spec,b_model_spec,
#                      c_outcome_spec,d_treatment_spec,
#                      e_covariate1_spec,f_covariate2_spec,
#                      g_covariate3_spec,h_covariate4_spec,
#                      file_name){
#   b_value<-c()
#   p_value<-c()
#   model <- c()
#   link <-c()
#   outcome <- c()
#   missing <- c()
#   treatment <- c()
#   covariate1 <- c()
#   covariate2 <- c()
#   covariate3 <- c()
#   covariate4 <- c()
#   
#   
#   for (a in c(1:length(a_model_spec)))
#     for (b in c(1:length(b_model_spec)))
#       for (c in c(1:length(c_outcome_spec)))
#         for (d in c(1:length(d_treatment_spec)))
#           for (e in c(1:length(e_covariate1_spec)))
#             for (f in c(1:length(f_covariate2_spec)))
#               for (g in c(1:length(g_covariate3_spec)))
#                 for (h in c(1:length(h_covariate4_spec))){{{{{{{{
#                   formula<-as.formula(paste(c_outcome_spec[c]," ~ ",
#                                             d_treatment_spec[d],
#                                             e_covariate1_spec[e],
#                                             f_covariate2_spec[f],
#                                             g_covariate3_spec[g],
#                                             h_covariate4_spec[h]))
#                   b_value.dat<-ifelse(a_model_spec[a]=="glm"& b_model_spec[b]=="logit",
#                                       summary(glm(formula,data=data,family=binomial(link = 'logit')))$coefficients[2,1],
#                                       ifelse(a_model_spec[a]=="glm"& b_model_spec[b]=="probit",
#                                              summary(glm(formula,data=data,family=binomial(link = 'probit')))$coefficients[2,1],
#                                              ifelse(a_model_spec[a]=="lm",
#                                                     summary(lm(formula,data=data))$coefficients[2,1],NA)))
#                   b_value<-c(b_value,b_value.dat)
#                   p_value.dat<-ifelse(a_model_spec[a]=="glm"& b_model_spec[b]=="logit",
#                                       summary(glm(formula,data=data,family=binomial(link = 'logit')))$coefficients[2,4],
#                                       ifelse(a_model_spec[a]=="glm"& b_model_spec[b]=="probit",
#                                              summary(glm(formula,data=data,family=binomial(link = 'probit')))$coefficients[2,4],
#                                              ifelse(a_model_spec[a]=="lm",
#                                                     summary(lm(formula,data=data))$coefficients[2,4],NA)))
#                   p_value<-c(p_value,p_value.dat)
#                   
#                   model <- c(model, a_model_spec[a])
#                   link <- c(link,b_model_spec[b])
#                   outcome <- c(outcome, c_outcome_spec[c])
#                   treatment <- c(treatment, d_treatment_spec[d])
#                   covariate1 <- c(covariate1, e_covariate1_spec[e])
#                   covariate2 <- c(covariate2, f_covariate2_spec[f])
#                   covariate3 <- c(covariate3, g_covariate3_spec[g])
#                   covariate4 <- c(covariate4, h_covariate4_spec[h])
#                 }}}}}}}}
#   
#   output_dat<-as.data.frame(cbind(model, link, outcome, treatment, covariate1,
#                                   covariate2, covariate3, covariate4,
#                                   b_value,p_value))
#   
#   
#   output_dat$covariate1<-gsub("[+]","",output_dat$covariate1)
#   output_dat$covariate2<-gsub("[+]","",output_dat$covariate2)
#   output_dat$covariate3<-gsub("[+]","",output_dat$covariate3)
#   output_dat$covariate4<-gsub("[+]","",output_dat$covariate4)
#   
#   
#   
#   write.csv(output_dat, paste0(file_name[1],Sys.Date(),".csv"))
#   
# }
# 
# '%!in%' <- function(x,y){!('%in%'(x,y))}
# 
# 
# r2.corr.mer <- function(m) {
#   lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
#   summary(lmfit)$r.squared
# }
# 
# 
# stdCoef.merMod <- function(object) {
#   sdy <- sd(getME(object,"y"))
#   sdx <- apply(getME(object,"X"), 2, sd)
#   sc <- fixef(object)*sdx/sdy
#   se.fixef <- coef(summary(object))[,"Std. Error"]
#   se <- se.fixef*sdx/sdy
#   return(data.frame(stdcoef=sc, stdse=se))
# }
# 
# ### Program to create a balance table
# cond_diffs_cont <- function(condition, outcome){
#   library(broom)
#   library(MAd)
#   N<- tapply(outcome , condition, 
#              function(x){length(x[!is.na(x)])}
#   )
#   mean<- tapply(outcome , condition, mean, na.rm=T)
#   sd<- tapply(outcome , condition, sd, na.rm=T)
#   t <- tidy(t.test(outcome~condition)) ### getting the statistical test
#   es <- t_to_d(t$statistic,t$parameter/2,t$parameter/2) ## Effect size - t to d formula
#   x <- round(cbind(N[1],mean[1], sd[1], N[2],mean[2], sd[2], t$statistic, es[1]),3)
#   x <- cbind(x,  format_pval(t$p.value))
# }
# 
# 
# cond_diffs_categ <- function(condition, outcome){
#   library(broom)
#   library(MAd)
#   N<- tapply(outcome , condition, 
#     function(x){length(x[!is.na(x)])}
#   )
#   mean<- tapply(outcome , condition, mean, na.rm=T)
#   t <- tidy(chisq.test(condition, outcome)) ### getting the statistical test
#   n <- sum(tidy(table(data$cond, data$s1_mindset_dichot))[,3])
#   es <- r_to_d(r_from_chi(t$statistic,n),n) ## Effect size - chisq to r to d  
#   x <- cbind(
#     N[1],
#     paste(round(mean[1], 3)*100, "%", sep="")
#     , NA, 
#     N[2],
#     paste(round(mean[2], 3)*100, "%", sep=""),
#     NA, 
#     round(t$statistic, 3), 
#     round(es[1], 3)
#   )
#   x <- cbind(x,  format_pval(t$p.value))
# }
# 
# balancetable <- function(cond, contvars, catvars, rownames){ ## Takes as input columns from data frame
#   library(dplyr)
#   x<- c()
#   for (a in c(1:length(contvars))){
#     x<- rbind(x,
#               (cond_diffs_cont(cond, contvars[,a]))
#     )
#   }
#   for (b in c(1:length(catvars))){
#     x<- rbind(x,
#               (cond_diffs_categ(cond, catvars[,b]))
#     )
#   }
#   rownames(x)<- rownames
#   colnames(x)<- c("N","M", "SD", "N","M", "SD",
#                   "Test Statistic", "d", "p value") 
#   return(x)
# }
# 
# ### BALANCE - CATEGORICAL
# balancetable_cat <- function(cond, catvars, rownames){ ## Takes as input columns from data frame
#   library(dplyr)
#   x<- c()
#   for (b in c(1:length(catvars))){
#     x<- rbind(x,
#               (cond_diffs_categ(cond, catvars[,b]))
#     )
#   }
#   rownames(x)<- rownames
#   colnames(x)<- c("N","M", "SD", "N","M", "SD",
#                   "Test Statistic", "d", "p value") 
#   return(x)
# }
# 
# ### Program to create a balance - CONTINUOUS ONLY
# balancetable_cont <- function(cond, contvars, rownames){ ## Takes as input columns from data frame
#   library(dplyr)
#   x<- c()
#   for (a in c(1:length(contvars))){
#     x<- rbind(x,
#               (cond_diffs_cont(cond, contvars[,a]))
#     )
#   }
#   rownames(x)<- rownames
#   colnames(x)<- c("N","M", "SD","N", "M", "SD",
#                   "Test Statistic", "d", "p value") 
#   return(x)
# }
# 
# #by condition, then by OUTCOME
# balancetable.0<- function(cond, contvars, catvars, rownames){ ## Takes as input columns from data frame
#   library(dplyr)
#   x<- c()
#   for (c in c(1:length(cond))){
#   for (a in c(1:length(contvars))){
#     x<- rbind(x,
#               (cond_diffs_cont(cond[,c], contvars[,a]))
#     )
#   }
#   for (b in c(1:length(catvars))){
#     x<- rbind(x,
#               (cond_diffs_categ(cond[,c], catvars[,b]))
#     )
#   }}
#   rownames(x)<- rownames
#   colnames(x)<- c("N","M", "SD", "N","M", "SD",
#                   "Test Statistic", "d", "p value") 
#   return(x)
# }
# 
# #by OUTCOME, then by CONDITION
# balancetable.01<- function(cond, contvars, catvars, rownames){ ## Takes as input columns from data frame
#   library(dplyr)
#   x<- c()
#  
#     for (a in c(1:length(contvars))){
#       for (c in c(1:length(cond))){
#       x<- rbind(x,
#                 (cond_diffs_cont(cond[,c], contvars[,a]))
#       )
#     }}
#     for (b in c(1:length(catvars))){
#       for (c in c(1:length(cond))){
#       x<- rbind(x,
#                 (cond_diffs_categ(cond[,c], catvars[,b]))
#       )
#     }}
#   rownames(x)<- rownames
#   colnames(x)<- c("N","M", "SD", "N","M", "SD",
#                   "Test Statistic", "d", "p value") 
#   return(x)
# }
# 
# ### Program to create a balance - CONTINUOUS ONLY
# balancetable_cont.01 <- function(cond, contvars, rownames){ ## Takes as input columns from data frame
#   library(dplyr)
#   x<- c()
#   for (a in c(1:length(contvars))){
#     for (c in c(1:length(cond))){
#     x<- rbind(x,
#               (cond_diffs_cont(cond[,c], contvars[,a]))
#     )
#   }}
#   rownames(x)<- rownames
#   colnames(x)<- c("N","M", "SD","N", "M", "SD",
#                   "Test Statistic", "d", "p value") 
#   return(x)
# }
# 
# ## FUNCTION ####
# lmer.0 <- function(data,
#                    c_outcome_spec,d_treatment_spec,
#                    e_covariate1_spec,f_covariate2_spec,
#                    g_covariate3_spec,h_covariate4_spec,
#                    file_name){
#   b_value<-c()
#   p_value<-c()
#   outcome <- c()
#   treatment <- c()
#   covariate1 <- c()
#   covariate2 <- c()
#   covariate3 <- c()
#   covariate4 <- c()
#   
#   
#   for (c in c(1:length(c_outcome_spec)))
#     for (d in c(1:length(d_treatment_spec)))
#       for (e in c(1:length(e_covariate1_spec)))
#         for (f in c(1:length(f_covariate2_spec)))
#           for (g in c(1:length(g_covariate3_spec)))
#             for (h in c(1:length(h_covariate4_spec))){{{{{{
#               formula<-as.formula(paste(c_outcome_spec[c]," ~ ",
#                                         d_treatment_spec[d],
#                                         e_covariate1_spec[e],
#                                         f_covariate2_spec[f],
#                                         g_covariate3_spec[g],
#                                         h_covariate4_spec[h],"+(1|randomid)"))
#               b_value.dat<-summary(lmer(formula,data=data))$coefficients[2,1]
#               b_value<-c(b_value,b_value.dat)
#               p_value.dat<-summary(lmer(formula,data=data))$coefficients[2,5]
#               p_value<-c(p_value,p_value.dat)
#               
#               outcome <- c(outcome, c_outcome_spec[c])
#               treatment <- c(treatment, d_treatment_spec[d])
#               covariate1 <- c(covariate1, e_covariate1_spec[e])
#               covariate2 <- c(covariate2, f_covariate2_spec[f])
#               covariate3 <- c(covariate3, g_covariate3_spec[g])
#               covariate4 <- c(covariate4, h_covariate4_spec[h])
#             }}}}}}
#   
#   output_dat<-as.data.frame(cbind(outcome, treatment, covariate1,
#                                   covariate2, covariate3, covariate4,
#                                   b_value,p_value))
#   
#   
#   output_dat$covariate1<-gsub("[+]","",output_dat$covariate1)
#   output_dat$covariate2<-gsub("[+]","",output_dat$covariate2)
#   output_dat$covariate3<-gsub("[+]","",output_dat$covariate3)
#   output_dat$covariate4<-gsub("[+]","",output_dat$covariate4)
#   
#   
#   
#   write.csv(output_dat, paste0(file_name[1],Sys.Date(),".csv"))
#   
# }
# 
# lastValue <- function(x)   tail(x[!is.na(x)], 1)
# 
# ## FUNCTION ####
# lmer.int.0 <- function(data,
#                        c_outcome_spec,d_treatment_spec,moderator,
#                        e_covariate1_spec,f_covariate2_spec,
#                        g_covariate3_spec,h_covariate4_spec,
#                        file_name){
#   b_value<-c()
#   p_value<-c()
#   outcome <- c()
#   treatment <- c()
#   covariate1 <- c()
#   covariate2 <- c()
#   covariate3 <- c()
#   covariate4 <- c()
#   
#   
#   for (c in c(1:length(c_outcome_spec)))
#     for (d in c(1:length(d_treatment_spec)))
#       for (e in c(1:length(e_covariate1_spec)))
#         for (f in c(1:length(f_covariate2_spec)))
#           for (g in c(1:length(g_covariate3_spec)))
#             for (h in c(1:length(h_covariate4_spec))){{{{{{
#               formula<-as.formula(paste(c_outcome_spec[c]," ~ ",
#                                         d_treatment_spec[d],"*",
#                                         moderator,
#                                         e_covariate1_spec[e],
#                                         f_covariate2_spec[f],
#                                         g_covariate3_spec[g],
#                                         h_covariate4_spec[h],"+(1|randomid)"))
#               b_value.dat<-lastValue(summary(lmer(formula,data=data))$coefficients[,1])
#               b_value<-c(b_value,b_value.dat)
#               p_value.dat<-lastValue(summary(lmer(formula,data=data))$coefficients[,5])
#               p_value<-c(p_value,p_value.dat)
#               
#               outcome <- c(outcome, c_outcome_spec[c])
#               treatment <- c(treatment, d_treatment_spec[d])
#               covariate1 <- c(covariate1, e_covariate1_spec[e])
#               covariate2 <- c(covariate2, f_covariate2_spec[f])
#               covariate3 <- c(covariate3, g_covariate3_spec[g])
#               covariate4 <- c(covariate4, h_covariate4_spec[h])
#             }}}}}}
#   
#   output_dat<-as.data.frame(cbind(outcome, treatment,moderator, covariate1,
#                                   covariate2, covariate3, covariate4,
#                                   b_value,p_value))
#   
#   
#   output_dat$covariate1<-gsub("[+]","",output_dat$covariate1)
#   output_dat$covariate2<-gsub("[+]","",output_dat$covariate2)
#   output_dat$covariate3<-gsub("[+]","",output_dat$covariate3)
#   output_dat$covariate4<-gsub("[+]","",output_dat$covariate4)
#   
#   
#   
#   write.csv(output_dat, paste0(file_name[1],Sys.Date(),".csv"))
#   
# }
# 
# 
# lm.beta.lmer <- function(mod) {
#   b <- fixef(mod)[-1]
#   sd.x <- apply(getME(mod,"X")[,-1],2,sd)
#   sd.y <- sd(getME(mod,"y"))
#   b*sd.x/sd.y
# }
# 
# 
# # N-gram analysis frequency plot
# freq_unigramplot_generate <- function(data,freqtitle){
#   
#   #Corpus object for tm_map functions
#   vector_doc <- VectorSource(data)
#   corpus <- VCorpus(vector_doc)
#   
#   #corpus <- tm_map(corpus,  content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),  mc.cores=1)
#   corpus<-tm_map(corpus,stripWhitespace)
#   corpus<-tm_map(corpus,PlainTextDocument)
#   
#   toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x,fixed=TRUE))
#   
#   #Removing special characters and URLs
#   #corpus <- tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
#   #corpus <- tm_map(corpus, toSpace, "@[^\\s]+")
#   
#   corpus<-tm_map(corpus, content_transformer(tolower))
#   corpus<-tm_map(corpus, stripWhitespace)
#   corpus<-tm_map(corpus, removePunctuation)
#   corpus<-tm_map(corpus, removeNumbers)
#   corpus<-tm_map(corpus, removeWords, stopwords('english'))
#   #RWeka controls failed to for me with strage instant error, so I found another tokenizers:
#   BigramTokenizer <-
#     function(x)
#       unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
#   TrigramTokenizer <-
#     function(x)
#       unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
#   
#   
#   #Count words
#   freq_df <- function(tdm){
#     freq <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
#     freq_df <- data.frame(word=names(freq), freq=freq)
#     return(freq_df)
#   }
#   
#   unigram <- removeSparseTerms(TermDocumentMatrix(corpus), 0.9999)
#   unigram_freq <- freq_df(unigram)
#   
#   bigram <- removeSparseTerms(TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer)), 0.9999)
#   bigram_freq <- freq_df(bigram)
#   
#   trigram <- removeSparseTerms(TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer)), 0.9999)
#   trigram_freq <- freq_df(trigram)
#   
#   ggplot(unigram_freq[1:25,], aes(reorder(word, -freq), freq)) +
#     labs(x = "Words/Phrases", y = "Frequency") +
#     ggtitle(paste(freqtitle,"Top-25 Unigrams")) +
#     theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1)) +
#     geom_bar(stat = "identity")
# }
# 
# library(RColorBrewer)
# pal=brewer.pal(8,"Blues")
# pal=pal[-(1:3)]
# 
# freq_unigramplot_wordcloud<- function(data){
#   
#   #Corpus object for tm_map functions
#   vector_doc <- VectorSource(data)
#   corpus <- VCorpus(vector_doc)
#   
#   #corpus <- tm_map(corpus,  content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),  mc.cores=1)
#   corpus<-tm_map(corpus,stripWhitespace)
#   corpus<-tm_map(corpus,PlainTextDocument)
#   
#   toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x,fixed=TRUE))
#   
#   #Removing special characters and URLs
#   #corpus <- tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
#   #corpus <- tm_map(corpus, toSpace, "@[^\\s]+")
#   
#   corpus<-tm_map(corpus, content_transformer(tolower))
#   corpus<-tm_map(corpus, stripWhitespace)
#   corpus<-tm_map(corpus, removePunctuation)
#   corpus<-tm_map(corpus, removeNumbers)
#   corpus<-tm_map(corpus, removeWords, stopwords('english'))
#   #RWeka controls failed to for me with strage instant error, so I found another tokenizers:
#   BigramTokenizer <-
#     function(x)
#       unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
#   TrigramTokenizer <-
#     function(x)
#       unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
#   
#   
#   #Count words
#   freq_df <- function(tdm){
#     freq <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
#     freq_df <- data.frame(word=names(freq), freq=freq)
#     return(freq_df)
#   }
#   
#   unigram <- removeSparseTerms(TermDocumentMatrix(corpus), 0.9999)
#   unigram_freq <- freq_df(unigram)
#   
#   bigram <- removeSparseTerms(TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer)), 0.9999)
#   bigram_freq <- freq_df(bigram)
#   
#   trigram <- removeSparseTerms(TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer)), 0.9999)
#   trigram_freq <- freq_df(trigram)
#   wordcloud(words=unigram_freq$word,freq=unigram_freq$freq,
#             max.words=50,scale=c(2,2),random.order = F, colors=pal)
# }
# 
# freq_bigramplot_generate <- function(data,freqtitle){
#   
#   #Corpus object for tm_map functions
#   vector_doc <- VectorSource(data)
#   corpus <- VCorpus(vector_doc)
#   
#   #corpus <- tm_map(corpus,  content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),  mc.cores=1)
#   corpus<-tm_map(corpus,stripWhitespace)
#   corpus<-tm_map(corpus,PlainTextDocument)
#   
#   toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x,fixed=TRUE))
#   
#   #Removing special characters and URLs
#   #corpus <- tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
#   #corpus <- tm_map(corpus, toSpace, "@[^\\s]+")
#   
#   corpus<-tm_map(corpus, content_transformer(tolower))
#   corpus<-tm_map(corpus, stripWhitespace)
#   corpus<-tm_map(corpus, removePunctuation)
#   corpus<-tm_map(corpus, removeNumbers)
#   corpus<-tm_map(corpus, removeWords, stopwords('english'))
#   #RWeka controls failed to for me with strage instant error, so I found another tokenizers:
#   BigramTokenizer <-
#     function(x)
#       unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
#   TrigramTokenizer <-
#     function(x)
#       unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
#   
#   
#   #Count words
#   freq_df <- function(tdm){
#     freq <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
#     freq_df <- data.frame(word=names(freq), freq=freq)
#     return(freq_df)
#   }
#   
#   unigram <- removeSparseTerms(TermDocumentMatrix(corpus), 0.9999)
#   unigram_freq <- freq_df(unigram)
#   
#   bigram <- removeSparseTerms(TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer)), 0.9999)
#   bigram_freq <- freq_df(bigram)
#   
#   trigram <- removeSparseTerms(TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer)), 0.9999)
#   trigram_freq <- freq_df(trigram)
#   
#   ggplot(bigram_freq[1:25,], aes(reorder(word, -freq), freq)) +
#     labs(x = "Words/Phrases", y = "Frequency") +
#     ggtitle(paste(freqtitle,"Top-25 Bigrams")) +
#     theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1)) +
#     geom_bar(stat = "identity")
#   
# }
# 
# freq_bigramplot_wordcloud<- function(data){
#   
#   #Corpus object for tm_map functions
#   vector_doc <- VectorSource(data)
#   corpus <- VCorpus(vector_doc)
#   
#   #corpus <- tm_map(corpus,  content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),  mc.cores=1)
#   corpus<-tm_map(corpus,stripWhitespace)
#   corpus<-tm_map(corpus,PlainTextDocument)
#   
#   toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x,fixed=TRUE))
#   
#   #Removing special characters and URLs
#   #corpus <- tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
#   #corpus <- tm_map(corpus, toSpace, "@[^\\s]+")
#   
#   corpus<-tm_map(corpus, content_transformer(tolower))
#   corpus<-tm_map(corpus, stripWhitespace)
#   corpus<-tm_map(corpus, removePunctuation)
#   corpus<-tm_map(corpus, removeNumbers)
#   corpus<-tm_map(corpus, removeWords, stopwords('english'))
#   #RWeka controls failed to for me with strage instant error, so I found another tokenizers:
#   BigramTokenizer <-
#     function(x)
#       unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
#   TrigramTokenizer <-
#     function(x)
#       unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
#   
#   
#   #Count words
#   freq_df <- function(tdm){
#     freq <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
#     freq_df <- data.frame(word=names(freq), freq=freq)
#     return(freq_df)
#   }
#   
#   unigram <- removeSparseTerms(TermDocumentMatrix(corpus), 0.9999)
#   unigram_freq <- freq_df(unigram)
#   
#   bigram <- removeSparseTerms(TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer)), 0.9999)
#   bigram_freq <- freq_df(bigram)
#   
#   trigram <- removeSparseTerms(TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer)), 0.9999)
#   trigram_freq <- freq_df(trigram)
#   wordcloud(words=bigram_freq$word,freq=bigram_freq$freq,max.words=50,
#             scale=c(.8,.8),random.order = F, colors=pal)
# }
# 
# freq_trigramplot_generate <- function(data,freqtitle){
#   
#   #Corpus object for tm_map functions
#   vector_doc <- VectorSource(data)
#   corpus <- VCorpus(vector_doc)
#   
#   #corpus <- tm_map(corpus,  content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),  mc.cores=1)
#   corpus<-tm_map(corpus,stripWhitespace)
#   corpus<-tm_map(corpus,PlainTextDocument)
#   
#   toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x,fixed=TRUE))
#   
#   #Removing special characters and URLs
#   #corpus <- tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
#   #corpus <- tm_map(corpus, toSpace, "@[^\\s]+")
#   
#   corpus<-tm_map(corpus, content_transformer(tolower))
#   corpus<-tm_map(corpus, stripWhitespace)
#   corpus<-tm_map(corpus, removePunctuation)
#   corpus<-tm_map(corpus, removeNumbers)
#   corpus<-tm_map(corpus, removeWords, stopwords('english'))
#   #RWeka controls failed to for me with strage instant error, so I found another tokenizers:
#   BigramTokenizer <-
#     function(x)
#       unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
#   TrigramTokenizer <-
#     function(x)
#       unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
#   
#   
#   #Count words
#   freq_df <- function(tdm){
#     freq <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
#     freq_df <- data.frame(word=names(freq), freq=freq)
#     return(freq_df)
#   }
#   
#   unigram <- removeSparseTerms(TermDocumentMatrix(corpus), 0.9999)
#   unigram_freq <- freq_df(unigram)
#   
#   bigram <- removeSparseTerms(TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer)), 0.9999)
#   bigram_freq <- freq_df(bigram)
#   
#   trigram <- removeSparseTerms(TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer)), 0.9999)
#   trigram_freq <- freq_df(trigram)
#   
#   ggplot(trigram_freq[1:25,], aes(reorder(word, -freq), freq)) +
#     labs(x = "Words/Phrases", y = "Frequency") +
#     ggtitle(paste(freqtitle,"Top-25 Trigrams")) +
#     theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1)) +
#     geom_bar(stat = "identity")
#   
# }
# 
# freq_trigramplot_wordcloud<- function(data){
#   
#   #Corpus object for tm_map functions
#   vector_doc <- VectorSource(data)
#   corpus <- VCorpus(vector_doc)
#   
#   #corpus <- tm_map(corpus,  content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),  mc.cores=1)
#   corpus<-tm_map(corpus,stripWhitespace)
#   corpus<-tm_map(corpus,PlainTextDocument)
#   
#   toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x,fixed=TRUE))
#   
#   #Removing special characters and URLs
#   #corpus <- tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
#   #corpus <- tm_map(corpus, toSpace, "@[^\\s]+")
#   
#   corpus<-tm_map(corpus, content_transformer(tolower))
#   corpus<-tm_map(corpus, stripWhitespace)
#   corpus<-tm_map(corpus, removePunctuation)
#   corpus<-tm_map(corpus, removeNumbers)
#   corpus<-tm_map(corpus, removeWords, stopwords('english'))
#   #RWeka controls failed to for me with strage instant error, so I found another tokenizers:
#   BigramTokenizer <-
#     function(x)
#       unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
#   TrigramTokenizer <-
#     function(x)
#       unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
#   
#   
#   #Count words
#   freq_df <- function(tdm){
#     freq <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
#     freq_df <- data.frame(word=names(freq), freq=freq)
#     return(freq_df)
#   }
#   
#   unigram <- removeSparseTerms(TermDocumentMatrix(corpus), 0.9999)
#   unigram_freq <- freq_df(unigram)
#   
#   bigram <- removeSparseTerms(TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer)), 0.9999)
#   bigram_freq <- freq_df(bigram)
#   
#   trigram <- removeSparseTerms(TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer)), 0.9999)
#   trigram_freq <- freq_df(trigram)
#   wordcloud(words=trigram_freq$word,freq=trigram_freq$freq,
#             max.words=50,scale=c(.8,.8),random.order = F, colors=pal)
# }
# 
# BigramTokenizer <-
#   function(x)
#     unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
# TrigramTokenizer <-
#   function(x)
#     unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
