# survey.design <- svydesign(ids = ~TID, prob=~1, data=implist,nest=TRUE)
# fitmodel.measure<-sem(model.measure,data=cal,fixed.x = FALSE)
# surveyfitmodel.measure<-lavaan.survey(fitmodel.measure,survey.design=survey.design)
# summary(surveyfitmodel.measure,fit.measures=TRUE,rsquare=TRUE,standardized=TRUE)
