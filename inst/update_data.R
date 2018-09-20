
# Skripti, joka päivittää kaikki pakettiin kuuluvat datasetit

library(phdR2)

fi2<-AddFolder("/home/juho/phd_data/raw_data/oldresults/fi/", "fi")
fi2 <- do.call(rbind,fi2)
jpfi <- AddGroupFromJson("/home/juho/phd_data/raw_data/results/fi/jp1_SVO_quantdata.json","fi")
ru2<-AddFolder("/home/juho/phd_data/raw_data/oldresults/ru/", "ru")
ru2 <- do.call(rbind,ru2)
jpru <- AddGroupFromJson("/home/juho/phd_data/raw_data/results/ru/jp1_SVO_quantdata.json","ru")
ru2 <- rbind(ru2,jpru)
fi2 <- rbind(fi2,jpfi)

x2 <- rbind(fi2,ru2)
GetD(x2)
GetL2Collocates()
GetL1aDataForS2S3()
CreateGtabs()
GetS1Means()
GetNumericCasesInGroups()
GetPatterns()


#library(dplyr)
#load("~/phd_data/data/sents.rda")
#load("~/workprojects/phdR2/data/d.rda")
#withsents <- d %>% left_join(sents,by=c("ID","lang"))
#g <- "L2a"
#l2 <- "fi"
#thisloc <- "S1"
#ss <- subset(withsents,lang==l2&group==g&location==thisloc)
#collocates <- SimpleCollocations(ss$sent,pat,2)


