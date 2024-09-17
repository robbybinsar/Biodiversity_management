
#read in Datasets
birds.matched<-read.csv("birds.matched.csv")
birdnames.matched<-read.csv("birdnames.matched.csv")
gtalo_birds<-read.csv("Gorantalo_Point_Pivot.csv")

#View data
View(birdnames.matched)
View(birds.matched)

#Take fieldnames from those used in Gorantalo dataset
gtalo_birds_frame<-as.data.frame(cbind(colnames(gtalo_birds)[2:102], 1:101))
colnames(gtalo_birds_frame)<-c("fieldname", "ID")

#Merge species ranges from matched dataset
gtalo_birds_metad<-left_join(gtalo_birds_frame, birds.matched, by="fieldname")

#Read in Tobias traits dataset
tobias.traits<-read.csv("tobias.translate.csv")
colnames(tobias.traits)[7]<-"fieldname"
left_join(gtalo_birds_frame, tobias.traits, by="fieldname")
Eaton_latin_convert<-read.csv("Eaton latin names conversion.csv")

View(left_join(gtalo_birds_frame, Eaton_latin_convert, by="fieldname"))
Eaton_guide<-read.csv("Eaton_guide.csv")
Eaton_guide$Common.Name
colnames(Eaton_latin_convert)
library(stringr)
Eaton_guide_cnames<-str_to_title(tolower(Eaton_guide$Common.Name))

Eaton_guide_cnames<-gsub(" ", ".", Eaton_guide_cnames)
Eaton_guide_cnames[which(endswith(Eaton_guide_cnames,'..')==TRUE)]<-substr(Eaton_guide_cnames[which(endswith(Eaton_guide_cnames,'..')==TRUE)],1,nchar(Eaton_guide_cnames[which(endswith(Eaton_guide_cnames,'..')==TRUE)])-2)
Eaton_guide_cnames[which(endswith(Eaton_guide_cnames,'.')==TRUE)]<-substr(Eaton_guide_cnames[which(endswith(Eaton_guide_cnames,'.')==TRUE)],1,nchar(Eaton_guide_cnames[which(endswith(Eaton_guide_cnames,'.')==TRUE)])-1)

Eaton_guide$fieldname<-gsub("-", ".", Eaton_guide_cnames) 
gtalo_birds_eaton<-left_join(gtalo_birds_frame, Eaton_guide2, by="fieldname")
Eaton_guide2<-Eaton_guide[,c(7:11,13:31)]
gtalo_birds_frame$fieldname<-gsub(" ", ".", str_to_title(gsub("[.]"," ", gtalo_birds_frame$fieldname)))
write.csv(gtalo_birds_eaton, "gtalo_birds_eaton.csv")
write.csv(Eaton_guide2, "Eaton_guide_fieldnames.csv")
gtalo_birds_manualedit<-read.csv("gtalo_birds_eaton.csv")
Eaton_guide2$fieldname2<-Eaton_guide2$fieldname
Gtalo_birds_v2<-left_join(gtalo_birds_manualedit, Eaton_guide2, by="fieldname2")
write.csv(Gtalo_birds_v2, "Gtalo_birds_v2.csv")

Gtalo_final<-read.csv("Gtalo_birds_eaton_final.csv")
tobias.trait()
colnames(Gtalo_final)[9]<-"latin"
Gtalo_final$latin<-gsub(" ", "_", Gtalo_final$latin)
Gtalo_final<-left_join(Gtalo_final, tobias.traits, by="latin")
Gtalo_final<-cbind(Gtalo_final, gtalo_birds_frame$fieldname)
colnames(Gtalo_final)[50]<-"fieldname"
Gtalo_final<-left_join(Gtalo_final, tobias.traits, by="fieldname")
write.csv(Gtalo_final, "Gtalo_final.csv")
Gtalo_clean<-read.csv("Gtalo_final.csv")
Gtalo_clean
birdnames.matched
birdnames.matched$latin<-gsub(" ", "_", birdnames.matched$latin)
write.csv(left_join(Gtalo_clean, birdnames.matched, by="latin"), "Gtalo_clean.csv")
Gtalo_clean$latin
birdnames.matched$latin

Gtalo_new<-read.csv("Gtalo_clean.csv")
Tobias.orig<-read.csv("rstb20190012_si_002 tobias.csv")
library(dplyr)
colnames(Tobias.orig)[1]<-"latin"
colnames(Gtalo_new)
Gtalo_new<-left_join(Gtalo_new, Tobias.orig, by="latin")
write.csv(Gtalo_new, "Gtalo_new.csv")
Gtalo_final<-read.csv("Gtalo_new.csv")
Gtalo_final<-cbind(Gtalo_final, gtalo_birds_frame$fieldname)
write.csv(Gtalo_final, "Gorantalo_bird_metadata_final.csv")
gtalo_birds_frame$fieldname
