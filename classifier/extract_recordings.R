library(warbleR)
library(suwo)

output <- query_xc(qword = "Centropus bengalensis cnt:indonesia loc:sulawesi", download = T, path = "C:/Users/Robby Butarbutar/OneDrive - University of Kent/PhD/Field Data/Classifier/1_train_data/lesser.coucal" )
write.csv(output, "C:/Users/Robby Butarbutar/OneDrive - University of Kent/PhD/Field Data/Classifier/1_train_data/lesser.coucal/centropus_bengalensis.csv")
