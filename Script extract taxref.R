taxref<-read.table("D:/Documents/Especes/TAXREF/TAXREF v15/TAXREFv15.txt", sep='\t', header=TRUE)

library(dplyr)
library(tidyr)

faune_aqua<-taxref %>%
  filter(REGNE=="Animalia",
         RANG=="ES",
         HABITAT==2|HABITAT==4|HABITAT==6|HABITAT==8,
         FR=="P" | FR=="E"| FR=="S" | FR=="C" |FR=="I" |FR=="J" |FR=="M") %>%
  drop_na(CD_SUP) %>%
  select(FAMILLE,GROUP1_INPN,GROUP2_INPN,GROUP3_INPN,CD_NOM,RANG,LB_NOM,NOM_VERN,HABITAT,FR) %>%
  mutate(statut_nat=if_else((FR=="I" | FR=="J" | FR=="M"),"EE","NAT")) %>%
  mutate(groupe_taxo = case_when(
    GROUP1_INPN=="Arthropodes" ~ GROUP2_INPN,
    GROUP1_INPN=="ChordÃ©s"~ GROUP2_INPN,
    GROUP1_INPN=="Mollusques"~ GROUP2_INPN,
    TRUE ~ GROUP1_INPN))

compte_NAT<-faune_aqua %>%
  filter(statut_nat=="NAT") %>%
  group_by(groupe_taxo) %>%
  summarize("Espèces_natives"=n())

compte_EE<-faune_aqua %>%
  filter(statut_nat=="EE") %>%
  group_by(groupe_taxo) %>%
  summarize("Espèces_exotiques"=n())

compte_esp<-full_join(compte_NAT,compte_EE,by="groupe_taxo")

compte_esp<-compte_esp %>%
  mutate(Total=Espèces_exotiques+Espèces_natives)%>%
  mutate(Freq_EE=Espèces_exotiques/(Espèces_exotiques+Espèces_natives)) %>%
  arrange(desc(Freq_EE))

