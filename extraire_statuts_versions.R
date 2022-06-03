library(tidyverse)
library(data.table)

# fonction qui extrait les statuts pour chacun des fichiers taxref contenu dans un répertoire
extraire_statuts_versions <- function(repertoire)
  
{
  
  versions_taxref <- list.files(path = repertoire,
                                pattern = ".txt$",
                                full.names = TRUE)
  
  # sous-fonction qui extrait les statuts pour un des fichiers text taxref
  extraire_statuts <- function(taxref_txt)
    
  {
    taxref <- data.table::fread(taxref_txt,
                                encoding = "UTF-8",
                                quote = "")
    
    faune_aqua <- taxref %>%
      filter(
        REGNE == "Animalia",
        RANG == "ES",
        HABITAT %in% c(2, 4, 6, 8),
        FR %in% c("P", "E", "S", "C", "I", "J", "M")
      ) %>%
      drop_na(CD_SUP) %>%
      select(FAMILLE,
             GROUP1_INPN,
             GROUP2_INPN,
             CD_NOM,
             RANG,
             LB_NOM,
             NOM_VERN,
             HABITAT,
             FR) %>%
      mutate(
        statut_nat = if_else(FR %in% c("I", "J", "M"),
                             true = "EE",
                             false = "NAT"),
        groupe_taxo = if_else(
          GROUP1_INPN %in% c("Arthropodes",  "Chordés", "Mollusques"),
          true = GROUP2_INPN,
          false = GROUP1_INPN
        )
      )
    
    compte <- faune_aqua %>%
      group_by(statut_nat, groupe_taxo) %>%
      tally() %>%
      pivot_wider(
        names_from = "statut_nat",
        values_from = "n",
        values_fill = 0
      ) %>%
      mutate(Total = EE + NAT,
             Freq_EE = EE / (EE + NAT),
             version = taxref_txt) %>%
      arrange(desc(Freq_EE))
    
    compte
    
  } # END extraire_statuts()
  
  purrr::map(.x = versions_taxref,
             .f = extraire_statuts) %>% 
    purrr::reduce(rbind)
  
} # END extraire_statuts_versions()

# application
resultats <- extraire_statuts_versions(repertoire = "raw_data") 

