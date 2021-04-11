## code to prepare `Letters` dataset goes here

column_names<-c("id", "fid", "Flexion", "Lemme", "Étiquettes", "Métagraphe",
                "Metaphone2", "Notes", "Sémantique", "Étymologie", "Sous.dictionnaire",
                "Google.1.grams", "Wikipédia", "Wikisource", "Littérature", "Total.occurrences",
                "Doublons", "Multiples", "Fréquence", "Indice.de.fréquence")

x<-read.delim("~/Data_spell/lexique-grammalecte-fr-v7.0.txt",
              header = FALSE,sep="\t",skip=16,
              #encoding = "UTF-8",
              col.names =column_names)

# distinct words
x<-x%>% distinct(Flexion)

all_tokens<-x%>% filter(nchar(Flexion)>1)%>% .$Flexion%>%
  tokenizers::tokenize_characters(.,strip_non_alphanum=TRUE)

extended_tokens <- unique(unlist(all_tokens))

tokens<- c("µ", "a", "á", "à", "â", "ä", "ã","æ", "b", "c",
  "ç", "d", "e", "é", "è", "ê", "ë", "f", "g", "h", "i", "í",
  "î", "ï", "j", "k", "l", "m", "n","o", "ó",
  "ô", "ö","œ", "p", "q", "r","s", "t", "u", "ú", "ù",
  "û", "ü", "v", "w", "x", "y", "ÿ", "z")

usethis::use_data(extended_tokens,tokens,internal = TRUE,overwrite = TRUE)
usethis::use_data(tokens,tokens,internal = TRUE,overwrite = TRUE)


