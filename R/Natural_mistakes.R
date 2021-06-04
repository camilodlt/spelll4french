###############################################################################
#                         NATURAL ("FAT FINGERS") MISTAKES                    #
###############################################################################
# LIBRARIES ------
library(dplyr)
library(tidyr)
library(data.table)
# FAT FINGER. Keyboard Neighbors: AZERTY ------
a <- c("z","q")
b <- c("v","n","g","h"," ")
c <- c("x","v","d","f"," ")
d <- c("x","c","s","f","e","r")
e <- c("z","r","s","d")
f<- c("r","d","t","g","c","v")
g<- c("t","f","h","v","b","y")
h<- c("g","j","y","u","b","n")
i<- c("u","j","o","k")
j<- c("h","k","n","u","i",",")
k<- c("j","i","o","l",",",";")
l<- c("k","o","p","m",":",";")
m<- c("l","ù","p",":","!","^")
n<- c("b",",","h","j"," ")
o<- c("i","p","k","l")
p<- c("o","^","m","k")
q<- c("s","a","w","z","<")
r<- c("e","t","d","f")
s<- c("q","d","z","e","w","x")
t<- c("r","y","f","g")
u<- c("y","i","h","j")
v<- c("c","b","f","g"," ")
w<- c("x","<","x","q")
x<- c("w","c","s","d"," ")
y<- c("t","u","g","h")
z<- c("a","e","q","s")

# special accents
e_tilde<- c("&","\"","z","a") # é

a_tilde_<- c("ç",")","o","p") # à
e_tilde_<- c("-","_","y","u") # è
u_tilde_ <- c("m","*","^","!","$") #ù
c_cedille<- c("_","à","i","o") # ç

u_dierese<- c("ÿ","ï") # ü
i_dierese<- c("ü","ö") # ï
o_dierese<- c("ï") # ö

# Turn into a DF: Assign neighbors to tokens ------
natural_dict <-data.frame(letters=tokens)
natural_dict$neighbor<- NA_character_
natural_dict$neighbor[natural_dict$letters=="a"]<- list(a)
natural_dict$neighbor[natural_dict$letters=="à"]<- list(a_tilde_)
natural_dict$neighbor[natural_dict$letters=="b"]<- list(b)
natural_dict$neighbor[natural_dict$letters=="c"]<- list(c)
natural_dict$neighbor[natural_dict$letters=="ç"]<- list(c_cedille)
natural_dict$neighbor[natural_dict$letters=="d"]<- list(d)
natural_dict$neighbor[natural_dict$letters=="e"]<- list(e)
natural_dict$neighbor[natural_dict$letters=="é"]<- list(e_tilde)
natural_dict$neighbor[natural_dict$letters=="è"]<- list(e_tilde_)
natural_dict$neighbor[natural_dict$letters=="f"]<- list(f)
natural_dict$neighbor[natural_dict$letters=="g"]<- list(g)
natural_dict$neighbor[natural_dict$letters=="h"]<- list(h)
natural_dict$neighbor[natural_dict$letters=="i"]<- list(i)
natural_dict$neighbor[natural_dict$letters=="ï"]<- list(i_dierese)
natural_dict$neighbor[natural_dict$letters=="j"]<- list(j)
natural_dict$neighbor[natural_dict$letters=="k"]<- list(k)
natural_dict$neighbor[natural_dict$letters=="l"]<- list(l)
natural_dict$neighbor[natural_dict$letters=="m"]<- list(m)
natural_dict$neighbor[natural_dict$letters=="n"]<- list(n)
natural_dict$neighbor[natural_dict$letters=="o"]<- list(o)
natural_dict$neighbor[natural_dict$letters=="p"]<- list(p)
natural_dict$neighbor[natural_dict$letters=="q"]<- list(q)
natural_dict$neighbor[natural_dict$letters=="r"]<- list(r)
natural_dict$neighbor[natural_dict$letters=="s"]<- list(s)
natural_dict$neighbor[natural_dict$letters=="t"]<- list(t)
natural_dict$neighbor[natural_dict$letters=="u"]<- list(u)
natural_dict$neighbor[natural_dict$letters=="ù"]<- list(u_tilde_)
natural_dict$neighbor[natural_dict$letters=="ü"]<- list(u_dierese)
natural_dict$neighbor[natural_dict$letters=="v"]<- list(v)
natural_dict$neighbor[natural_dict$letters=="w"]<- list(w)
natural_dict$neighbor[natural_dict$letters=="x"]<- list(x)
natural_dict$neighbor[natural_dict$letters=="y"]<- list(y)
natural_dict$neighbor[natural_dict$letters=="z"]<- list(z)

# INFO PRINTING ------
# Print which characters have no neighbors ---
print("NO NEIGHBORS FOR :")
natural_dict%>% filter(is.na(neighbor))%>%.$letters
# Print which characters Do have no neighbors ---
print("NEIGHBORS FOR :")
natural_dict%>% filter(!is.na(neighbor))%>%.$letters

# Unnest tibble -------
natural_dict_tibble <- natural_dict%>% as_tibble()
natural_dict_unnested<- natural_dict%>% unnest(neighbor)%>%
  filter(!is.na(neighbor))

natural_dict_unnested_DT<- as.data.table(natural_dict_unnested)
