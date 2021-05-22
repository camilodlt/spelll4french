# Objective 

Spell checking package for the french language in R.  
Spell4french will be used to train a seq2seq model for spelling correction.   
The results of these models will then be part of the package itself.   

At its core, spell4french tries brute force to solve spelling errors. To do that, when an spelling mistake is noticed, it tries different variations of the same word hoping that one of these variations would be a correct (and the wanted) word. 

Variations can be done in the opposite sense, that is, we start with correct words, and apply 'deletes', 'transpositions', 'splits',etc. in order to obtain an incorrect word. This can be useful for traning a model that recognize or correct spelling mistakes. 


