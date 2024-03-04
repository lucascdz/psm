# process XML alignment files and calculate kappa
# pressupose source-language

require(xml2)
require(rvest)
require(tidyr)
require(stringr)
require(tidyverse)

# argument: XML file (downloaded from Ugarit; if d. from Alpheios, check empty refs)
# XMLPath <- '/Users/lucascdz/FILES/doutorado/projetos/mutiraoDC/alignment/data/Justino_18_4-6_lat-por_alignment_ugarit.xml'

# function

GetLinkTypes <- function(XMLPath){

   XMLAnnotator1 <- read_xml(XMLPath)

   # function for extracting data from XML
   GetPairs <- function(WdsList){

      TokensWordIDs <- WdsList %>% rvest::html_elements('w') %>% xml_attr(., 'n')
      TokensWordForms <- WdsList %>% rvest::html_elements('w') %>% rvest::html_elements('text') %>% xml_text(.)
      TokensWordRefs <- WdsList %>% rvest::html_elements('w') %>% rvest::html_elements('refs') %>% xml_attr(., 'nrefs')

      TokensDF <- data.frame(tokenId=TokensWordIDs,tokenForm=TokensWordForms,tokenRefs=TokensWordRefs, stringsAsFactors = F)

      return(TokensDF)
   }

   # get pairs (= tokens L1 x refs L2)
   TokensL1DF <- GetPairs(xml_child(xml_child(XMLAnnotator1, 3), 1))
   colnames(TokensL1DF) <- unlist(lapply(seq_along(colnames(TokensL1DF)), function(i) colnames(TokensL1DF)[i] <- paste0('L1',colnames(TokensL1DF)[i])))

   # assign link types to target-language (based on refs 'duplicates')
   TokensL1DF$linkTypeSource <- '1'
   for(i in seq_along(TokensL1DF$L1tokenId)){
      if(nrow(TokensL1DF[TokensL1DF$L1tokenRefs==TokensL1DF$L1tokenRefs[i],]) > 1){
         if(!identical(TokensL1DF$L1tokenRefs[i],'')){
            TokensL1DF$linkTypeSource[i] <- 'N'
         }
      }
   }

   # assign link types to target-language (based on 'space' in refs)
   TokensL1DF$linkTypeTarget <- ''
   for(i in seq_along(TokensL1DF$L1tokenId)){
      if(identical(TokensL1DF$L1tokenRefs[i], '')) {
         TokensL1DF$linkTypeTarget[i] <- '0'
      } else if(str_detect(TokensL1DF$L1tokenRefs[i], '\\s')){
         TokensL1DF$linkTypeTarget[i] <- 'N'
      } else {
         TokensL1DF$linkTypeTarget[i] <- '1'
      }
   }

   TokensL1DF$linkType <- unlist(lapply(seq_along(TokensL1DF$L1tokenId), function(i) paste0(TokensL1DF$linkTypeSource[i],'-',TokensL1DF$linkTypeTarget[i])))

   return(TokensL1DF)

}

# arguments
Annotator1XmlPath <- '/Users/lucascdz/FILES/doutorado/projetos/mutiraoDC/alignment/guidelines_paper/files/ugarit-aligner--Annotator1-version1.xml'
Annotator2XmlPath <- '/Users/lucascdz/FILES/doutorado/projetos/mutiraoDC/alignment/guidelines_paper/files/ugarit-aligner--Annotator2-version1.xml'

Annotator1DataDF <- GetLinkTypes(Annotator1XmlPath)
colnames(Annotator1DataDF) <- unlist(lapply(seq_along(colnames(Annotator1DataDF)), function(i) colnames(Annotator1DataDF)[i] <- paste0('A1_',colnames(Annotator1DataDF)[i])))
colnames(Annotator1DataDF)[1] <- 'Id'

Annotator2DataDF <- GetLinkTypes(Annotator2XmlPath)
colnames(Annotator2DataDF) <- unlist(lapply(seq_along(colnames(Annotator2DataDF)), function(i) colnames(Annotator2DataDF)[i] <- paste0('A2_',colnames(Annotator2DataDF)[i])))
colnames(Annotator2DataDF)[1] <- 'Id'

ParallelDataDF <- left_join(Annotator1DataDF,Annotator2DataDF)

# get combinations (DF subsets)
count_1_1_1_1 <- nrow(ParallelDataDF[ParallelDataDF$A1_linkType=='1-1' & ParallelDataDF$A2_linkType=='1-1',])
count_1_1_1_N <- nrow(ParallelDataDF[ParallelDataDF$A1_linkType=='1-1' & ParallelDataDF$A2_linkType=='1-N',])
count_1_1_N_1 <- nrow(ParallelDataDF[ParallelDataDF$A1_linkType=='1-1' & ParallelDataDF$A2_linkType=='N-1',])
count_1_1_N_N <- nrow(ParallelDataDF[ParallelDataDF$A1_linkType=='1-1' & ParallelDataDF$A2_linkType=='N-N',])

count_1_N_1_1 <- nrow(ParallelDataDF[ParallelDataDF$A1_linkType=='1-N' & ParallelDataDF$A2_linkType=='1-1',])
count_1_N_1_N <- nrow(ParallelDataDF[ParallelDataDF$A1_linkType=='1-N' & ParallelDataDF$A2_linkType=='1-N',])
count_1_N_N_1 <- nrow(ParallelDataDF[ParallelDataDF$A1_linkType=='1-N' & ParallelDataDF$A2_linkType=='N-1',])
count_1_N_N_N <- nrow(ParallelDataDF[ParallelDataDF$A1_linkType=='1-N' & ParallelDataDF$A2_linkType=='N-N',])

count_N_1_1_1 <- nrow(ParallelDataDF[ParallelDataDF$A1_linkType=='N-1' & ParallelDataDF$A2_linkType=='1-1',])
count_N_1_1_N <- nrow(ParallelDataDF[ParallelDataDF$A1_linkType=='N-1' & ParallelDataDF$A2_linkType=='1-N',])
count_N_1_N_1 <- nrow(ParallelDataDF[ParallelDataDF$A1_linkType=='N-1' & ParallelDataDF$A2_linkType=='N-1',])
count_N_1_N_N <- nrow(ParallelDataDF[ParallelDataDF$A1_linkType=='N-1' & ParallelDataDF$A2_linkType=='N-N',])

count_N_N_1_1 <- nrow(ParallelDataDF[ParallelDataDF$A1_linkType=='N-N' & ParallelDataDF$A2_linkType=='1-1',])
count_N_N_1_N <- nrow(ParallelDataDF[ParallelDataDF$A1_linkType=='N-N' & ParallelDataDF$A2_linkType=='1-N',])
count_N_N_N_1 <- nrow(ParallelDataDF[ParallelDataDF$A1_linkType=='N-N' & ParallelDataDF$A2_linkType=='N-1',])
count_N_N_N_N <- nrow(ParallelDataDF[ParallelDataDF$A1_linkType=='N-N' & ParallelDataDF$A2_linkType=='N-N',])

# calculate kappa

TOTAL <- sum(count_1_1_1_1,count_1_1_1_N,count_1_1_N_1,count_1_1_N_N,count_1_N_1_1,count_1_N_1_N,count_1_N_N_1,count_1_N_N_N,count_N_1_1_1,count_N_1_1_N,count_N_1_N_1,count_N_1_N_N,count_N_N_1_1,count_N_N_1_N,count_N_N_N_1,(count_N_N_N_N/2))

PO <- sum(count_1_1_1_1,count_1_N_1_N,count_N_1_N_1,(count_N_N_N_N/2))/TOTAL

PA__1_1 <- (nrow(ParallelDataDF[ParallelDataDF$A1_linkType=='1-1',])/TOTAL)*(nrow(ParallelDataDF[ParallelDataDF$A2_linkType=='1-1',])/TOTAL)
PA__1_N <- (nrow(ParallelDataDF[ParallelDataDF$A1_linkType=='1-N',])/TOTAL)*(nrow(ParallelDataDF[ParallelDataDF$A2_linkType=='1-N',])/TOTAL)
PA__N_1 <- (nrow(ParallelDataDF[ParallelDataDF$A1_linkType=='N-1',])/TOTAL)*(nrow(ParallelDataDF[ParallelDataDF$A2_linkType=='N-1',])/TOTAL)
PA__N_N <- (nrow(ParallelDataDF[ParallelDataDF$A1_linkType=='N-N',])/2/TOTAL)*(nrow(ParallelDataDF[ParallelDataDF$A2_linkType=='N-N',])/2/TOTAL)

PA <- sum(PA__1_1,PA__1_N,PA__N_1,PA__N_N)

kappa <- (PO-PA)/(1-PA)

# create the matrix
KappaMatrixDF <- data.frame(x=c('A2_1-1','A2_1-N','A2_N-1','A2_N-N'), A1_1_1=c(count_1_1_1_1,count_1_1_1_N,count_1_1_N_1,count_1_1_N_N), A1_1_N=c(count_1_N_1_1,count_1_N_1_N,count_1_N_N_1,count_1_N_N_N), A1_N_1=c(count_N_1_1_1,count_N_1_1_N,count_N_1_N_1,count_N_1_N_N), A1_N_N=c(count_N_N_1_1,count_N_N_1_N,count_N_N_N_1,count_N_N_N_N))


