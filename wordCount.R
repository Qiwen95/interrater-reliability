library(pdftools)
library(stringr)

######################
## FUNCTIONS
######################

openPDF <- function (folderName){
  # Opens multiple PDF files within a given folder
  # Args:
  #    folderName: name (string) of folder where the files are located
  #
  # Returns:
  #    files: list of files that can be analyzed using R. Input for wordCount function.
  #        The working directory must be set prior to running this code using either:
  #        - setwd()
  #        OR
  #        - file.path("folder_name", "filename.pdf")
  # 
  # Error Handling:
  #    suppressWarnings are enabled for files where 'foreign' docx bullets can
  #        yield 'Badly formatted number' warning. And specific font weights can 
  #        yield 'invalid font weight'. Both warnings do not effect the word count.
  filenames <- list.files(folderName, pattern = "*.pdf", full.names = TRUE)
  files <- suppressWarnings(lapply(filenames, pdf_text))
  return(files)
}

wordCount <- function(files, totWords){
  # Computes the number of words in a given file
  # Args:
  #    files: list of files output by openPDF function
  #    totWords: initalized total words, set to 0 for initialization
  #
  # Returns:
  #    totWords: the total number of words for all files within files list
  # 
  for (txt in files){
    txt <- removePunctuation(txt)
    txt <- str_replace(gsub("\\s+", " ", str_trim(txt)), "\n", " ")
    totWords <- totWords + sum(lengths(strsplit(txt, " ")))
  }
  return(totWords)
}

######################
## Main Execution
######################

noInterFiles <- openPDF("noInteractionFiles")
print(paste0("No interaction total words (pdf): ", wordCount(noInterFiles,0)))

interFiles <- openPDF("interactionFiles")
print(paste0("Interaction total words (pdf): ", wordCount(interFiles,0)))