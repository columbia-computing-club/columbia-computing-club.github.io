
#############################################################################
# Split, Match, Replace, and other ways to Juggle Multiple Data Types in R  #
# Author: Margaret Hannum                                                   #
# 10/9/2017                                                                 #
# Columbia Biostatistics Computing Club Seminar                             #
#############################################################################

#Goal: Be able to split, match, replace, and subset your data using pattern recognition in r!

#############
# substr    #
#############
#Use Substring if you have a consistent, specific length in your string that you want to grab
x <- c("asfef", "qwerty", "yuiop[.yucky", "b", "stuff.blah.yuck")
substr(x, 1, 1)
#Now try grabbing different parts (first element only, elements 3-7)

#But what if you want to grab a pattern that varies, or don't know where in the string it is? 
#Or only want the part of a string before or after a certain character, like "."?

###################
# grep, sub, gsub #
###################
#These functions search for patterns using regular expressions
?regex
#Regular expressions cheat sheet https://www.rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf
#Another cheat sheet [http://www.robelle.com/smugbook/regexpr.html] 

#grep simply searches for matches to a pattern
grep("y", x) #Searches for "y" and returns index of items that had a match
x[5] #print the 5th element of x
#What if you want to print all elements that contain a certain pattern? Use grep as your index
x[grep("y", x)] #use grep index to print components that contain the pattern
x[grep("\\.", x)] #Finds "." and returns vector of indices that had a match, can also invert = TRUE
#In R, use double \\ to escape the regex meaning of symbols (eg usually "." means to match a character of any value)

grepl("\\.", x) #grepl returns logical, can be used in functions that require logical

#Now try to replace the periods with spaces
#sub replaces FIRST occurence of a pattern in a string
#gsub performs replacement of ALL matches after searching for matches to a pattern
sub("\\.", " ", x) #Replaces first instance with a space
gsub("\\.", " ", x) #Global sub: Replaces ALL instances
#now practice replacing all "a" with "b" 
#practice when a word starts with "a" replace that "a" with "b" (hint: use ^)
#practice replacing when a phrase ends in "yuck" change "yuck" to "yum" (hint: use $)

#############
# strsplit  #
#############
#Split elements of a character vector into substrings
##input has to be character vector, else it will be coerced to be a character

# split x on the period
strsplit(x, "\\.", fixed = FALSE) #fixed = FALSE means you're using regular expressions
strsplit(x, ".", fixed = TRUE) #fixed = TRUE means it'll look for whatever you specify, it escapes from regex
#now try splitting on "[" and "e" to see what happens

#What if you want to discard everthing after your split? Use sapply!
sapply(strsplit(x, "\\.", fixed = FALSE), "[", 1) #works through entire vector

##########
# Subset #
##########
#Returns subset that meets conditions you specify (logical expression or selection)
#Application: can use subset to match to an annotation file, subset samples to match datasets etc
n <- c("asfef", "yuiop[.yucky", "additional") #new list with common elements to x
subset(x, x %in% n) #Subset of x where elements match with elements in y

#Use grepl with subset
subset(x, grepl("y", x)) #subset of x where elements contain "y" anywhere
#try subsetting where y is at the beginning (^), end ($), search for multiple ([ya])


#Cheat sheet: Logical operators http://www.statmethods.net/management/operators.html
#Note if you want to subset based on matching a character vector, use == instead of =
  #e.g subset(x, x == "b")

#Clear space

##############################################################
# Applied example using gene expression and copy number data #
# Public data acquired from The Cancer Genome Atlas          #
# Data reduced for size constraints                          #
##############################################################

#set working directory to the location where you saved the data
setwd("/Users/TinyDragon/Desktop/CBCC Presentation/Data")
#First explain What is an RData file- how do you make it, how do you load it?
load("mrna.reduced.RData")
head(mrna[,200:210])
#Examine the data. What do the row names look like? 
#How about the column names?
#What class is the data? Why can we check one column and know the class of the whole matrix?

#Load Selected Gene Expression Features from top 1000 probes (selected from positive correlation)
ge.feat <- read.csv("Selected GE Features.csv")
#This is called an "annotation file" - allows you to move between data types
#Goal is to get small subset of gene expression (mrna) data (which has 16K) from selected gene features
#What is the maximum amount of genes we should have after the match?


#Explain .txt file and read.table
#Now load copy number data
#Getting CNA Segments in matrix form
#Want to fill missing values with NA, use first column as rownames, specify that header is true
#cna.seg <- read.table("all_data_by_genes.txt", fill = TRUE, row.names = 1, header = TRUE)
cna.seg <- read.table("cna.seg.reduced.txt", fill = TRUE, row.names = 1, header = TRUE)
#notice that because we told it the column names, it classified the columns appropriately. Good to check this though, esp if you have requirements on class later.
#What would have happened to the classes if I didn't tell it to read in the header?
#Why couldn't I read in the full txt file as a matrix? What would have happened to the column classes?

#I want my data in a matrix, and I don't need the first four columns
cna.seg.m <- data.matrix(cna.seg[-(1:4)])

#now lets look at the column names here versus the ones in the mrna file--- they don't match!
#Which function could we use here?

#Split column names and discard information after "|"
#Here we also use sapply to apply a function over entire matrix - 
#here we tell it to split the column names on the "|" symbol 
#discard whatever is after the split ("[["), and repeat that process just once
colnames(mrna) <- sapply(strsplit(colnames(mrna), "|", fixed = TRUE), '[[', 1)


##################################
# Match gene expression features #
##################################
mrna <- t(mrna) #transpose because we want to subset on the rowwise basis
mrna.cn.select <- subset(mrna, rownames(mrna) %in% ge.feat$Gene_symbol)
#mrna.df <- as.data.frame(t(mrna)) #here's how you would go about making a data frame if you needed that format
#mrna.df <-subset(mrna.df, rownames(mrna.df) %in% ge.feat$Gene_symbol)
 

#Here we want to just have a part of the string- the first 12 characters
colnames(cna.seg.m) <- substr(colnames(cna.seg.m),1,12)
#One last problem with our data: the column names have a . instead of a -
#Have to sub - for . so we can match colnames with mrna data
colnames(cna.seg.m) <- gsub("\\.", "-", colnames(cna.seg.m))

#Match CNA features with ones selected on gene expression
cna.select <-subset(cna.seg.m, rownames(cna.seg.m) %in% rownames(mrna.cn.select)) ##change mrna to mrna.cn.select
mrna.cn.select <-subset(mrna.cn.select, rownames(mrna.cn.select) %in% rownames(cna.select))

#Now that we have matching column titles we can create subsets of matching samples.
#We have to TRANSPOSE first since R prefers to work row by row. 

###
#Now we need to match the samples, since cna has 1080 but mrna has 960
#transpose matrices since subset works on rows
cna.select <- t(cna.select)
mrna.cn.select <- t(mrna.cn.select)
cna.select <-subset(cna.select, rownames(cna.select) %in% rownames(mrna.cn.select))
#Order the column names to have same order in both matrices
cna.select <- cna.select[,order(colnames(cna.select))] 
mrna.cn.select <- mrna.cn.select[,order(colnames(mrna.cn.select))]
#Now you should have two 60x542 matrices with matching features and samples!

#Combine matrices into list after preparation, as may be required by algorithm
tcga.list <- list(cna.select, mrna.cn.select)

############################################
# How to save prepared data as .RData file #
############################################
#setwd("C:/Users/hannumm/Documents/CBCC/Data")
#save(mrna.cn.select, file = "mrna.cn.select.RData") 
#Sometimes if you have been working on prepping a lot of datasets and want to save the whole workspace
#save.image(file = ...) #to save entire workspace to load later


#########################################################################
# Advanced use of grep & strsplit to modify methylation annotation file #
#########################################################################
#Motivation: we want all unique genenames for each methylation probe 
#Some probes are for multiple genes, and in our file genes are separated by ";", which is inconvenient

#Load methylation annotation file
load("dmrcpgs.RData") #out file
out$genename[241:250] #You can see that the genename vector is very messy, with inconsistent ";" and multiple values between

#Let's find an Index where there are no multiples
length(grep(pattern = ";", x = out$genename, value = FALSE, invert = FALSE))
length(complete.cases(out$genename)) #checking if there are NA's
#There are 27K sites that have more than one gene (are separated by ";" in the genename column)
#There are no missing values but I'll show you what to do if you need to remove them

#First we're Splitting annotation file into two, one df with multiple gene names, one without
#Because we need to do different manipulation on each! 

#First make new df with the rows that have ";" in the genename column
out.split <- out[grep(pattern = ";", x = out$genename, value = FALSE),] 
#Split the gene symbols with more than one gene and make a list out of the results
split <- strsplit(out.split$genename, ";")

#Now Repeat all elements of other rows by number of splits, make new df 
#****Below, check out "unlist(split)! This is the big kahuna and the reason we did the whole thing
#For each other variable, we are saying, repeat the elements of VECTOR the amount of times in the length of the split list
out.split.rep <- data.frame(CHR = rep(out.split$CHR, sapply(split, length)), IlmnID = rep(out.split$IlmnID, sapply(split, length)),  address = rep(out.split$address, sapply(split, length)), genename = unlist(split))

#Now make df WITHOUT multiple gene symbols and remove ones with NA gene symbol
#We also want a df without multiples since we will append it later to our split
out.single <- out[grep(pattern = ";", x = out$genename, value = FALSE, invert = TRUE),]  #invert = TRUE is how we're selecting without ";"
out.single <- out.single[,-2] #subtracting second column (MAPInfo) since I didn't need it
out.single <- out.single[complete.cases(out.single[,4]),] #Getting rid of NA's by just selecting the complete cases
#Combine cpgs with multiple genes and single genes
out.split.final <- rbind(out.split.rep, out.single)

#We can look at the number of unique values using length and unique functions to see what we added!
length(unique(out.single$genename)) #We started with 3235 unique genes in single form without ";"
length(unique(out.split.rep$genename)) #4209 unique genes After splitting ";" in our df with multiple
length(unique(out.split.final$genename)) #5377 unique genes After combining single and split 

#########################################################################
# Now Match methylation probes with the out.split.final annotation file #
#########################################################################
load("meth.reduced.RData") #load methylation data- just 3 samples since there are so many features it was a huge file!!

#Messy column names again
#Remember how to remove the unwanted characters? substr!
colnames(meth) <- substr(colnames(meth),1,12)

#Subset gene expression and meth data with each other to match gene names 
#(this was why I need to split annotation file, to be sure all unique gene names are able to be matched!)
#mrna <- t(mrna) #Check dimensions in mrna, transpose if necessary
mrna.meth.select <- subset(mrna, rownames(mrna) %in% out.split.final$genename) #4409 genes that are also in methylation data
annot.select <- subset(out.split.final, out.split.final$genename %in% rownames(mrna.meth.select)) #69491K probes that match with genes in our expression data
meth.select <- subset(meth, rownames(meth) %in% annot.select$IlmnID) #51K probes match (reduced from 480K)

##########################################################
#Subset methylation and expression based on sample names #
##########################################################
###Unfortunately the examples I provided do not have matching samples, but you would go about it the same way as before
#Have to transpose first since subset works on rows
#mrna.meth.select <- t(mrna.meth.select)   
#meth.select <- t(meth.select)
#mrna.meth.select <- subset(mrna.meth.select, rownames(mrna.meth.select) %in% rownames(meth.select))
#meth.select <- subset(meth.select, rownames(meth.select) %in% rownames(mrna.meth.select))
###Matching sample names only left 663 samples??   
#Transpose back    
#mrna.meth.select <- t(mrna.meth.select)
#meth.select <- t(meth.select)

#Order columns
#mrna.meth.select <- mrna.meth.select[,order(colnames(mrna.meth.select))]
#meth.select <- meth.select[,order(colnames(meth.select))]

###############################
# Some more advanced examples #
###############################

#For those adept with the terminal, strsplit is akin to using egrep
# Example: 'egrep -v “^#” test.vcf | cut -f8 | cut  -d ‘;’ -f2  | cut –d ‘=’ –f2 >> test.dp.txt' 
#specifies column 8, cuts at depth where it finds ‘;’, prints the second split, 
#then cuts at "=" and prints the second split, saving output as a new .txt file
#(in this case "|" act as "%>%" pipes)

#What if you have huge data that you want to read in column by column instead of row-by-row? 
#fread function allows you to quickly read in by multiple specifications
#First 5 columns of full sample, which gives cg id, gene name, and coordinates
#library(fread)
#meth.genename <- fread("BRCA.methylation__humanmethylation450__jhu_usc_edu__Level_3__within_bioassay_data_set_function__data.data.txt", 
#                          header = TRUE, skip = 1, select = 1:5, data.table = FALSE)
  
#can get fancy and read in just certain columns if you need to
#meth.f.100 <- fread("BRCA.methylation__humanmethylation450__jhu_usc_edu__Level_3__within_bioassay_data_set_function__data.data.txt", 
#                       header = TRUE, 
#                       skip = 1, #this was done to preserve the correct classes, otherwise fread interprets everything as factors
#                       #select = 1:3541, #total number of columns in the data set
#                       select = 1:401,
#                       #data.table = FALSE)[c(TRUE, rep(c(TRUE, FALSE, FALSE, FALSE),885))]  #To make subset of betas (First column is cpg names. Subsequent Column order was originally Beta, Gene Symbol, Chromosome, Gene Coordinate for each sample, so I made last three FALSE.)
#                       data.table = FALSE)[c(TRUE, rep(c(TRUE, FALSE, FALSE, FALSE),100))]


#Reduced sets
#mrna <- tcga.mrna[1:60,-(1:13)]
#save(mrna, file = "/Users/TinyDragon/Desktop/CBCC Presentation/Data/mrna.reduced.RData")

#cna.seg <- cna.seg[,1:150]
#write.table(cna.seg, "/Users/TinyDragon/Desktop/CBCC Presentation/Data/cna.seg.reduced.txt", sep = "\t")
