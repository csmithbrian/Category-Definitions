setwd("T:/Users/BrianS/Projects/Category Definitions/")
#Author - Brian Smith 
#email - bcsmith@healthstream.com

#README
#You will need R to run this script. Download here: https://cran.r-project.org/
#You will need four CSV files to run this script:
#   1 - An extract of PM data with question and category info in a very specific format: see older iterations of the extract such as QCategories_04042016.csv for details. Also see Mingle 10380 in research support. A systems developer can create this extract, at time of writing that is Peter Edenfield.
#   2 - A Master category definition file with the category template for all survey types. See BriansMasterCategoryDefinitionList.csv for details.
#   3 - A crosswalk or 'dictionary' to specify which Survey Type (Master definition file format) each Survey ID should be compared to.
#   4 - A Survey ID whitelist which tells the script to skip certain survey IDs (mostly if they are determined to be custom surveys for specific clients)
#   You do not want to rebuild the whitelist each QA period. Find the old whitelist file and add onto it incrementally.
#   Also note that all of these file MUST be in CSV format. You will probably build or receive several of them in Excel so make sure that you convert them to CSV in Excel.
#The top line of the file sets the working directory for the script. If you are not working out of the specified folder (unlikely) you should change it to make your life easier. It will set where the output file is generated and the default folder for selecting data files.
#Expected output is a few hundred rows. Each row is either a missing category, extra category, missing question, extra question (within a particular category definition) or duplicate question within one survey ID+Survey type ID
#If you want to QA only one Survey ID, you can create a subset of the PM data extract in csv format and feed the script that file when it asks for the PM data extract file.




#Script notes
#4-19 - added whitelist function, added additional categories output, and reduced output of additional questions if they are part of a non-standard category. Total output currently under 400 lines using 04042016 extract
#Exception handling: blank QalphaIDs?

#File Select
CSVFilter <- matrix(ncol = 2)
CSVFilter[1,1] <- "CSV";CSVFilter[1,2] <- "*.csv"
definitions <- read.csv(choose.files(default = "BriansMasterCategoryDefinitionList.csv",multi = FALSE,filters = CSVFilter,caption = "Select Master Category Definition List"))
pmdata <- read.csv(choose.files(multi = FALSE,caption = "Select PM Data Extract File",filters = CSVFilter))
whitelist <- read.csv(choose.files(default = "survey_whitelist.csv",multi = FALSE,filters = CSVFilter,caption = "Select Whitelist File"))
category_def_dictionary<- read.csv(choose.files(default = "Survey ID - Category Definition Dictionary.csv",multi = FALSE,filters = CSVFilter,caption = "Select Survey Dictionary File"))

#Old - explicit read csvs rather than interactive file select
#Whitelist File
#whitelist <- read.csv("survey_whitelist.csv")
#whitelist <- whitelist[,1]
#Definitions File
#definitions <- read.csv("BriansMasterCategoryDefinitionList.csv")
#Extract File
#<- read.csv("QCategories_04042016.csv") #Placeholder for testing. Replace w/ select file
#SurveyID to Survey Definition Crosswalk
#category_def_dictionary <- read.csv("Survey ID - Category Definition Dictionary.csv") #Placeholder for testing. Replace w/ select file

pmdata$uniqueID <- paste(pmdata$SurveyID,pmdata$SurveyTypeID,sep = "_")

#Create null table w/ SURVID_SURVTYPE, SURVDEF, CATEGORY, MISSING_CAT, MISSING_QUES, ADDITIONAL_QUES, DUPLICATE_QUES
QA_Table <- data.frame(SURVID_SURVTYPE=character(),SURVDEF=character(),CATEGORY=character(),MISSING_CAT=character(),ADDTL_CAT=character(),MISSING_QUES=character(),ADDITIONAL_QUES=character(),DUPLICATE_QUES=character(),stringsAsFactors = FALSE)
#loop around each survey - set unique survey id (surveyID_SurveyTypeID)
for (i in unique(pmdata$uniqueID)) {
  #subset definitions to get just the relevant survey
  test_df <- pmdata[pmdata$uniqueID==i,]
  survey_def <- category_def_dictionary[which(category_def_dictionary[,1]==unique(test_df$SurveyID)),2]
  categ_def <- definitions[which(definitions$Survey==as.character(survey_def)),]
  if(unique(test_df$SurveyID) %in% whitelist[,1]){
    next(i)
  }
  
  
  #Additional Categories
  ADDTL_CAT <- unique(as.character(test_df$RptCategory)[which(!(tolower(test_df$RptCategory) %in% tolower(categ_def$Category.Label)))])
  ADDTL_CAT_TABLE <- data.frame("SURVID_SURVTYPE"=rep(i,length(ADDTL_CAT)),"SURVDEF"=rep(as.character(survey_def),length(ADDTL_CAT)),CATEGORY=rep("",times=length(ADDTL_CAT)),MISSING_CAT=rep("",times=length(ADDTL_CAT)),ADDTL_CATEGORY=ADDTL_CAT,MISSING_QUES=rep("",times=length(ADDTL_CAT)),ADDITIONAL_QUES=rep("",times=length(ADDTL_CAT)),DUPLICATE_QUES=rep("",times=length(ADDTL_CAT)))
  #Find Missing categories
  MISSING_CAT <- unique(as.character(categ_def$Category.Label)[which(!(tolower(categ_def$Category.Label) %in% tolower(test_df$RptCategory)))])
  MISSING_CAT_TABLE <- data.frame("SURVID_SURVTYPE"=rep(i,length(MISSING_CAT)),"SURVDEF"=rep(as.character(survey_def),length(MISSING_CAT)),CATEGORY=rep("",times=length(MISSING_CAT)),MISSING_CAT,ADDTL_CATEGORY=rep("",times=length(MISSING_CAT)),MISSING_QUES=rep("",times=length(MISSING_CAT)),ADDITIONAL_QUES=rep("",times=length(MISSING_CAT)),DUPLICATE_QUES=rep("",times=length(MISSING_CAT)))
  QA_Table <- rbind(QA_Table,MISSING_CAT_TABLE)
  QA_Table <- rbind(QA_Table,ADDTL_CAT_TABLE)
  #Find duplicate QIDs
  dupes_table <- test_df[duplicated(test_df$QAlphaID)|duplicated(test_df$QAlphaID,fromLast = TRUE),]
  dupes_table <- dupes_table[,8:10]
  dupes_table2 <- data.frame("SURVID_SURVTYPE"=rep(i,length(dupes_table[,1])),"SURVDEF"=rep(as.character(survey_def),length(dupes_table[,1])),"CATEGORY"=as.character(dupes_table[,3]),MISSING_CAT=rep("",length(dupes_table[,1])),ADDTL_CATEGORY=rep("",length(dupes_table[,1])),MISSING_QUES=rep("",length(dupes_table[,1])),ADDITIONAL_QUES=rep("",length(dupes_table[,1])),"DUPLICATE_QUES"=as.character(dupes_table[,1]),stringsAsFactors = FALSE)
  QA_Table <- rbind(QA_Table,dupes_table2)
  #loop around each category (in pmdata)
  for (x in unique(test_df$RptCategory)) {
    
    if(!(x %in% ADDTL_CAT)){
      #build vectors (change as.character to tolower if necessary, see above)
      test_vector <- tolower(test_df$QAlphaID[test_df$RptCategory==x])
      reference_vector <- tolower(categ_def$QID[which(tolower(categ_def$Category.Label)==tolower(x))])
      #Questions missing from category
      missing_ques <- reference_vector[which(!(reference_vector %in% test_vector))]
      #Additional Questions in category (add logic to skip if length(reference_vector)=0 - indicates an extra category)
      additional_ques <- test_vector[which(!(test_vector %in% reference_vector))]
      if(length(missing_ques)>0){
        missing_ques_table <- data.frame("SURVID_SURVTYPE"=rep(i,length(missing_ques)),"SURVDEF"=rep(as.character(survey_def),length(missing_ques)),CATEGORY=rep(x,times=length(missing_ques)),MISSING_CAT=rep("",times=length(missing_ques)),ADDTL_CATEGORY=rep("",times=length(missing_ques)),MISSING_QUES=missing_ques,ADDITIONAL_QUES=rep("",times=length(missing_ques)),DUPLICATE_QUES=rep("",times=length(missing_ques)))
        QA_Table <- rbind(QA_Table,missing_ques_table)
      }
      if(length(additional_ques)>0){
        additional_ques_table <- data.frame("SURVID_SURVTYPE"=rep(i,length(additional_ques)),"SURVDEF"=rep(as.character(survey_def),length(additional_ques)),CATEGORY=rep(x,times=length(additional_ques)),MISSING_CAT=rep("",times=length(additional_ques)),ADDTL_CATEGORY=rep("",times=length(additional_ques)),MISSING_QUES=rep("",times=length(additional_ques)),ADDITIONAL_QUES=additional_ques,DUPLICATE_QUES=rep("",times=length(additional_ques)))
        QA_Table <- rbind(QA_Table,additional_ques_table)
      }
    }
  }
  #Category names match (find all definition categories in extract?)
  #Number of categories?
  #
  #Append rows to master table
}




#Write table to csv
write.csv(QA_Table,"QA_Table.csv")