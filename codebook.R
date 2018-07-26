#Creating new codebook that correspond to cleaned variables

#writing function to switch "" to NA

toNA <- function(x){
  ifelse(x == "", NA, x)
}

### --- Teen PA

#Loading in teenPA codebook
codebook_teenPA <- read.csv("FLASHE-Public-Codebook teenPA.csv", stringsAsFactors = F)

#choosing the first three columns
codebook_teenPA <- codebook_teenPA %>%
  #creating new column that combines var label and question if question is blank
  mutate(question_full = ifelse(Question == "", Delivered.Variable.Label, Question)) %>%
  select(-Delivered.Variable.Label, -Question) %>%
  mutate_at(vars(Valid.Values:X.10), funs(toNA))

#Getting rid of blank rows
codebook_teenPA <- codebook_teenPA[c(1, 2, 4, 6:185), ]

### --- teenDiet

#Loading in teenDiet codebook
codebook_teenDiet <- read.csv("FLASHE-Public-Codebook teenDiet.csv", stringsAsFactors = F)

#choosing the first three columns
codebook_teenDiet <- codebook_teenDiet %>%
  #creating new column that combines var label and question if question is blank
  mutate(question_full = ifelse(Question == "", Delivered.Variable.Label, Question)) %>%
  select(-Delivered.Variable.Label, -Question) %>%
  mutate_at(vars(Valid.Values:X.10), funs(toNA))

#Getting rid of blank rows
codebook_teenDiet <- codebook_teenDiet[c(1, 2, 4, 6:147), ]

### --- teenDemo

#Loading in teenDiet codebook
codebook_teenDemo <- read.csv("FLASHE-Public-Codebook teenDemo.csv", stringsAsFactors = F)

#choosing the first three columns
codebook_teenDemo <- codebook_teenDemo %>%
  #creating new column that combines var label and question if question is blank
  mutate(X.8 = NA, X.9 = NA, X.10 = NA) %>%
  mutate(question_full = ifelse(Question == "", Delivered.Variable.Label, Question)) %>%
  select(-Delivered.Variable.Label, -Question) %>%
  mutate_at(vars(Valid.Values:X.10), funs(toNA))

#Getting rid of blank rows
codebook_teenDemo <- codebook_teenDemo[1:28, ]

### --- BINDING EACH ROW TOGETHER
codebook_teen <- bind_rows(codebook_teenDemo, codebook_teenDiet, codebook_teenPA)

#sourcing the df's code
source("dfs.R")

#taking out the distinguish column since it is not in the codebook and will not match up
teen_full2 <- teen_full %>%
  select(-distinguish)

#Creating lists to be able to create a teen codebook
names_teen <- names(teen_full2)

names_teen_code <- codebook_teen$Delivered.Variable.Name

codebook_question <- codebook_teen$question_full

#valid values
codebook_valid1 <- codebook_teen$Valid.Values
codebook_valid2 <- codebook_teen$X
codebook_valid3 <- codebook_teen$X.1
codebook_valid4 <- codebook_teen$X.2
codebook_valid5 <- codebook_teen$X.3
codebook_valid6 <- codebook_teen$X.4
codebook_valid7 <- codebook_teen$X.5
codebook_valid8 <- codebook_teen$X.6
codebook_valid9 <- codebook_teen$X.7
codebook_valid10 <- codebook_teen$X.8
codebook_valid11 <- codebook_teen$X.9
codebook_valid12 <- codebook_teen$X.10

#codebook and cleaned teen vars
codebook_df_teen <- data.frame("codebook_var" = names_teen_code, "cleaned_var" = names_teen, 
                               "codebook_question" = codebook_question, "valid_value1" = codebook_valid1,
                               "valid_value2" = codebook_valid2, "valid_value3" = codebook_valid3,
                               "valid_value4" = codebook_valid4, "valid_value5" = codebook_valid5,
                               "valid_value6" = codebook_valid6, "valid_value7" = codebook_valid7,
                               "valid_value8" = codebook_valid8, "valid_value9" = codebook_valid9,
                               "valid_value10" = codebook_valid10, "valid_value11" = codebook_valid11,
                               "valid_value12" = codebook_valid12)

#changing factors to characters
codebook_df_teen <- codebook_df_teen %>%
  mutate_at(vars(codebook_var:valid_value12), funs(as.character))



### --- NOW FOR PARENTS

### --- Parent PA

#Loading in teenPA codebook
codebook_parentPA <- read.csv("FLASHE-Public-Codebook parentPA.csv", stringsAsFactors = F)

#choosing the first three columns
codebook_parentPA <- codebook_parentPA %>%
  #creating new column that combines var label and question if question is blank
  mutate(question_full = ifelse(Question == "", Delivered.Variable.Label, Question)) %>%
  select(-Delivered.Variable.Label, -Question) %>%
  mutate_at(vars(Valid.Values:X.10), funs(toNA))

#Getting rid of blank rows
codebook_parentPA <- codebook_parentPA[c(1, 2, 4, 5, 7:134), ]

### --- parentDiet

#Loading in teenDiet codebook
codebook_parentDiet <- read.csv("FLASHE-Public-Codebook parentDiet.csv", stringsAsFactors = F)

#choosing the first three columns
codebook_parentDiet <- codebook_parentDiet %>%
  #creating new column that combines var label and question if question is blank
  mutate(question_full = ifelse(Question == "", Delivered.Variable.Label, Question)) %>%
  select(-Delivered.Variable.Label, -Question) %>%
  mutate_at(vars(Valid.Values:X.10), funs(toNA))

#Getting rid of blank rows
codebook_parentDiet <- codebook_parentDiet[c(1, 2, 4, 5, 7:147), ]

### --- parentDemo

#Loading in teenDiet codebook
codebook_parentDemo <- read.csv("FLASHE-Public-Codebook parentDemo.csv", stringsAsFactors = F)

#choosing the first three columns
codebook_parentDemo <- codebook_parentDemo %>%
  select(-c(X.11:X.24)) %>%
  #creating new column that combines var label and question if question is blank
  mutate(question_full = ifelse(Question == "", Delivered.Variable.Label, Question)) %>%
  select(-Delivered.Variable.Label, -Question) %>%
  mutate_at(vars(Valid.Values:X.10), funs(toNA))

### --- BINDING EACH ROW TOGETHER
codebook_parent <- bind_rows(codebook_parentDemo, codebook_parentDiet, codebook_parentPA)

#taking out the distinguish column since it is not in the codebook and will not match up
parent_full2 <- parent_full %>%
  select(-distinguish)

#Creating lists to be able to create a teen codebook
names_parent <- names(parent_full2)

names_parent_code <- codebook_parent$Delivered.Variable.Name

codebook_question_parent <- codebook_parent$question_full

#valid values
codebook_valid1 <- codebook_parent$Valid.Values
codebook_valid2 <- codebook_parent$X
codebook_valid3 <- codebook_parent$X.1
codebook_valid4 <- codebook_parent$X.2
codebook_valid5 <- codebook_parent$X.3
codebook_valid6 <- codebook_parent$X.4
codebook_valid7 <- codebook_parent$X.5
codebook_valid8 <- codebook_parent$X.6
codebook_valid9 <- codebook_parent$X.7
codebook_valid10 <- codebook_parent$X.8
codebook_valid11 <- codebook_parent$X.9
codebook_valid12 <- codebook_parent$X.10

#codebook and cleaned teen vars
codebook_df_parent <- data.frame("codebook_var" = names_parent_code, "cleaned_var" = names_parent, 
                               "codebook_question" = codebook_question_parent, "valid_value1" = codebook_valid1,
                               "valid_value2" = codebook_valid2, "valid_value3" = codebook_valid3,
                               "valid_value4" = codebook_valid4, "valid_value5" = codebook_valid5,
                               "valid_value6" = codebook_valid6, "valid_value7" = codebook_valid7,
                               "valid_value8" = codebook_valid8, "valid_value9" = codebook_valid9,
                               "valid_value10" = codebook_valid10, "valid_value11" = codebook_valid11,
                               "valid_value12" = codebook_valid12)


#changing factors to characters
codebook_df_parent <- codebook_df_parent %>%
  mutate_at(vars(codebook_var:valid_value12), funs(as.character))


### --- CREATING ONE LARGE CODEBOOK (INDIVIDUAL FORMAT)

codebook_df_full <- codebook_df_parent %>%
  full_join(codebook_df_teen, by = "cleaned_var") %>%
  mutate(codebook_var = ifelse(is.na(codebook_var.x), codebook_var.y, codebook_var.x),
         codebook_question = ifelse(is.na(codebook_question.x), codebook_question.y, codebook_question.x),
         valid_value1 = ifelse(is.na(valid_value1.x), valid_value1.y, valid_value1.x),
         valid_value2 = ifelse(is.na(valid_value2.x), valid_value2.y, valid_value2.x),
         valid_value3 = ifelse(is.na(valid_value3.x), valid_value3.y, valid_value3.x),
         valid_value4 = ifelse(is.na(valid_value4.x), valid_value4.y, valid_value4.x),
         valid_value5 = ifelse(is.na(valid_value5.x), valid_value5.y, valid_value5.x),
         valid_value6 = ifelse(is.na(valid_value6.x), valid_value6.y, valid_value6.x),
         valid_value7 = ifelse(is.na(valid_value7.x), valid_value7.y, valid_value7.x),
         valid_value8 = ifelse(is.na(valid_value8.x), valid_value8.y, valid_value8.x),
         valid_value9 = ifelse(is.na(valid_value9.x), valid_value9.y, valid_value9.x),
         valid_value10 = ifelse(is.na(valid_value10.x), valid_value10.y, valid_value10.x),
         valid_value11 = ifelse(is.na(valid_value11.x), valid_value11.y, valid_value11.x),
         valid_value12 = ifelse(is.na(valid_value12.x), valid_value12.y, valid_value12.x)) %>%
  select(cleaned_var, codebook_var, codebook_question, valid_value1, valid_value2, valid_value3,
         valid_value4, valid_value5, valid_value6, valid_value7, valid_value8, valid_value9,
         valid_value10, valid_value11, valid_value12)

#Fixing weird issues that crash the shiny app

codebook_df_full <- codebook_df_full %>%
  mutate(codebook_question = ifelse(cleaned_var == "WEIGHTCHNG", "Are you currently trying to:",
                                    ifelse(cleaned_var == "WORKSTAT", "What is your current employment status? Are you:",
                                           ifelse(cleaned_var == "AGEMENSTR", 
                                                  "How old were you when you had your first menstrual period? Were you", codebook_question))))

#needed to fix if they had <c9> or <ca>
