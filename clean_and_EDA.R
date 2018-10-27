library(tidyverse)
#First, we load the data 
## In order to work with all the dataframes in the directory, we'll create a vector with all their paths and then use lapply and read_csv to load all of them at once
df_paths <- list.files("data", full.names = TRUE)
list_of_dfs <- lapply(df_paths, read_csv)

#We name the dataframes in the resulting list with the file names they came from (minus the extension)
df_names <- tolower(basename(substr(df_paths, 1, nchar(df_paths)-4)))
names(list_of_dfs) <- df_names

#Let's see what we have
str(list_of_dfs)

#Rename columns for lower case and avoiding spaces, to make it easier to call them
for (i in 1:length(list_of_dfs)) {
  list_of_dfs[[i]] <- rename_all(list_of_dfs[[i]], funs(gsub(" ", "_", .)))
  list_of_dfs[[i]] <- rename_all(list_of_dfs[[i]], funs(tolower(.)))
} 
 
str(list_of_dfs)

#Bring 6 separate dataframes to my environment, to make it easier to call them
list2env(list_of_dfs, .GlobalEnv)

#Look at each dataframe to see what additional cleanup is needed

#donations
#turn the donation_included_optional_donation field from character Yes/No to logicals (first I check that there are no Maybes or other values)
table(donations$donation_included_optional_donation)
donations$donation_included_optional_donation <- donations$donation_included_optional_donation == "Yes"

#donors
#turn donor_is_teacher from character to logical (checking first that all values are yes or no)
table(donors$donor_is_teacher)
donors$donor_is_teacher <- donors$donor_is_teacher == "Yes"

#projects
glimpse(projects)
#project_type seems to be a factor. Check and if appropriate change to factor
table(projects$project_type)
projects$project_type <- as.factor(projects$project_type)
#same with a few other fields
table(projects$project_subject_category_tree)
table(projects$project_subject_subcategory_tree)
#these two are interesting and it seems like I might want to play with the strings/do further wrangling at some point, so I'll leave as character for now
table(projects$project_grade_level_category)
projects$project_grade_level_category <- as.factor(projects$project_grade_level_category)
table(projects$project_resource_category)
projects$project_resource_category <- as.factor(projects$project_resource_category)
table(projects$project_current_status)
projects$project_current_status <- as.factor(projects$project_current_status)

#resources
glimpse(resources)
#examined resource_vendor_name, decided to keep it as character given the amount of different vendors. might change my mind later
table(resources$resource_vendor_name)

#schools
glimpse(schools)
table(schools$school_metro_type)
schools$school_metro_type <- as.factor(schools$school_metro_type)

#teachers
glimpse(teachers)
table(teachers$teacher_prefix)
teachers$teacher_prefix <- as.factor(teachers$teacher_prefix)

## Now we check for duplicates, using donation_id because they should be unique for each donation
donations_duplicates <- donations %>%
  filter(duplicated(donation_id) | duplicated(donation_id, fromLast = TRUE)) %>%
  arrange(donation_id)
#Examine to see if the whole observation is duplicated (which would make it safe to delete)
View(donations_duplicates)
#It seems like the only variable that is different is donation_received (the date and time). Because this is the only difference, because the dates are close to each other and because the duplicates are so few, I will assume that they are safe to remove.
donations <- donations %>% filter(!duplicated(donation_id))

#This seems like the right time to split between training, testing and validation
set.seed(42)
train_donations <- sample_frac(donations, 0.9)
train_row_nrs <- as.numeric(rownames(train_donations))
nontrain_donations <- donations[-train_row_nrs,]
validate_donations <- sample_frac(nontrain_donations, 0.5)
validate_row_nrs <- as.numeric(rownames(validate_donations))
test_donations <- nontrain_donations[-validate_row_nrs,]


##NOW WE CAN START EDA
summary(donations)

#First let's look at each variable (mostly) on their on
#project_id
head(sort(table(train_donations$project_id), decreasing = T))
by_project <- train_donations %>% group_by(project_id) %>%
  summarize(count_donations = n(),
            sum_donations = sum(donation_amount),
            avg_amount = mean(donation_amount)) %>%
  arrange(desc(count_donations)) 
View(head(by_project, n = 25))
head(arrange(by_project, desc(sum_donations)))
ggplot(by_project, aes(x = count_donations)) +
  geom_bar()+
  geom_rug()
#filtering to <100 to zoom in
by_project %>% filter(count_donations < 75) %>%
  ggplot(aes(x = count_donations)) +
  geom_bar() +
  geom_rug()

#donor_id
head(sort(table(train_donations$donor_id), decreasing = T))
range(train_donations$donation_received_date)
#Why are there donors with thousands of donations? (Like 1000+donations per year) We'll need to look into these further

#----
#This below doesn't really work (yet?)
#ggplot(donations, aes(x= project_id)) +
#  geom_bar()
# plot(donations)
