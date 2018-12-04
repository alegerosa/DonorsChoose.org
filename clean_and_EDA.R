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

##NOW WE CAN START EDA
summary(donations)

#First let's look at each variable on their own (mostly) 
#project_id
head(sort(table(donations$project_id), decreasing = T))
by_project <- donations %>% group_by(project_id) %>%
  summarize(count_donations = n(),
            sum_donations = sum(donation_amount),
            avg_amount = mean(donation_amount)) %>%
  arrange(desc(count_donations)) 
View(head(by_project, n = 25))
head(arrange(by_project, desc(sum_donations)))
ggplot(by_project, aes(x = count_donations)) +
  geom_bar()+
  geom_rug()
ggplot(by_project, aes(x = count_donations)) +
  geom_histogram()+
  geom_rug()
ggplot(by_project, aes(x = count_donations)) +
  geom_density()+
  geom_rug()
ggplot(by_project, aes(x = 1, y = count_donations)) +
  geom_boxplot()
mean(by_project$count_donations)
median(by_project$count_donations)

#filtering to <100 to zoom in
by_project %>% filter(count_donations < 75) %>%
  ggplot(aes(x = count_donations)) +
  geom_bar() +
  geom_rug()
by_project %>% filter(count_donations < 75) %>%
  ggplot(aes(x = count_donations)) +
  geom_histogram() +
  geom_rug()
by_project %>% filter(count_donations < 75) %>%
  ggplot(aes(x = count_donations)) +
  geom_density() +
  geom_rug()

#the easier way to get this info

nrow(filter(by_project, count_donations < 10))
table(by_project$count_donations)
summary(by_project$count_donations)

#Do projects under 10 donations, indicate projects that never really took off?
by_project %>% filter(count_donations <= 10) %>%
  ggplot(aes(x = count_donations, y = sum_donations)) +
  geom_point(aes(alpha = 0.6))
#Not really.

summary(by_project$sum_donations)
ggplot(by_project, aes(x = sum_donations)) +
  geom_density()+
  geom_rug()
#zoomed in
ggplot(by_project, aes(x = sum_donations)) +
  geom_density()+
  geom_rug() +
  coord_cartesian(xlim = c(0, 10000))

#Wow, 75% of projects got $390 or less. Now it would be interesting to see how this compares to projects' goals (Does this reflect that most projects are relatively small, or does it show that only a small minority of projects get funded?)
summary(projects$project_cost)
ggplot(projects, aes(x = project_cost)) +
  geom_density()+
  geom_rug()
#zoomed in
ggplot(projects, aes(x = project_cost)) +
  geom_density()+
  geom_rug() +
  coord_cartesian(xlim = c(0, 10000))

top_100_proj <- by_project %>%
  arrange(desc(sum_donations)) %>%
  mutate(perc_of_total = sum_donations/sum(sum_donations),
         cumm_perc = cumsum(perc_of_total)) %>%
  top_n(100, sum_donations)
#So, it would seem like only a minority of the projects get fully funded. It will be interesting to continue to explore this. But now, moving on.


pareto_table_by_project <- by_project %>%
  arrange(desc(sum_donations)) %>%
  mutate(perc_of_total = sum_donations/sum(sum_donations),
         cumm_perc = cumsum(perc_of_total),
         rank = percent_rank(cumm_perc))
pareto_chart_by_project <- ggplot(pareto_table_by_project, aes(x = rank, y = cumm_perc)) +
  geom_line() +
  labs(title = "Pareto Chart By Project", x = "% Rank of project", y = "% of Cummulative donations")
pareto_chart_by_project


?rank
#donor_id
head(sort(table(donations$donor_id), decreasing = T), n = 25)
range(donations$donation_received_date)
#Why are there donors with thousands of donations? (Like 1000+donations per year) We'll need to look into these further
summary(donations$donor_id)
by_donor_id <- donations %>% group_by(donor_id) %>%
  summarize(count_donations = n(),
            sum_donations = sum(donation_amount),
            avg_amount = mean(donation_amount)) %>%
  arrange(desc(count_donations)) 
View(by_donor_id)
summary(by_donor_id$count_donations)
avg_over_tot_by_don_id <- by_donor_id %>%
  ggplot(aes(count_donations, avg_amount)) +
  geom_point()
#what if I try with less outliers
avg_over_tot_by_don_id_no_out <- by_donor_id %>%
  filter(count_donations < 3000 & avg_amount < 12000) %>%
  ggplot(aes(count_donations, avg_amount, alpha = 0.6)) +
  geom_point()
avg_over_tot_by_don_id_no_out

#----
#This will be here for whenever is the right time to split between training, testing and validation
#set.seed(42)
#train_donations <- sample_frac(donations, 0.9)
#train_row_nrs <- as.numeric(rownames(train_donations))
#nontrain_donations <- donations[-train_row_nrs,]
#validate_donations <- sample_frac(nontrain_donations, 0.5)
#validate_row_nrs <- as.numeric(rownames(validate_donations))
#test_donations <- nontrain_donations[-validate_row_nrs,]



#This below doesn't really work (yet?)
#ggplot(donations, aes(x= project_id)) +
#  geom_bar()
# plot(donations)
