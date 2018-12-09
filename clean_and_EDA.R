library(tidyverse)
library(gridExtra)
library(lubridate)
library(scales)
library(cowplot)

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


#donor_id
head(sort(table(donations$donor_id), decreasing = T), n = 25)
range(donations$donation_received_date)
#Why are there donors with thousands of donations? (Like 1000+donations per year) We'll need to look into these further. For now, I am assuming that these donor_ids aggregate group donations or automated donations (say, by a company that rounds up transactions or employeed contributions, etc.) and that for summary purposes they are just as valid as any other.
summary(donations$donor_id)
by_donor_id <- donations %>% group_by(donor_id) %>%
  summarize(count_donations = n(),
            sum_donations = sum(donation_amount),
            avg_amount = mean(donation_amount)) %>%
  arrange(desc(count_donations)) 
View(by_donor_id)

avg_over_tot_by_don_id <- by_donor_id %>%
  ggplot(aes(count_donations, avg_amount)) +
  geom_point()
avg_over_tot_by_don_id
#what if I try with less outliers (a way of zooming in)
avg_over_tot_by_don_id_no_out <- by_donor_id %>%
  filter(count_donations < 1000 & avg_amount < 1000) %>%
  ggplot(aes(count_donations, avg_amount, alpha = 0.6)) +
  geom_point()
avg_over_tot_by_don_id_no_out

#Let's see what the distribution of # of donations per donor looks like
summary(by_donor_id$count_donations)
#Median is 1, meaning more than half the donors have given only once.
table(by_donor_id$count_donations)[1]/nrow(by_donor_id)
#So 73% of donors are first timers  
ggplot(by_donor_id, aes(x = count_donations)) +
  geom_density(aes(y = ..scaled..))+
  geom_rug()
#Let's see the same distribution, but only taking into account those who donated more than once. I'll also zoom in to see only up to 5000 donations per donor 
by_donor_id %>% filter(count_donations > 1) %>%
  ggplot(aes(x = count_donations)) +
  geom_density(aes(y = ..scaled..)) +
  geom_rug() +
  coord_cartesian(xlim = c(0, 5000))
#There's more to explore about how first time and 2nd time donors and about retention. We'll stay here for now and see the pareto chart

summary(by_donor_id, sum_donations)
ggplot(by_donor_id, aes(x = count_donations, y = sum_donations, alpha = 0.6)) +
  geom_point()
ggplot(by_donor_id, aes(x = sum_donations)) +
  geom_density(aes(y = ..scaled..)) +
  geom_rug() +
  coord_cartesian(xlim = c(0,25000))
table(by_donor_id$sum_donations)
quantile(by_donor_id$sum_donations, c(.1, .2, .3, .4, .5, .6, .7, .8, .9, .95, .99, .9999))
quantile(by_donor_id$count_donations, c(.1, .2, .3, .4, .5, .6, .7, .8, .9, .95, .99, .9999))
pareto_table_by_donor <- by_donor_id %>%
  arrange(desc(sum_donations)) %>%
  mutate(perc_of_total = sum_donations/sum(sum_donations),
         cumm_perc = cumsum(perc_of_total),
         rank = percent_rank(cumm_perc))
pareto_chart_by_donor <- ggplot(pareto_table_by_donor, aes(x = rank, y = cumm_perc)) +
  geom_line() +
  labs(title = "Pareto Chart By Donor", x = "% Rank of donor", y = "% of Cummulative donations")
pareto_chart_by_donor
grid.arrange(pareto_chart_by_donor, pareto_chart_by_project, ncol = 2)

#for my next chart I'll add a column to donations (and I'll try this two different ways bc I'm testing if donor_cart_sequence means what I think it means)

ids_of_onetime_donors <- select(filter(by_donor_id, count_donations == 1), donor_id)
ids_of_repeat_donors <- select(filter(by_donor_id, count_donations > 1), donor_id)

donations <- donations %>%
  mutate(
    first_donation = case_when(
      donor_cart_sequence == 1 ~ "first",
      donor_cart_sequence > 1 ~ "recurrent"),
    one_and_done = case_when(
      as.character(donor_id) %in% pull(ids_of_onetime_donors) ~ "One-and-done donor",
      as.character(donor_id) %in% pull(ids_of_repeat_donors) ~ "Recurrent donor")
    )

chart_first_donation <- donations %>% filter(year(donation_received_date) > 2012 & year(donation_received_date) < 2018) %>%
  ggplot(aes(x = year(donation_received_date), y = donation_amount, fill = first_donation)) +
  geom_bar(stat = "sum")
chart_first_donation

chart_one_done <- donations %>% filter(year(donation_received_date) > 2012 & year(donation_received_date) < 2018) %>%
  ggplot(aes(x = year(donation_received_date), y = donation_amount, fill = one_and_done)) +
  geom_bar(stat = "sum")
chart_one_done

#NOTE: would be interesting to do the same but by number of donors.

quantile(donations$donor_cart_sequence, c(.1, .2, .3, .4, .5, .6, .7, .8, .9, .99, .999))
year(donations$donation_received_date)


##THIS IS A LOT OF TEMP STEPS I USED TO GET THE "ONE-AND-DONE" METRIC, AND IT STILL HAS ISSUES, SO LEAVING HERE FOR NOW
onetime_vector <- donationsdonoridchar %in% pull(ids_of_onetime_donors)
summary(onetime_vector)
View(donations$donor_id)
View(ids_of_onetime_donors)
donationsdonoridchar <- as.character(donations$donor_id)
as.character(donations$donor_id[1679253]) == ids_of_onetime_donors[1,]
class(ids_of_onetime_donors)
pull(ids_of_onetime_donors)

#Analysis for my summary of key fundraising insights
donations_plus <- donations %>%
  left_join(projects, by = "project_id") %>%
  left_join(donors, by = "donor_id") %>%
  left_join(schools, by = "school_id") %>%
  left_join(teachers, by = "teacher_id")

#Revenue and growth trends by year
revenue_per_year_plot <- donations %>%
  filter(year(donation_received_date) > 2012 & year(donation_received_date) < 2018) %>%
  ggplot(aes(x = year(donation_received_date), y = donation_amount/1000000, fill = "pink")) +
  geom_bar(stat = "sum") +
  labs(y = "Total donation value (in millions)") +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = dollar_format())
yearly_growth_table <- donations_plus %>%
  filter(year(donation_received_date) > 2012 & year(donation_received_date) < 2018) %>%
  group_by(year = year(donation_received_date)) %>%
  summarize(total = sum(donation_amount)) %>%
  mutate(perc_growth = (total/lag(total))-1)
cagr <- percent(mean(yearly_growth_table$perc_growth, na.rm = TRUE))
perc_growth_per_year_plot <- yearly_growth_table %>%
  ggplot(aes(x = year, y = perc_growth)) +
  geom_line() +
  labs(y = "% growth") +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0,1))

plot_grid(perc_growth_per_year_plot, revenue_per_year_plot, align = "v", nrow = 2, rel_heights = c(1/4, 3/4))

#Revenue and retention trends per year
revenue_per_year_per_first_plot <- donations %>%
  filter(year(donation_received_date) > 2012 & year(donation_received_date) < 2018) %>%
  ggplot(aes(x = year(donation_received_date), y = donation_amount/1000000, fill = first_donation)) +
  geom_bar(stat = "sum") +
  labs(y = "Total donation value (in millions)") +
  theme(legend.position = "bottom",
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = dollar_format())
yearly_retained_table <- donations_plus %>%
  filter(year(donation_received_date) > 2012 & year(donation_received_date) < 2018) %>%
  group_by(year = year(donation_received_date), first_donation) %>%
  summarize(total = sum(donation_amount)) %>%
  spread(first_donation, total) %>%
  mutate(total = first + recurrent) %>%
  ungroup() %>%
  mutate(perc_retained = recurrent/lag(total))
avg_retention <- percent(mean(yearly_retained_table$perc_retained, na.rm = TRUE))
perc_retained_per_year_plot <- yearly_retained_table %>%
  ggplot(aes(x = year, y = perc_retained)) +
  geom_line() +
  labs(y = "% retained") +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0, 1))
plot_grid(perc_retained_per_year_plot, revenue_per_year_per_first_plot, align = "v", nrow = 2, rel_heights = c(1/4, 3/4))

perc_retained_per_year_plot
revenue_per_year_per_first_plot


 #Top states by donation amount
top_10_school_states <- donations_plus %>%
  group_by(school_state) %>%
  summarize(donation_value = sum(donation_amount),
            perc_of_total = sum(donation_amount)/sum(donations_plus$donation_amount)) %>%
  arrange(desc(perc_of_total)) %>%
  mutate(cumm_perc = cumsum(perc_of_total)) %>%
  top_n(10, perc_of_total)

#Donations by month       
by_month <- donations_plus %>%
  group_by(month(donation_received_date)) %>%
  summarize(donation_value = sum(donation_amount),
            perc_of_total = sum(donation_amount)/sum(donations_plus$donation_amount)) %>%
  arrange(desc(perc_of_total)) %>%
  mutate(cumm_perc = cumsum(perc_of_total))


ggplot(donations, aes(x = donation_amount)) +
  geom_histogram(binwidth = 5) +
  geom_rug() +
  coord_cartesian(xlim = c(0,5000))

quantile(donations_plus$donation_amount, c(.1, .2, .3, .4, .5, .6, .7, .8, .9, .99, .999))

ggplot(donations_plus, aes(x = first_donation, y = donation_amount)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 100))




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
