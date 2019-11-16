library(tidyverse)
library(gridExtra)
library(lubridate)
library(scales)
library(cowplot)

#Read in the data. This will throw an error if you haven't run the clean_and_EDA.R script in this computer before
donations_plus <- readRDS(file = "data/donations_plus.rds")

#Adding the required columns to be able to create useful Retention Rate charts and calculations.

#The first donor list identifies donors whose donors whose donor cart sequence starts at a number higher than one, because that means that those donors have given before the data starts, meaning on or before 2012

donors_before_data <- donations_plus %>%
  group_by(donor_id) %>%
  filter(min(donor_cart_sequence) > 1) %>%
  select(donor_id)
donors_2012 <- donations_plus %>%
  filter(year(donation_received_date) == 2012) %>%
  select(donor_id)
donors_2013 <- donations_plus %>%
  filter(year(donation_received_date) == 2013) %>%
  select(donor_id)
donors_2014 <- donations_plus %>%
  filter(year(donation_received_date) == 2014) %>%
  select(donor_id)
donors_2015 <- donations_plus %>%
  filter(year(donation_received_date) == 2015) %>%
  select(donor_id)
donors_2016 <- donations_plus %>%
  filter(year(donation_received_date) == 2016) %>%
  select(donor_id)
donors_2017 <- donations_plus %>%
  filter(year(donation_received_date) == 2017) %>%
  select(donor_id)
donors_2018 <- donations_plus %>%
  filter(year(donation_received_date) == 2018) %>%
  select(donor_id)


donations_plus <- donations_plus %>%
  mutate(donor_gave_last_year = case_when(
    year(donation_received_date) == 2013 ~ as.character(donor_id) %in% pull(donors_2012),
    year(donation_received_date) == 2014 ~ as.character(donor_id) %in% pull(donors_2013),
    year(donation_received_date) == 2015 ~ as.character(donor_id) %in% pull(donors_2014),
    year(donation_received_date) == 2016 ~ as.character(donor_id) %in% pull(donors_2015),
    year(donation_received_date) == 2017 ~ as.character(donor_id) %in% pull(donors_2016),
    year(donation_received_date) == 2018 ~ as.character(donor_id) %in% pull(donors_2017)))

summary(donations_plus$donor_gave_last_year)

donations_plus <- donations_plus %>%
  mutate(donor_gave_ever_before = case_when(
    year(donation_received_date) == 2012 ~ as.character(donor_id) %in% pull(donors_before_data),
    year(donation_received_date) == 2013 ~ as.character(donor_id) %in% pull(donors_before_data) | as.character(donor_id) %in% pull(donors_2012),
    year(donation_received_date) == 2014 ~ as.character(donor_id) %in% pull(donors_before_data) | as.character(donor_id) %in% pull(donors_2013) | as.character(donor_id) %in% pull(donors_2012),
    year(donation_received_date) == 2015 ~ as.character(donor_id) %in% pull(donors_before_data) | as.character(donor_id) %in% pull(donors_2014) | as.character(donor_id) %in% pull(donors_2013) | as.character(donor_id) %in% pull(donors_2012),
    year(donation_received_date) == 2016 ~ as.character(donor_id) %in% pull(donors_before_data) | as.character(donor_id) %in% pull(donors_2015) | as.character(donor_id) %in% pull(donors_2014) | as.character(donor_id) %in% pull(donors_2013) | as.character(donor_id) %in% pull(donors_2012),
    year(donation_received_date) == 2017 ~ as.character(donor_id) %in% pull(donors_before_data) | as.character(donor_id) %in% pull(donors_2016) | as.character(donor_id) %in% pull(donors_2015) | as.character(donor_id) %in% pull(donors_2014) | as.character(donor_id) %in% pull(donors_2013) | as.character(donor_id) %in% pull(donors_2012),
    year(donation_received_date) == 2018 ~ as.character(donor_id) %in% pull(donors_before_data) | as.character(donor_id) %in% pull(donors_2017) | as.character(donor_id) %in% pull(donors_2016) | as.character(donor_id) %in% pull(donors_2015) | as.character(donor_id) %in% pull(donors_2014) | as.character(donor_id) %in% pull(donors_2013) | as.character(donor_id) %in% pull(donors_2012)))

summary(donations_plus$donor_gave_ever_before)


donations_plus <- donations_plus %>%
  mutate(retention_status = case_when(
    donor_gave_last_year ~ "Retained donor",
    !donor_gave_last_year & donor_gave_ever_before ~ "Reactivated donor",
    !donor_gave_ever_before ~ "New donor"
  ))
table(donations_plus$retention_status)

#Revenue and growth trends by year
revenue_per_year_plot <- donations_plus %>%
  filter(year(donation_received_date) > 2012 & year(donation_received_date) < 2018) %>%
  group_by(year = year(donation_received_date), retention_status) %>%
  summarize(donation_millions = sum(donation_amount)/1000000) %>%
  ggplot(aes(x = year, y = donation_millions, fill = "pink")) +
  geom_col() +
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
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0, .25, .5)) +
  coord_cartesian(ylim = c(0,.5)) +
  geom_text(aes(label = percent(perc_growth)), position = position_dodge(), vjust = -1.5)
perc_growth_per_year_plot
plot_grid(perc_growth_per_year_plot, revenue_per_year_plot, align = "v", nrow = 2, rel_heights = c(1/4, 3/4))

#Revenue and retention trends per year
revenue_per_year_per_first_plot <- donations %>%
  filter(year(donation_received_date) > 2012 & year(donation_received_date) < 2018) %>%
  group_by(year = year(donation_received_date), first_donation) %>%
  summarize(donation_millions = sum(donation_amount)/1000000) %>%
  ggplot(aes(x = year, y = donation_millions, fill = first_donation)) +
  geom_col() +
  labs(y = "Total donation value (in millions)", fill = "From a first-time donor?") +
  theme(legend.position = "bottom",
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = dollar_format())
yearly_retained_table <- donations %>%
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
  coord_cartesian(ylim = c(.5, 1))
plot_grid(perc_retained_per_year_plot, revenue_per_year_per_first_plot, align = "v", nrow = 2, rel_heights = c(1/4, 3/4))

perc_retained_per_year_plot
revenue_per_year_per_first_plot

yearly_growth_table %>% ggplot(aes(x = year, y = total)) +
  geom_col()


#This was all good for a quick(er) calculation, but now we've already set up the dataset to enable us to properly track (yearly) retention rates over time. This one will be yearly


#New retention charts
revenue_per_year_per_retained <- donations_plus %>%
  filter(year(donation_received_date) > 2012 & year(donation_received_date) < 2018) %>%
  group_by(year = year(donation_received_date), retention_status) %>%
  summarize(donation_millions = sum(donation_amount)/1000000) %>%
  ggplot(aes(x = year, y = donation_millions, fill = retention_status)) +
  geom_col() +
  labs(y = "Total donation value (in millions)", fill = "Donor gave in the year prior?") +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  scale_y_continuous(labels = dollar_format())
yearly_proper_retained_table <- donations_plus %>%
  filter(year(donation_received_date) > 2012 & year(donation_received_date) < 2018) %>%
  group_by(year = year(donation_received_date), donor_gave_last_year) %>%
  summarize(total = sum(donation_amount)) %>%
  spread(donor_gave_last_year, total) %>%
  mutate(total = sum(c(`FALSE`,`TRUE`), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(perc_retained = `TRUE`/lag(total))
avg_retention <- percent(mean(yearly_proper_retained_table$perc_retained, na.rm = TRUE))
perc_proper_retained_per_year_plot <- yearly_proper_retained_table %>%
  ggplot(aes(x = year, y = perc_retained)) +
  geom_line() +
  labs(y = "Value Retention") +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(.3, .6)) +
  coord_cartesian(ylim = c(.3,.6)) +
  geom_text(aes(label = percent(perc_retained)), position = position_dodge(), vjust = -1.5)
plot_grid(perc_proper_retained_per_year_plot, revenue_per_year_per_retained, align = "v", nrow = 2, rel_heights = c(1/4, 3/4))
perc_proper_retained_per_year_plot
#let's do a retention table manually and see how it compares (for double checking and trying other approaches)
manual_retention_table <- donations_plus %>%
  group_by(year(donation_received_date)) %>%
  summarize(total_donations = sum(donation_amount),
            total_donors = n_distinct(donor_id))

retained_2014 <- donations_plus %>%
  filter(as.character(donor_id) %in% pull(donors_2013), year(donation_received_date) == 2014) %>%
  summarise(sum_donation_value = sum(donation_amount),
            nr_donors = n_distinct(donor_id))

retained_2015 <- donations_plus %>%
  filter(as.character(donor_id) %in% pull(donors_2014), year(donation_received_date) == 2015) %>%
  summarise(sum_donation_value = sum(donation_amount),
            nr_donors = n_distinct(donor_id))

retained_2016 <- donations_plus %>%
  filter(as.character(donor_id) %in% pull(donors_2015), year(donation_received_date) == 2016) %>%
  summarise(sum_donation_value = sum(donation_amount),
            nr_donors = n_distinct(donor_id))

retained_2017 <- donations_plus %>%
  filter(as.character(donor_id) %in% pull(donors_2016), year(donation_received_date) == 2017) %>%
  summarise(sum_donation_value = sum(donation_amount),
            nr_donors = n_distinct(donor_id))

manual_retention_table$donations_retained <- as.numeric(c("","", retained_2014$sum_donation_value,retained_2015$sum_donation_value,retained_2016$sum_donation_value, retained_2017$sum_donation_value, ""))

manual_retention_table$donors_retained <- as.numeric(c("","", retained_2014$nr_donors,retained_2015$nr_donors,retained_2016$nr_donors, retained_2017$nr_donors, ""))

manual_retention_table <- manual_retention_table %>%
  mutate(value_retention = donations_retained/lag(total_donations),
         donor_retention = donors_retained/lag(total_donors))

#Let's repeat the retention numbers but with donor-retention
revenue_per_year_per_retained_donors <- donations_plus %>%
  filter(year(donation_received_date) > 2012 & year(donation_received_date) < 2018) %>%
  group_by(year = year(donation_received_date), retention_status) %>%
  summarize(donors = n_distinct(donor_id)) %>%
  ggplot(aes(x = year, y = donors, fill = retention_status)) +
  geom_col() +
  labs(y = "Number of donors", fill = "Donor gave in the year prior?") +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  scale_y_continuous(label = scales::comma)
yearly_proper_retained_donors_table <- donations_plus %>%
  filter(year(donation_received_date) > 2012 & year(donation_received_date) < 2018) %>%
  group_by(year = year(donation_received_date), donor_gave_last_year) %>%
  summarize(total = n_distinct(donor_id)) %>%
  spread(donor_gave_last_year, total) %>%
  mutate(total = sum(c(`FALSE`,`TRUE`), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(perc_retained = `TRUE`/lag(total))
avg_donor_retention <- percent(mean(yearly_proper_retained_donors_table$perc_retained, na.rm = TRUE))
perc_proper_retained_donors_per_year_plot <- yearly_proper_retained_donors_table %>%
  ggplot(aes(x = year, y = perc_retained)) +
  geom_line() +
  labs(y = "Donor Retention") +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(.1,.3)) +
  geom_text(aes(label = percent(perc_retained)), position = position_dodge(), vjust = -1.5)
plot_grid(perc_proper_retained_donors_per_year_plot, revenue_per_year_per_retained_donors, align = "v", nrow = 2, rel_heights = c(1/4, 3/4))


#And now for final sanity check: get a random sample of 5 donations so I can manually check that it's imputed correctly
sample <- sample_n(donations_plus, 5) %>%
  select(donor_id, donation_received_date, donor_gave_last_year)
sample
sample2 <- filter(donations_plus, donor_id %in% sample$donor_id) %>%
  select("donor_id", "donation_received_date", "donor_gave_last_year") %>%
  arrange(donor_id)

#Also, Why are the retentained+reactivated sums soooo different from the first or repeat donation results? One explanation could be that there's a lot of 'hidden reactivation' because we don't have data from earlier years, so let's see what the minimum donation cart sequences look like.
another_sanity_check <- donations_plus %>%
  group_by(donor_id) %>%
  summarise(minumum_don_cart_seq = min(donor_cart_sequence)) %>%
  group_by(minumum_don_cart_seq) %>%
  summarise(count = n())
head(another_sanity_check, 100)
another_sanity_check[1,2]/sum(another_sanity_check$count)
table(another_sanity_check)

?table()

#Top states by donation amount
top_10_school_states <- donations_plus %>%
  group_by(school_state) %>%
  summarize(donation_value = sum(donation_amount),
            perc_of_total = sum(donation_amount)/sum(donations_plus$donation_amount)) %>%
  arrange(desc(perc_of_total)) %>%
  mutate(cumm_perc = cumsum(perc_of_total)) %>%
  top_n(10, perc_of_total)

#Let's figure out timing
#Donations by month       
by_month <- donations_plus %>%
  filter(year(donation_received_date) > 2012 & year(donation_received_date) < 2018) %>%
  group_by(month = month(donation_received_date)) %>%
  summarize(donation_value = sum(donation_amount),
            perc_of_total = sum(donation_amount)/sum(donations_plus$donation_amount)) %>%
  arrange(desc(perc_of_total)) %>%
  mutate(cumm_perc = cumsum(perc_of_total))
donations_plus %>%
  filter(year(donation_received_date) > 2012 & year(donation_received_date) < 2018) %>%
  group_by(month = month(donation_received_date), factor_year = as.factor(year(donation_received_date))) %>%
  summarise(donation_value = sum(donation_amount)) %>%
  ggplot(aes(x = month, y = donation_value, fill = factor_year)) +
  geom_col()

#Donations by week of the year
donations_plus %>%
  filter(year(donation_received_date) > 2012 & year(donation_received_date) < 2018) %>%
  group_by(week = week(donation_received_date), factor_year = as.factor(year(donation_received_date))) %>%
  summarise(donation_value = sum(donation_amount)) %>%
  ggplot(aes(x = week, y = donation_value, fill = factor_year)) +
  geom_col()

#Donations by day of the year
#These are a whole bunch of plots and tables that I am trying, to see which ones prove more interesting
donations_plus %>%
  filter(year(donation_received_date) > 2012 & year(donation_received_date) < 2018) %>%
  group_by(day_of_year = yday(donation_received_date), factor_year = as.factor(year(donation_received_date))) %>%
  summarise(donation_value = sum(donation_amount)) %>%
  ggplot(aes(x = day_of_year, y = donation_value, fill = factor_year)) +
  geom_col()

donations_plus %>%
  group_by(month = month(donation_received_date), day = day(donation_received_date)) %>%
  summarize(donations = sum(donation_amount)) %>%
  ggplot(aes(x = day, y = as.factor(month))) +
  geom_tile(aes(fill = donations))


donations_plus %>%
  group_by(month = month(donation_received_date), day = day(donation_received_date)) %>%
  summarize(donations = sum(donation_amount)) %>%
  arrange(desc(donations))

donations_plus %>%
  group_by(month = month(donation_received_date), day = day(donation_received_date)) %>%
  summarize(donations = sum(donation_amount)) %>%
  arrange(desc(donations))

donations_plus %>%
  filter(year(donation_received_date) > 2012, year(donation_received_date) < 2018) %>%
  group_by(year = as.factor(year(donation_received_date)), month = month(donation_received_date), day = day(donation_received_date)) %>%
  summarize(donations = sum(donation_amount)) %>%
  ggplot(aes(x = day, y = donations, color = year)) +
  geom_line() +
  facet_wrap(~ month, nrow = 4, ncol = 3)
donations_plus %>%
  filter(year(donation_received_date) > 2012, year(donation_received_date) < 2018) %>%
  group_by(month = month(donation_received_date), day = day(donation_received_date)) %>%
  summarize(donations = sum(donation_amount)) %>%
  ggplot(aes(x = day, y = donations)) +
  geom_line() +
  facet_wrap(~ month, nrow = 4, ncol = 3)

top_n(donations_plus, 25, donation_amount) %>%
  arrange(desc(donation_amount))

top_25_donation_days <- donations_plus %>%
  group_by(date = date(donation_received_date)) %>%
  summarize(donations = sum(donation_amount)) %>%
  top_n(25, donations) %>%
  arrange(desc(donations))

donations_plus %>%
  filter(year(donation_received_date) > 2012, year(donation_received_date) < 2018, donation_received_date != as_date(2017-29-03)) %>%
  group_by(year = as.factor(year(donation_received_date)), month = month(donation_received_date)) %>%
  filter() %>%
  summarize(donations = sum(donation_amount)) %>%
  arrange(desc(donations))
donations_faceted_month_year <- donations_plus %>%
  filter(year(donation_received_date) > 2012, year(donation_received_date) < 2018, !(day(donation_received_date) == 29 & month(donation_received_date) == 3 & year(donation_received_date) == 2017)) %>%
  group_by(year = as.factor(year(donation_received_date)), month = month(donation_received_date), day = day(donation_received_date)) %>%
  summarize(donations = sum(donation_amount)) %>%
  ggplot(aes(x = day, y = donations, color = year)) +
  geom_line() +
  facet_wrap(~ month, nrow = 4, ncol = 3)

donations_faceted_month_year_with_outlier <- donations_plus %>%
  filter(year(donation_received_date) > 2012, year(donation_received_date) < 2018) %>%
  group_by(year = as.factor(year(donation_received_date)), month = factor(month(donation_received_date), labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")), day = day(donation_received_date)) %>%
  summarize(donations = sum(donation_amount)/1000) %>%
  ggplot(aes(x = day, y = donations, color = year)) +
  geom_line() +
  facet_wrap(~ month, nrow = 4, ncol = 3) +
  ylab("Donation value (thousands)") +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 9)) +
  scale_y_continuous(labels = dollar_format())


donations_plus %>%
  filter(year(donation_received_date) > 2012, year(donation_received_date) < 2018, retention_status != "New donor") %>%
  group_by(year = as.factor(year(donation_received_date)), month = month(donation_received_date), day = day(donation_received_date)) %>%
  summarize(donations = sum(donation_amount)) %>%
  ggplot(aes(x = day, y = donations, color = year)) +
  geom_line() +
  facet_wrap(~ month, nrow = 4, ncol = 3) 

donations_faceted_month_year_with_outlier +
  coord_cartesian(ylim = c(0, 1300000))

donations_by_day_facet_year <- donations_plus %>%
  filter(year(donation_received_date) > 2012, year(donation_received_date) < 2018) %>%
  group_by(year = as.factor(year(donation_received_date)), day = yday(donation_received_date)) %>%
  summarize(donations = sum(donation_amount)) %>%
  ggplot(aes(x = day, y = donations, color = year)) +
  geom_line() +
  facet_wrap(~ year, nrow = 5, ncol = 1)

donations_plus %>%
  filter(year(donation_received_date) > 2012, year(donation_received_date) < 2018, !(day(donation_received_date) == 29 & month(donation_received_date) == 3 & year(donation_received_date) == 2017)) %>%
  group_by(year = as.factor(year(donation_received_date)), week = week(donation_received_date)) %>%
  summarize(donations = sum(donation_amount)) %>%
  ggplot(aes(x = week, y = donations, color = year)) +
  geom_line() +
  facet_wrap(~ year, nrow = 5, ncol = 1)

donations_plus %>%
  filter(year(donation_received_date) > 2012, year(donation_received_date) < 2018) %>%
  group_by(year = as.factor(year(donation_received_date)), month = month(donation_received_date)) %>%
  summarize(donations = sum(donation_amount)) %>%
  ggplot(aes(x = month, y = donations, fill = year)) +
  geom_col() +
  facet_wrap(~ year, nrow = 5, ncol = 1)

donations_by_month <- donations_plus %>%
  filter(year(donation_received_date) > 2012, year(donation_received_date) < 2018) %>%
  group_by(year = fct_rev(as.factor(year(donation_received_date))), month = factor(month(donation_received_date), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>%
  summarize(donations = sum(donation_amount)/1000000) %>%
  ggplot(aes(x = month, y = donations, fill = year)) +
  geom_col() +
  ylab("Donation value (millions)") +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = dollar_format())

donations_plus %>%
  filter(year(donation_received_date) > 2012, year(donation_received_date) < 2018) %>%
  group_by(year = as.factor(year(donation_received_date)), day = as_date(donation_received_date)) %>%
  summarize(donations = sum(donation_amount)) %>%
  ggplot(aes(x = day, y = donations, color = year)) +
  geom_line()

donations_plus %>%
  filter(year(donation_received_date) > 2012, year(donation_received_date) < 2018, !(day(donation_received_date) == 29 & month(donation_received_date) == 3 & year(donation_received_date) == 2017)) %>%
  group_by(year = as.factor(year(donation_received_date)), day = as_date(donation_received_date)) %>%
  summarize(donations = sum(donation_amount)) %>%
  ggplot(aes(x = day, y = donations, color = year)) +
  geom_line()

donations_plus %>%
  filter(year(donation_received_date) > 2012, year(donation_received_date) < 2018) %>%
  group_by(year = as.factor(year(donation_received_date)), month = month(donation_received_date)) %>%
  summarize(donations = sum(donation_amount)) %>%
  ggplot(aes(x = month, y = donations, color = year)) +
  geom_line()

donations_plus %>%
  filter(year(donation_received_date) > 2012, year(donation_received_date) < 2018) %>%
  group_by(year = as.factor(year(donation_received_date)), week = week(donation_received_date)) %>%
  summarize(donations = sum(donation_amount)) %>%
  ggplot(aes(x = week, y = donations, color = year)) +
  geom_line()

donations_plus %>%
  filter(year(donation_received_date) > 2012, year(donation_received_date) < 2018) %>%
  group_by(date = date(donation_received_date)) %>%
  summarize(donations = sum(donation_amount)) %>%
  ggTimeSeries::ggplot_calendar_heatmap("date", "donations") +
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_continuous(low = 'yellow', high = 'red') +
  facet_wrap(~Year, ncol = 1)

donations_plus %>% 
  filter(year(donation_received_date) > 2015, year(donation_received_date) < 2018) %>%
  group_by(month(donation_received_date)) %>%
  summarize(donations = sum(donation_amount)) %>%
  arrange(desc(donations))

donations_plus %>% 
  filter(year(donation_received_date) > 2015, year(donation_received_date) < 2018) %>%
  group_by(date = date(donation_received_date)) %>%
  summarize(donations = sum(donation_amount)) %>%
  ggplot(aes(x = as.factor(month(date)), y = donations)) +
  geom_boxplot()

donations_plus %>% 
  filter(year(donation_received_date) > 2015, year(donation_received_date) < 2018) %>%
  ggplot(aes(x = as.factor(month(donation_received_date)), y = donation_amount)) +
  geom_boxplot()

donations_plus %>% 
  filter(year(donation_received_date) > 2015, year(donation_received_date) < 2018) %>%
  group_by(date = date(donation_received_date), month = as.factor(month(donation_received_date))) %>%
  summarize(donations = sum(donation_amount)) %>%
  ggplot(aes(x = month, y = donations)) +
  geom_boxplot()


ggplot(donations_plus, aes(x = donation_amount)) +
  geom_histogram(binwidth = 5) +
  geom_rug() +
  coord_cartesian(xlim = c(0,5000)) 

quantile(donations_plus$donation_amount, c(.1, .2, .3, .4, .5, .6, .7, .8, .9, .99, .999))

ggplot(donations_plus, aes(x = first_donation, y = donation_amount)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 100))

donations_plus %>%
  filter(year(donation_received_date) > 2016 & year(donation_received_date) < 2018) %>%
  group_by(month(donation_received_date)) %>%
  summarize(donation_value = sum(donation_amount),
            perc_of_total = sum(donation_amount)/sum(donations_plus$donation_amount)) %>%
  arrange(desc(perc_of_total)) %>%
  mutate(cumm_perc = cumsum(perc_of_total))
donations_plus %>%
  filter(year(donation_received_date) > 2012 & year(donation_received_date) < 2018) %>%
  group_by(month = month(donation_received_date), factor_year = as.factor(year(donation_received_date))) %>% 
  summarise(donation_value = sum(donation_amount)) %>%
  ggplot(aes(x = month, y = donation_value, fill = factor_year)) +
  geom_col()

