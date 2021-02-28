library(tidyverse)
incarceration_df <- read.csv("./data/incarceration_trends.csv")
# Part 1: Data Description
nobs <- nrow(incarceration_df)
smallest_year  <- min(incarceration_df$year)
largest_year  <- max(incarceration_df$year)
smallest_year
largest_year
df_lastest <- incarceration_df %>% filter(year==largest_year)
total_pop <- sum(df_lastest$total_pop)
total_pop
total_jail <- sum(df_lastest$total_jail_pop, na.rm=T)
total_jail
total_prison <- sum(df_lastest$total_prison_pop, na.rm=T)
total_prison

data_description <- list(nobs=nobs, smallest_year=smallest_year, 
                         largest_year=largest_year, 
                         total_pop=total_pop,
                         total_jail=total_jail,
                         total_prison=total_prison)

# Part II
incarceration_time_series <- incarceration_df %>% mutate(total_prison_pop=replace_na(total_prison_pop, 0),
                            white_prison_pop=replace_na(white_prison_pop, 0),
                            black_prison_pop=replace_na(black_prison_pop, 0),
                            latinx_prison_pop=replace_na(latinx_prison_pop, 0),
                            aapi_prison_pop=replace_na(aapi_prison_pop, 0),
                            native_prison_pop=replace_na(native_prison_pop, 0),
                            total_jail_pop=replace_na(total_jail_pop, 0),
                            white_jail_pop=replace_na(white_jail_pop, 0),
                            black_jail_pop=replace_na(black_jail_pop, 0),
                            latinx_jail_pop=replace_na(latinx_jail_pop, 0),
                            aapi_jail_pop=replace_na(aapi_jail_pop, 0),
                            native_jail_pop=replace_na(native_jail_pop, 0)
                            ) %>%
  group_by(year) %>% summarise(
    Total=sum(total_prison_pop + total_jail_pop),
    White=sum(white_prison_pop + white_jail_pop),
    Black=sum(black_prison_pop + black_jail_pop),
    Latinx=sum(latinx_prison_pop + latinx_jail_pop),
    AAPI=sum(aapi_prison_pop + aapi_jail_pop),
    Native=sum(native_prison_pop + native_jail_pop)
  ) %>% gather(key=Race, value=Pop, -year) %>% 
  mutate(Race=factor(Race, levels = c("Total", "White", "Black", "Latinx", "AAPI", "Native")))
incarceration_over_time_plot  <- ggplot(data=incarceration_time_series, aes(x=year, y=Pop, color=Race)) + geom_point() + geom_line() +
  scale_color_brewer(palette = "Dark2") +
  labs(x="Year", y="Population (# people)", title="Incarerated Population Over Time")
incarceration_over_time_plot 

## Part 3
top_10_black_incarceration_states_df <-
  incarceration_df %>% filter(year==2016) %>% mutate(total_prison_pop=replace_na(total_prison_pop, 0),
                                          black_pop_15to64=replace_na(black_pop_15to64, 0),
                                          total_pop_15to64=replace_na(total_pop_15to64,0),
                                          white_prison_pop=replace_na(white_prison_pop, 0),
                                          black_prison_pop=replace_na(black_prison_pop, 0),
                                          latinx_prison_pop=replace_na(latinx_prison_pop, 0),
                                          aapi_prison_pop=replace_na(aapi_prison_pop, 0),
                                          native_prison_pop=replace_na(native_prison_pop, 0),
                                          total_jail_pop=replace_na(total_jail_pop, 0),
                                          white_jail_pop=replace_na(white_jail_pop, 0),
                                          black_jail_pop=replace_na(black_jail_pop, 0),
                                          latinx_jail_pop=replace_na(latinx_jail_pop, 0),
                                          aapi_jail_pop=replace_na(aapi_jail_pop, 0),
                                          native_jail_pop=replace_na(native_jail_pop, 0)
) %>% group_by(state) %>% summarise(
    Total=sum(total_jail_pop+total_prison_pop)/sum(total_pop_15to64),
    Black=sum(black_prison_pop + black_jail_pop)/sum(black_pop_15to64),
  ) %>% arrange(desc(Black)) %>% head(10) %>% gather(key=Category, value=Percent, -state) %>%
  mutate(Category=factor(Category, levels=c("Total", "Black")), 
         state=factor(state, levels = c("CO", "NV", "NE", "AZ", "KY", "PA","IA", "WY", "OK", "WI")))

top_10_black_plot <- ggplot(data=top_10_black_incarceration_states_df, aes(x=state, y=Percent, fill=Category)) + 
  geom_bar(stat="identity", position = position_dodge()) +
  coord_flip() +
  labs(x="State", y="Percent Incarcerated", title="States with Highest Rate of Black Incarcerated") +
  scale_fill_manual(values=c("red", "black"))


## Part 4
library(maps)
incarceration_county <-  incarceration_df %>% filter(state=="WA") %>% filter(year==largest_year) %>% 
  mutate(
         white_prison_pop=replace_na(white_prison_pop, 0),
         black_prison_pop=replace_na(black_prison_pop, 0),
         white_jail_pop=replace_na(white_jail_pop, 0),
         black_jail_pop=replace_na(black_jail_pop, 0),
         black_pop_15to64=replace_na(black_pop_15to64, 0),
         white_pop_15to64=replace_na(white_pop_15to64, 0)
        
  )  %>% mutate(
    White=(white_jail_pop+white_prison_pop)/white_pop_15to64,
    Black=(black_prison_pop + black_jail_pop)/black_pop_15to64,
    rate=Black/White
  ) %>% select(fips, rate) 

fips <- county.fips  %>% separate(polyname, into=c("state", "subregion"), sep=",") %>%
  filter(state=="washington")
fips$subregion[fips$fips==53053] <- "pierce"
fips$subregion[fips$fips==53055] <- "san juan"
fips <- fips[!duplicated(fips$fips), ]
incarceration_county <- left_join(incarceration_county, fips)

library(ggthemes)

county <- map_data("county")
wa_county <- county %>% filter(region=="washington")
incarceration_county_map  <- left_join(wa_county, incarceration_county)
map_plot <- ggplot(incarceration_county_map, aes(x=long, y=lat, fill=rate, group=subregion)) + geom_polygon() +
  scale_fill_distiller(palette = "YlOrBr") + theme_light() +
  labs(x="", y="", title="Discrepancies between racial incarceration rates in WA", fill="ratio of \n black:white",
       caption="Display the ratio of black incarceration ratio to black incarceration rate.")


## Part 5
part5_plot <- incarceration_df %>% mutate(
  female_pop_15to64 = replace_na(female_pop_15to64, 0),
  male_pop_15to64 = replace_na(male_pop_15to64, 0),
  female_jail_pop=replace_na(female_jail_pop, 0),
  female_prison_pop=replace_na(female_prison_pop, 0),
  male_jail_pop=replace_na(male_jail_pop, 0),
  male_prison_pop=replace_na(male_prison_pop, 0),
) %>% filter(state %in% c("AL", "WA", "CA", "IL")) %>% group_by(state, year) %>%
  summarise(
    female=sum(female_jail_pop+female_prison_pop)/sum(female_pop_15to64),
    male=sum(male_jail_pop+male_prison_pop)/sum(male_pop_15to64),
  ) %>% gather(key=gender, value=rate, female:male) %>%
  ggplot(aes(x=year, y=rate, color=gender)) + geom_line() + facet_wrap(~state)
