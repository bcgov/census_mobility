#' Goal: to be able to study occupational mobility by linking the 2016 and the 2021 census. Ideally
#' at the 5 digit NOC level, but sadly this is not possible: The transition matrix would have
#' 512^2-512=261,632 off diagonal cells, and the 1/16th census sample (stratified by age) would not be
#' large enough to ensure the off diagonal cells are not suppressed.
#'
#' This script creates an custom grouping of NOCs where the smallest group represents just under 1%
#' of the labour market, whereas the largest group is around 3%. The result is 67 occupational groupings,
#' yielding a much less sparse 67^2=4,489 cell transition matrix.

library(tidyverse)
library(here)
library(janitor)
library(conflicted)
conflicts_prefer(dplyr::filter)
options(scipen = 999)
threshold <- .01 #groups larger than this can stand alone.
#functions---------------------
dominant_desc <- function(tbbl) {
  top <- tbbl |> slice_max(prop, n = 1)
  paste(top$desc, "& related occupations")
}
#read in the data---------------------------
noc_counts <- read_csv(here("data", "census_noc_counts.csv"))|>
  clean_names()|>
  rename(noc_desc=contains("noc"))|>
  mutate(noc_2021=str_sub(noc_desc, 1,5),
         desc=str_sub(noc_desc, 7))|>
  group_by(noc_2021, desc)|>
  summarize(value=sum(value))|>
  ungroup()|>
  mutate(prop=value/sum(value),
         noc5=noc_2021,
         noc4=str_sub(noc_2021, 1,4),
         noc3=str_sub(noc_2021, 1,3),
         noc2=str_sub(noc_2021, 1,2),
         noc1=str_sub(noc_2021, 1,1)
         )
#These 5 digit NOCs are large enough to form their own grouping (no aggregation necessary)
in_large5 <- noc_counts|>
  filter(prop>threshold)|>
  mutate(custom_group=noc_2021)|>
  select(custom_group, custom_desc=desc, noc_2021, prop)

#This removes the above occupations (already mapped)
remaining4 <- anti_join(noc_counts, in_large5, by=join_by(noc_2021))

#This is the set of 4 digit aggregates that are large enough to form a group.
large4 <- remaining4|>
  group_by(noc4)|>
  nest()|>
  mutate(custom_desc=map_chr(data, dominant_desc))|>
  unnest(data)|>
  group_by(noc4, custom_desc)|>
  summarize(prop=sum(prop))|>
  filter(prop>threshold)|>
  select(-prop)

#this is the set of occupations that are contained in the 4 digit aggregates above.
in_large4 <- left_join(large4, remaining4, by=join_by(noc4))|>
  select(custom_group=noc4, custom_desc, noc_2021, prop)

#this removes the above occupations (already mapped)
remaining3 <- anti_join(remaining4, in_large4, by = join_by(noc_2021))

#This is the set of 3 digit aggregates that are large enough to form a group.
large3 <- remaining3|>
  group_by(noc3)|>
  nest()|>
  mutate(custom_desc=map_chr(data, dominant_desc))|>
  unnest(data)|>
  group_by(noc3, custom_desc)|>
  summarize(prop=sum(prop))|>
  filter(prop>threshold)|>
  select(-prop)

#this is the set of occupations that are contained in the 3 digit aggregates above.
in_large3 <- left_join(large3, remaining3, by=join_by(noc3))|>
  select(custom_group=noc3, custom_desc, noc_2021, prop)

#this removes the above occupations (already mapped)
remaining2 <- anti_join(remaining3, in_large3, by = join_by(noc_2021))

#This is the set of 2 digit aggregates that are large enough to form a group.
large2 <- remaining2|>
  group_by(noc2)|>
  nest()|>
  mutate(custom_desc=map_chr(data, dominant_desc))|>
  unnest(data)|>
  group_by(noc2, custom_desc)|>
  summarize(prop=sum(prop))|>
  filter(prop>threshold)|>
  select(-prop)

#this is the set of occupations that are contained in the 2 digit aggregates above.
in_large2 <- left_join(large2, remaining2, by=join_by(noc2))|>
  select(custom_group=noc2, custom_desc, noc_2021, prop)

#this removes the above occupations (already mapped)
remaining1 <- anti_join(remaining2, in_large2, by = join_by(noc_2021))

#This is the set of 1 digit aggregates that are large enough to form a group.
large1 <- remaining1|>
  group_by(noc1)|>
  nest()|>
  mutate(custom_desc=map_chr(data, dominant_desc))|>
  unnest(data)|>
  group_by(noc1, custom_desc)|>
  summarize(prop=sum(prop))|>
  filter(prop>threshold)|>
  select(-prop)

#this is the set of occupations that are contained in the 1 digit aggregates above.
in_large1 <- left_join(large1, remaining1, by=join_by(noc1))|>
  select(custom_group=noc1, custom_desc, noc_2021, prop)

#this removes the above occupations (already mapped)
remaining0 <- anti_join(remaining1, in_large1, by = join_by(noc_2021))

# 4 out of the 5 remaining occupations are noc3=632
noc_632 <- remaining0|>
  filter(str_sub(noc_2021, 1,3)=="632")
noc_632$custom_desc <- dominant_desc(noc_632)
noc_632$custom_group <- "632"
noc_632 <- noc_632|>
  select(custom_group, custom_desc, noc_2021, prop)

#very few legislators... wrap them in with senior managers
noc_0001 <- anti_join(remaining0, noc_632, by = join_by(noc_2021))|>
  mutate(custom_group="0001",
         custom_desc="Senior Managers & Legislators")|>
  select(custom_group, custom_desc, noc_2021, prop)

#change senior managers custom_group to wrap in legislators

in_large5$custom_group[in_large5$noc_2021=="00018"] <- "0001"
in_large5$custom_desc[in_large5$noc_2021=="00018"] <- "Senior Managers & Legislators"

# bind all the levels together----------------------

custom_mapping_2021 <- bind_rows(in_large5, in_large4, in_large3, in_large2, in_large1, noc_632, noc_0001)|>
  arrange(custom_group, desc(prop))

#sanity checks
stopifnot(near(sum(custom_mapping_2021$prop),1))
stopifnot(nrow(noc_counts)==nrow(custom_mapping_2021))

rolled_up <- custom_mapping_2021|>
  group_by(custom_group, custom_desc)|>
  summarize(prop=sum(prop))

hist(rolled_up$prop)

write_csv(custom_mapping_2021, here("out", "custom_mapping.csv"))







