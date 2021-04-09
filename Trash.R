### Forgot the number values are classes not ratings

#by.sp <- survey.dat %>% 
select(Frequency_General:Frequency_Oysters, Where_Tuna:Preference_Oysters)

#This doesn't work - reorders the data so one person's responses aren't together. This prob matters?
by.sp.gathered <- by.sp %>% 
  pivot_longer(Frequency_Tuna:Frequency_Oysters, names_to = "Species", values_to = "Frequency", names_prefix = "Frequency_") %>% 
  pivot_longer(Where_Tuna:Where_Oysters, names_to = "Species_where", values_to = "Where") %>% 
  pivot_longer(Form_Tuna:Form_Oysters, names_to = "Species_form", values_to = "Form") %>% 
  pivot_longer(Preference_Tuna:Preference_Oysters, names_to = "Species_Pref", values_to = "Preference")

frequency.sp <- select(survey.dat, Frequency_Tuna:Frequency_Oysters)

frequncy.sp.summary <- as.data.frame(table(frequency.sp$Frequency_Tuna))
frequency.sp.mean <- frequency.sp %>% 
  summarize(mean_Tuna = mean(Frequency_Tuna),
            mean_Shrimp = mean(Frequency_Shrimp),
            mean_Salmon = mean(Frequency_Salmon),
            mean_Flounder = mean(Frequency_Flounder),
            mean_BlueCrab = mean(Frequency_BlueCrab),
            mean_Clams = mean(Frequency_Clams),
            mean_Mullet = mean(Frequency_Mullet),
            mean_Oysters = mean(Frequency_Oysters))

### Trying to create a function to summarize where data and failed

where.sp.summary <- as.data.frame(table(where.sp$Where_Tuna))
where.sp.summary <- rename(where.sp.summary, Response = Var1)

where.sp.fun <- function(col, df) {
  
  for (col in df) {
    x <- as.data.frame(table(df$col))
    left_join(where.sp.summary, x, by = "Var1")
  }
  
}

col <- c("Where_Shrimp", "Where_Salmon", "Where_Flounder", "Where_BlueCrab", "Where_Clams", "Where_Mullet", "Where_Oysters")
where.sp.fun(col, where.sp)
