library(readxl)
library(tidyverse)

df <- read_xlsx("./data/cpv2020_b_eum_01_poblacion.xlsx", 
                sheet = "03",
                skip = 7)

names(df) <- c("Entidad", "Edad", "Total", "Hombres", "Mujeres", "Relacionhm")
head(df)


df2 <- df %>%
    filter(Entidad != "Estados Unidos Mexicanos") %>%
    filter(Edad != "Total") %>%
    filter(Edad != "No especificado") %>%
    mutate(age = as.numeric(substr(Edad,0,2))) %>%
    mutate(age_group = case_when(
        age >= 0 & age < 5 ~ "0 to 4",
        age >= 5 & age < 15 ~ "5 to 14",
        age >= 15 & age < 18 ~ "15 to 18",
        age >= 18 & age < 66 ~ "18 to 65",
        age >= 66 & age < 75 ~ "66 to 74",
        age >= 75 & age < 85 ~ "18 to 65",
        age >= 85 ~ "More than 85",
    )) %>%
    mutate(unit_of_care = case_when(
        age >= 0 & age < 5 ~ 2.0,
        age >= 5 & age < 15 ~ 1.5,
        age >= 15 & age < 18 ~ 1.2,
        age >= 18 & age < 66 ~ 1.0,
        age >= 66 & age < 75 ~ 1.2,
        age >= 75 & age < 85 ~ 1.5,
        age >= 85 ~ 2.0,
    ))

df2 %>%
    group_by(Entidad, age_group) %>%
    summarise(n = sum(Total)) %>%
    mutate(freq = n /sum(n) * 100)%>%
    filter(age_group != "18 to 65")%>%
    filter(age_group != "15 to 18")%>%
    filter(age_group != "66 to 74")%>%
    ggplot(aes(x = Entidad, y = freq, fill = age_group))+
    geom_bar(stat = "identity") +
    coord_flip()
