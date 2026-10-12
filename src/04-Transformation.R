formula1 %>%
  filter(constructorName == "Alfa Romeo") %>%
  select(constructorName, constructorCountry, constructorImage)
