## Formål:  Generere en BibTex-fil av en tabell med metadata fra Cristin
##          og norske arkiver. Denne kan så importeres i referansehåndterings-
##          verktøy som Zotero eller EndNote.


# Nødvendige pakker -------------------------------------------------------

library(tidyverse)
library(bib2df)


# Lese inn data og fjerne irrelevante felter ------------------------------

df <- read_csv2("poster_filtrert.csv") |> 
  select(bibtexkey = note,
         title,
         abstract,
         category = type,
         institution,
         year,
         keyword,
         url) |> 
  mutate(author = "",
         category = if_else(is.na(category),
                            "misc", category))

## Gjøre kolonnenavn gjenkjennelige for df2bib-funksjonen
names(df) <- toupper(names(df))

# Generere bibliografi-fil
df2bib(df, "kvalitet_bibliografi.bib")