## Formål:  Konstruere publikasjonssett fra arkiv- og Cristinposter
##          og filtrere de basert på lekikalske termer


# Nødvendige pakker -------------------------------------------------------

library(tidyverse)
library(quanteda)


# Data --------------------------------------------------------------------

## Publikasjoner fra arkiver
arkiv_data <- readRDS("arkiv_poster_til_filtrering.rds")

## Publikasjoner registrert i Cristin, som ikke er arkivert
cristin_data <- read_csv("nvi_2011_2021.csv") |> 
  filter(!VARBEIDLOPENR %in% arkiv_data$cristin_id) |> 
  select(id = VARBEIDLOPENR,
         title = TITTELTEKST_ORIGINAL) |> 
  mutate(id = as.character(id))

## Leksikon
leksikon <- dictionary(file = "leksikon.yml")



# Preprosessering av data før filtrering ----------------------------------

## Filtrere arkivposter på år og slå sammen kategorier
arkiv_data_df <- arkiv_data |>
  filter(dato > 2012) |>
  mutate(type = case_when(str_detect(type, "article") ~ "article",
                          str_detect(type, "Doctoral") ~ "phdthesis",
                          str_detect(type, "Master") ~ "mastersthesis",
                          str_detect(type, "Chapter") ~ "incollection",
                          str_detect(type, "Working") ~ "techeport",
                          str_detect(type, "eport") ~ "techreport",
                          str_detect(type, "Book") ~ "book",
                          str_detect(type, "onference") ~ "inproceedings",
                          str_detect(type, "Peer reviewed") ~ "article",
                          str_detect(type, "Version") ~ "article",
                          TRUE ~ "misc")) |> 
  select(note = ids,
         title = title,
         abstract = description,
         type,
         institution = Institusjon,
         year = dato,
         keyword = subject,
         cristin_id)

## Fjerne cristinposter som allerede er blant arkivpostene
cristin_data_df <- cristin_data |>
  filter(!id %in% arkiv_data_df$cristin_id) |> 
  rename(note = id)

## Slå sammen arkiv- og Cristinposter
corpus_data <- arkiv_data_df |>
  select(-cristin_id) |>
  bind_rows(cristin_data_df) |>
  filter(!is.na(title)) |> 
  distinct(title, .keep_all = TRUE)



# Korpus ------------------------------------------------------------------

## Opprette tittelkorpus
titler_corpus <- corpus(corpus_data |>
                          filter(!is.na(title)) |> 
                          distinct(note, .keep_all = TRUE),
                        docid_field = "note",
                        text_field = "title")

## Opprette sammendragskorpus
sammendrag_corpus <- corpus(corpus_data |>
                              filter(!is.na(abstract)) |> 
                              distinct(note, .keep_all = TRUE),
                            docid_field = "note",
                            text_field = "abstract")

## Filtrere ord i tittel på leksikale begreper
titler_kwic <- titler_corpus |> 
  tokens() |> 
  kwic(pattern = leksikon)

## Filtrere ord i sammendrag på leksikale begreper
sammendrag_kwic <- sammendrag_corpus |> 
  tokens() |> 
  kwic(pattern = leksikon)

## Hjelpefunksjon for å slå sammen treff i leksikon
check_uniq <- function(vec){
  if (length(unique(vec)) == 1){
    return(unique(vec))
  } else {
    return(paste(vec,
                 collapse = "; "))
  }
}

## Konvertere filtrert tittelkorpus til tabell
titler_df <- titler_kwic |>
  as_tibble() |> 
  select(id = docname, 
         "nøkkelord_tittel" = keyword,
         kategori = pattern) |>
  mutate(id = str_remove(id, pattern = "\\.\\d.*$"),
         nøkkelord_tittel = tolower(nøkkelord_tittel),
         kategori = as.character(kategori)) |> 
  distinct(id, nøkkelord_tittel, .keep_all = TRUE) |> 
  group_by(id) |> 
  summarise(nøkkelord_tittel = list(nøkkelord_tittel),
            kategori = list(kategori),
            .groups = "drop") |> 
  mutate(nøkkelord_tittel = map(nøkkelord_tittel,
                                check_uniq),
         kategori = map(kategori,
                        check_uniq))

## Konvertere filtrert sammendragskorpus til tabell
sammendrag_df <- sammendrag_kwic |>
  as_tibble() |> 
  select(id = docname, 
         "nøkkelord_sammendrag" = keyword,
         kategori = pattern) |>
  mutate(id = str_remove(id, pattern = "\\.\\d.*$"),
         nøkkelord_sammendrag = tolower(nøkkelord_sammendrag),
         kategori = as.character(kategori)) |> 
  distinct(id, nøkkelord_sammendrag, .keep_all = TRUE)  |> 
  group_by(id) |> 
  summarise(nøkkelord_sammendrag = list(nøkkelord_sammendrag),
            kategori = list(kategori),
            .groups = "drop") |> 
  mutate(nøkkelord_sammendrag = map(nøkkelord_sammendrag,
                                    check_uniq),
         kategori = map(kategori,
                        check_uniq))

## Slå sammen tittel- og sammendragstabeller
data_df <- corpus_data |>
  inner_join(titler_df, 
             by = c("note" = "id")) |>
  left_join(sammendrag_df, 
            by = c("note" = "id", "kategori")) |>
  mutate(empty = if_else(is.na(nøkkelord_tittel) & 
                           is.na(nøkkelord_sammendrag), 
                         TRUE, 
                         FALSE)) |> 
  filter(!empty) |>
  select(-empty)

## Bedre formattering av resultatene
full_df <- data_df |>
  mutate(url = if_else(str_detect(note, "oai:"),
                       paste0("https://hdl.handle.net/",
                              str_remove(note, "^.*:")),
                       paste0("https://app.cristin.no/results/show.jsf?id=",
                              note)),
         nøkkelord_tittel = as.character(nøkkelord_tittel),
         nøkkelord_sammendrag = as.character(nøkkelord_sammendrag),
         nøkkelord_sammendrag = if_else(nøkkelord_sammendrag == "NULL",
                                        NA_character_,
                                        nøkkelord_sammendrag),
         kategori = as.character(kategori)) |> 
  distinct(note, .keep_all = TRUE)

## Ytterligere filtrering etter gjennomgang av irrelevante leksikale treff
filtrert_df <- full_df |> 
  anti_join(full_df |> 
              filter(str_detect(nøkkelord_tittel, "[N|n]ursing"),
                     str_detect(title, "[N|n]ursing home")),
            by = "note") |> 
  anti_join(full_df |> 
              filter(str_detect(nøkkelord_sammendrag, "[N|n]ursing"),
                     str_detect(title, "[N|n]ursing home")),
            by = "note") |> 
  anti_join(full_df |> 
              filter(
                str_detect(nøkkelord_tittel, 
                           "[B|b]achel*") | 
                  str_detect(nøkkelord_tittel,
                             "[H|h]øyere utd") |
                  str_detect(nøkkelord_tittel,
                             "[H|h]øyskoleu") |
                  str_detect(nøkkelord_tittel,
                             "[U|u]ndergraduate") |
                  str_detect(nøkkelord_tittel, 
                             "[H|h]igher ed") &
                  kategori == "HØYERE UTDANNING"),
            by = "note")




## Eksportere til .csv
write_excel_csv2(filtrert_df,
                 file = "poster_filtrert.csv",
                 na = "")