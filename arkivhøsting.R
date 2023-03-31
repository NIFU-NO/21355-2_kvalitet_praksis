## Formål:        Høste norske institusjonelle arkiver og 
##                filtrere bort irrelevante poster

# Nødvendige pakker -------------------------------------------------------

library(tidyverse)
library(oai)
library(rvest)


# Klargjøring for arkivhøsting --------------------------------------------

## Hente arkivoversikt og id for Brage-arkiv
brage_arkiver <- read_html("https://sikt.no/tjenester/brage-lokale-vitenarkiv") |>
  html_nodes(xpath = "/html/body/div/main/article/div[2]/div/figure/table") |>
  html_table(header = TRUE)
  
brage_arkiver <- brage_arkiver[[1]] |> 
  filter(!str_detect(`OAI-PMH URL`, "løpig")) |>
  mutate(url = paste0("https://", `OAI-PMH URL`, "request"),
         identifier = map(url, list_identifiers)) |>
  select(-`OAI-PMH URL`)


## UiT og UiO har separate vitenarkiv, disse må hentes separat
eksterne_arkiver <- tibble(Institusjon = c("UiT", "UiO"),
                           Vitenarkiv = c("Munin", "DUO"),
                           url = c("https://www.ub.uit.no/munin-oai/request",
                                   "https://duo.uio.no/oai/request"),
                           identifier = map(url, list_identifiers))


## Binde sammen til en tabell
arkiver <- bind_rows(brage_arkiver, 
                     eksterne_arkiver)



# Hente ID for alle arkivposter -------------------------------------------

## Hente ut ID per publikasjon
records <- arkiver |>
  select(Institusjon, 
         url, 
         identifier) |>
  unnest(identifier) |>
  select(Institusjon, 
         url, 
         identifier) |>
  nest_by(Institusjon, 
          url) |>
  mutate(ids = map(data, ~as.vector(t(.)))) |>
  select(-data) |>
  unnest(ids)

## Lagre ID-sett
saveRDS(records, "oai_ids.rds")



# Høsting av metadata -----------------------------------------------------

## Funksjon for å hente poster fra API i tilfelle feilmeldinger
safe_get <- safely(.f = get_records)

## Hente metadata for hver ID
records <- records |>
  mutate(results = map2(.x = url,
                        .y = ids,
                        ~safe_get(id = .y, url = .x)))

## Lagre fullstendige metadata
saveRDS(records, "arkiv_poster.rds")



# Filtrering av metadata for komplett utvalg ------------------------------


## Hente ut relevante metadata: dato, id, tittel, emneord, sammendrag,
##                              publikasjonstype, språk, lisensiering
arkiv_metadata <- records |>
  unnest_longer(results) |> 
  filter(results_id == "result") |> 
  unnest(results) |>
  ungroup() |> 
  hoist(results,
        date = list("metadata", "date"),
        identifier = list("metadata", "identifier"),
        title = list("metadata", "title"),
        subject = list("metadata", "subject"),
        description = list("metadata", "description"),
        type = list("metadata", "type"),
        language = list("metadata", "language"),
        rights = list("metadata", "rights")) |> 
  mutate(cristin_id = str_extract(identifier, "(?<=cristin:).\\d*")) |>
  select(-results, -results_id)

#Antall dokumenter totalt 17.02.23: 440581

## Filtrere bort duplikater og språk og 
## kategorier uten oppdragsrelevans
arkiv_metadata <- arkiv_metadata |>
  filter(str_detect(language, "[N|n]no") |
           str_detect(language, "[E|e]ng"),
         str_detect(type, "[P|p]eer") |
           str_detect(type, "[A|a]rticle") |
           str_detect(type, "[C|c]hapter") |
           str_detect(type, "[D|d]octoral") |
           str_detect(type, "[B|b]ook") |
           str_detect(type, "[R|r]eport") |
           str_detect(type, "ing [P|p]aper"))

## Antall dokumenter etter filtrering 17.02.23: 141976

## Filtrere for tidsperioden vi er interessert i
arkiv_metadata <- arkiv_metadata |>
  separate_rows(date, sep = ";") |> 
  filter(str_detect(date, "^\\d")) |>
  mutate(dato = str_extract(date, "^.{4}")) |> 
  mutate(dato = as.numeric(dato)) |>
  filter(!is.na(dato)) |>
  group_by(ids) |>
  slice(which.min(dato)) |>
  ungroup() |>
  filter(dato > 2007)

## Endelig antall dokumenter 17.02.23: 126982

## Lagre metadata
saveRDS(arkiv_metadata, "arkiv_poster_til_filtrering.rds")