library(fs)
library(here)
library(httr)

dir_create(path = here('2020-12-21_Arsenal/data'))

GET(
  "https://query.data.world/s/qtv72zcg4lvkqxq66owgr52uiuhbby",
  write_disk(path = here('2020-12-21_Arsenal/data/data.xlsx'))
)


