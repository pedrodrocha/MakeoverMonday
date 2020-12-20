library(tidyverse)
library(patchwork)
library(ggtext)
library(showtext)
library(pdftools)

dat <- readxl::read_xlsx(here::here('2020-12-14_BobRoss/raw_data/elements-by-episode.xlsx'))

dat %>%
  group_by(Element) %>%
  summarise(
    Total = sum(Included)
  ) %>%
  ungroup() %>%
  mutate(
    Percent = round((Total/length(unique(dat$Episode))) * 100)
  ) %>%
  arrange(desc(Percent)) %>%
  mutate(
    Element =
      case_when(
        Element == 'Tree' ~ 'At least one tree',
        Element == 'Trees' ~ 'At least two trees',
        Element == 'Deciduous' ~ 'Deciduous tree',
        Element == 'Conifer' ~ 'Coniferous tree',
        Element == 'Mountain' ~ 'At least one mountain',
        Element == 'Snowy Mountain' ~ 'Snow-covered mountain',
        Element == 'Mountains' ~ 'At least two mountains',
        Element == 'Cumulus' ~ 'Cumulus Clouds',
        Element == 'Structure' ~ 'Man-made structure',
        Element == 'Winter' ~ 'Winter setting',
        Element == 'Framed' ~ 'Frame',
        Element == 'SUN' ~ 'Sun',
        Element == 'Cirrus' ~ 'Cirrus clouds',
        Element == 'FOG' ~ 'Fog',
        Element == 'Night' ~ 'Nighttime',
        TRUE ~ Element
      )
  ) %>%
  filter(Element != 'Steve Ross') %>%
  filter(Percent > 0) -> prep

prep$bullet <- 100

font_add_google(name = 'Montserrat',family = 'montserrat')
font_add_google(name = 'Open Sans',family = 'opensans')
font_add_google(name = 'Allura',family = 'allura')
showtext_auto()

prep %>%
  ggplot(aes(y = reorder(Element, Percent), x = Percent)) +
  geom_col(aes(x = bullet), fill = '#444444', alpha = .1) +
  geom_col(fill = '#005C9F') +
  geom_vline(xintercept = 0, color = 'whitesmoke') +
  geom_vline(xintercept = 25, color = 'whitesmoke') +
  geom_vline(xintercept = 50, color = 'whitesmoke') +
  geom_vline(xintercept = 75, color = 'whitesmoke') +
  geom_vline(xintercept = 100, color = 'whitesmoke') +
  geom_text(
    aes(y = Element, x = Percent, label = if_else(Percent == 90, '90%',as.character(Percent))),
    hjust = -.25,
    color = '#005C9F',
    fontface = 'bold',
    family = 'montserrat'
  ) +
  scale_x_continuous(labels = function(x) { paste0(x,'%')}) +
  theme_minimal() +
  labs(
    title = 'The paintings of Bob Ross',
    subtitle = '% containing each element',
    caption = "#MakeoverMonday 2020, 50 | @pedro_drocha | Dados: FiveThirtyEight"
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(
      fill = '#F0f1f0',
      color = '#f0f1f0'
    ),
    axis.title = element_blank(),
    axis.text = element_text(
      family = 'montserrat',
      size = 12,
      color = '#444444'
    ),
    axis.ticks.x = element_line(color = "whitesmoke", size = 3),
    plot.title = element_text(
      hjust = 1.55,
      face = 'bold',
      family = 'opensans',
      size = 43,
      color = '#444444',
      margin = margin(b = 10, t = 10)
      ),
    plot.subtitle = element_text(
      hjust = -.7,
      family = 'allura',
      size = 30,
      color = '#444444',
      margin = margin(t= 0, b = 30)
    ),
    plot.caption = element_text(
      hjust = -.7,
      vjust = -9.5,
      color = '#44444490',
      family = 'montserrat',
      size = 6.6
    ),
    plot.margin =  margin(20,20,20,20)

  ) -> plot

ggsave(
  plot = plot,
  filename = here::here('2020-12-14_BobRoss/plot.pdf'),
  device = cairo_pdf,
  width = 9,
  height = 11,
  dpi = 400
)

pdf_convert(pdf = here::here('2020-12-14_BobRoss/plot.pdf'),
            format = "png", dpi = 400)

