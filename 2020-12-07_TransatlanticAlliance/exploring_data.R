library(tidyverse)
library(patchwork)
library(ggtext)
library(showtext)
library(pdftools)

data <- readxl::read_xlsx(here::here('2020-12-07_TransatlanticAlliance/Week49.xlsx'))

data_good <- data %>%
  filter(Response == 'Good')

data_bad <- data %>%
  filter(Response == 'Bad')

data_us <- data %>%
  filter(Respondent == 'United States')

data_ger <- data %>%
  filter(Respondent == 'Germany')

font_add_google(name = 'Montserrat',family = 'montserrat')
font_add_google(name = 'Open Sans',family = 'opensans')
showtext_auto()

data_us %>%
  ggplot() +
  geom_hline(yintercept = 50, color = '#444444',alpha = 0.08) +
  geom_segment(
    aes(x = 2017, xend = 2017, y = 22, yend = 68), size = 1, col = '#444444'
  ) +
  geom_segment(
    aes(x = 2018, xend = 2018, y = 25, yend = 70), size = 1, col = '#444444'
  ) +
  geom_segment(
    aes(x = 2019, xend = 2019, y = 17, yend = 75), size = 1, col = '#444444'
  ) +
  geom_segment(
    aes(x = 2020, xend = 2020, y = 21, yend = 74), size = 1, col = '#444444'
  ) +
  geom_point(
    aes(x = Year, y = Value, color = Response),
    show.legend = FALSE,
    size = 4
  ) +
  geom_text(
    aes(x = Year, y = Value, label = glue::glue('{Value}%')),
    hjust = if_else(data_us$Response == 'Good',-.3,1.4),
    color = if_else(data_us$Response == 'Bad','#e34a27','#2384C0'),
    family = 'montserrat',
    fontface = 'bold'
  ) +
  labs(
    title = 'United States'
  ) +
  scale_y_continuous(limits = c(0,100), breaks = 50,labels = function(x) paste0(x, "%")) +
  scale_color_manual(values = c('#e34a27','#2384C0')) +
  coord_flip() +
  theme_void() +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.y = element_text(
      margin = margin(t = 10, b = 10, r = 10),
      color = '#444444',
      family = 'montserrat',
    ),
    plot.margin = margin(t = 10,l = 10, b = 20),
    plot.background = element_rect(color = '#f0f0f0',fill = '#f0f0f0'),
    plot.title = element_text(
      color = '#444444',
      family = 'montserrat',
      hjust = .5,
      margin = margin(t = 10, b = 20),
      size = 11.9,
      face = 'bold'
    ),
    axis.text.x = element_text(color = "#444444", family = 'montserrat', size = 8, margin = margin(t = 8))
  )  -> us

data_ger %>%
  ggplot() +
  geom_hline(yintercept = 50, color = '#444444',alpha = 0.08) +
  geom_segment(
    aes(x = 2020, xend = 2020, y = 18, yend = 79), size = 1, col = '#444444'
  ) +
  geom_segment(
    aes(x = 2019, xend = 2019, y = 34, yend = 64), size = 1, col = '#444444'
  ) +
  geom_segment(
    aes(x = 2018, xend = 2018, y = 24, yend = 73), size = 1, col = '#444444'
  ) +
  geom_segment(
    aes(x = 2017, xend = 2017, y = 56, yend = 42), size = 1, col = '#444444'
  ) +
  geom_point(
    aes(x = Year, y = Value, color = Response),
    show.legend = FALSE,
    size = 4
  ) +
  geom_text(
    aes(x = Year, y = Value, label = glue::glue('{Value}%')),
    hjust = if_else(data_us$Response == 'Bad',-.3,1.4),
    fontface = 'bold',
    color = if_else(data_us$Response == 'Bad','#e34a27','#2384C0'),
    family = 'montserrat'
  ) +
  labs(
    title = 'Germany'
  ) +
  scale_color_manual(values = c('#e34a27','#2384C0')) +
  coord_flip() +
  theme_void() +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    # axis.text.y = element_text(
    #   margin = margin(t = 10, b = 10, r = 10),
    #   color = '#444444',
    #   family = 'montserrat',
    # ),
    plot.margin = margin(t = 10,l = 10, b = 20),
    plot.background = element_rect(color = '#f0f0f0',fill = '#f0f0f0'),
    plot.title = element_text(
      color = '#444444',
      family = 'montserrat',
      hjust = .5,
      margin = margin(t = 10, b = 20), face = 'bold', size = 11.9
    ),
    axis.text.x = element_text(color = "#444444", family = 'montserrat', size = 8, margin = margin(t = 8))
  )+
  scale_y_continuous(limits = c(0,100), breaks = 50,labels = function(x) paste0(x, "%"))-> ger

us + ger +
  plot_annotation(
    title = 'How would you rate the current relationship between Germany and the United States?',
    subtitle = 'In the US predominantly people see the relationship with Germany in <b style="color:#2384C0; size:40">GOOD</b> standing, but more Germans now see it <b style="color:#e34a27">BAD</b>',
    caption = "#MakeoverMonday 2020, 49 | @pedro_drocha | Data: Körber-Stiftung",
    theme = theme(
      plot.background = element_rect(fill = '#f0f0f0', color = '#f0f0f0'),
      plot.caption = element_text(
        hjust = 0, color = '#44444490',family = 'montserrat', size = 6
      ),
      plot.title = element_text(
        hjust = 0, color = '#444444', family = 'opensans', size = 16.1, margin = margin(t = 10, b = 5), face = 'bold'
        ),
      plot.subtitle = element_markdown(
        hjust = 0, color = '#444444', family = 'opensans', size = 11.2, margin = margin(b = 5, r=50, t = 3)
      ),
      plot.margin = margin(l = 10, b = 10, t = 10, r = 10)
    )
  ) -> plot_ing

plot_ing
us + ger +
  plot_annotation(
    title = 'Como você avalia a relação atual entre Estados Unidos e Alemanha?',
    subtitle = 'Em entrevistas conduzidas pelo Pew Research Center e o Körber-Stiftung antes das eleições presidenciais <br>nos EUA, estadunidenses continuam percebendo a relação com a Alemanha como <b style="color:#2384C0; size:16;">BOA</b>, mas cada vez<br> mais alemães avaliam a relação com os EUA como <b style="color:#e34a27">RUIM</b>',
    caption = "#MakeoverMonday 2020, 49 | @pedro_drocha | Dados: Körber-Stiftung",
    theme = theme(
      plot.background = element_rect(fill = '#f0f0f0', color = '#f0f0f0'),
      plot.caption = element_text(
        hjust = 0, color = '#44444490',family = 'montserrat', size = 6
      ),
      plot.title = element_text(
        hjust = 0, color = '#444444', family = 'opensans', size = 15, margin = margin(t = 10, b = 5), face = 'bold'
      ),
      plot.subtitle = element_markdown(
        hjust = 0, color = '#444444', family = 'opensans', size = 11, margin = margin(b = 5, r=50)
      ),
      plot.margin = margin(l = 10, b = 10, t = 10)
    )
  ) -> plot_pt

plot_pt

ggsave(here::here('2020-12-07_TransatlanticAlliance/plotMMVizReview2.pdf'),
       plot = plot_ing,
       device = cairo_pdf)

pdf_convert(pdf = here::here('2020-12-07_TransatlanticAlliance/plotMMVizReview2.pdf'),
            format = "png", dpi = 400)

