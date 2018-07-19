library(jsonlite)
library(tidyverse)

# Закачуємо та об'єднуємо дані голосувань та словники змінних
# з Порталу відкритих даних

all_votings <- read_csv("http://data.rada.gov.ua/ogd/zal/ppz/skl8/plenary_result_by_name-skl8.csv")

factions_dict <- fromJSON(readLines("http://data.rada.gov.ua/ogd/zal/ppz/skl8/dict/factions.json"))[[1]] %>% 
  rename(faction = name) %>% 
  mutate(faction = recode(faction,
                                 `Група "Воля народу"` = "Воля народу",
                                 `Група "Партія "Відродження"` = "Відродження",
                                 `Група "Відродження"` = "Відродження",
                                 `Група "Економічний розвиток"` = "Відродження",
                                 `Фракція ПАРТІЇ "БЛОК ПЕТРА ПОРОШЕНКА"` = "Блок Петра Порошенка",
                                 `Фракція політичної партії "Всеукраїнське об’єднання "Батьківщина"` = "Батьківщина",
                                 `Фракція  Політичної партії "НАРОДНИЙ ФРОНТ"` = "Народний фронт",
                                 `Фракція Політичної партії "Опозиційний блок"` = "Опозиційний блок",
                                 `Фракція Радикальної партії Олега Ляшка` = "Радикальна партія Ляшка",
                                 `Фракція Політичної партії "Об’єднання "САМОПОМІЧ"` = "Самопоміч"))

# comments_dict <- fromJSON(readLines("http://data.rada.gov.ua/ogd/zal/ppz/skl8/dict/komments.json"))[[1]] 
mps_dict <-   fromJSON(readLines("http://data.rada.gov.ua/ogd/zal/ppz/skl8/dict/mps.json"))[[1]]
result_dict <- data.frame(result = c(0:5),
          result_char = c("Відсутній", "За", "Проти", "Утримався", "Не голосував", "Присутній"))

questions_dict <- read_csv("http://data.rada.gov.ua/ogd/zal/ppz/skl8/plenary_agenda-skl8.csv")
events_dict <- read_csv("http://data.rada.gov.ua/ogd/zal/ppz/skl8/plenary_event_question-skl8.csv")

all_votings_full <- all_votings %>%
  mutate(id_mp = as.character(id_mp),
         id_fraction = as.character(id_fraction)) %>% 
  left_join(factions_dict, c("id_fraction" = "id")) %>% 
  left_join(mps_dict) %>% 
  rename(fullname = name) %>% 
  left_join(result_dict) %>% 
  left_join(select(events_dict, -date_agenda, -id_question), by = "id_event") 

# Відфільтровуємо голосування з решти подій
# Може працювати довго (хвилин 10) і при цьому підвисати - це нормально,
# дуже великий датафрейм

all_votings_full <- all_votings_full %>% 
  filter(str_detect(name_event, "голосування")) %>%
  select(-id_mp, -id_fraction, -result, -sex)

# Записуємо в файл даних RDS (спеціальний формат R, менший за обсягом і швидше працює,
# але не підходить для роботи в інших середовищах)

create.dir("data")
saveRDS(all_votings_full, file = "data/all_votings_full.rds")

# Тільки голосування "в цілому" (вирішальні для законів)

only_general <- all_votings_full %>% 
  filter(str_detect(name_event, "в цілому")) %>% 
  mutate(vote_simple = ifelse(result_char == "За", 1, 0))

saveRDS(only_general, file = "data/only_general.rds")
