
# Приклад: Виборчий кодекс
# Вставляємо id з результатів голосувань сайту Ради
# (як тут: http://w1.c1.rada.gov.ua/pls/radan_gs09/ns_golos_print?g_id=15333&vid=1)

voting <- get_voting_results(15333)

# Візуалізація - змінити назву графіка та файлу, за потреби адаптувати розмір

visualize_voting(voting, title = "ГОЛОСУВАННЯ ЗА ВИБОРЧИЙ КОДЕКС") +
  labs(caption = "За даними офіційного сайту Верховної Ради") +
  theme(plot.caption = element_text(hjust = -0.4)) +
  ggsave(filename = "elections.png", width = 678, height = 363, dpi = 400, 
       device = png, limitsize = F)

# Більше одного голосування
# Даємо два вектори в аргументах: id голосувань і позначення законопроектів

voting <- get_voting_results(c(15118, 15333), c("ВИБОРИ НАРДЕПІВ", "ВИБОРЧИЙ КОДЕКС"))
visualize_voting(voting)

# аргумент voting_order: можемо відсортувати за обраним ЗП

visualize_voting(voting, voting_order = "ВИБОРИ НАРДЕПІВ")
