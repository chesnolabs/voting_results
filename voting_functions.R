library(tidyverse)
library(stringr)
library(rvest)
library(jsonlite)
library(extrafont)
library(forcats)

get_mps <- function(){
  mps <- read_csv("http://data.rada.gov.ua/ogd/mps/skl8/mps08-data.csv") %>% 
    filter(is.na(date_end))
  return(mps)  
}

get_all_mps <- function(){
  mps <- fromJSON(readLines(file("http://data.rada.gov.ua/ogd/mps/skl8/mps-data.json",
                                 encoding = "UTF-16")))
  mps_list <- mps
  mps <- mps_list[[1]]
  mps$fullname <- with(mps, paste(surname, firstname, patronymic))
  mps$shortname <- paste0(mps$surname, " ", 
                          str_sub(mps$firstname, 1, 1), ".", 
                          str_sub(mps$patronymic, 1, 1), ".")
  mps$shortname <- gsub("'", "’", mps$shortname)
  mps$shortname[mps$fullname == "Тимошенко Юлія Володимирівна"] <- "Тимошенко Юлія В."
  mps$shortname[mps$fullname == "Тимошенко Юрій Володимирович"] <- "Тимошенко Юрій В."
  mps$fullname[mps$surname == "Найєм"] <- "Найєм Мустафа-Масі"
  mps$shortname[mps$surname == "Найєм"] <- "Найєм М. ."
  mps$fullname[mps$surname == "Джемілєв"] <- "Джемілєв Мустафа"
  mps$shortname[mps$surname == "Джемілєв"] <- "Джемілєв М. ."
  mps$absence_s <- with(mps,
                        round(presentAuto_absent/(presentAuto_absent+presentAuto_present)*100, 1))
  mps$mazhor <- ifelse(is.na(mps$district_num), "l", "m")
  return(mps)
}

get_voting_results_one <- function(number, v_name = NULL, old = F, convocation = 0){
  if(old == T){
    links <- paste0("http://w1.c1.rada.gov.ua/pls/radan_gs09/ns_arh_golos_print?g_id=",
                    number, "&vid=1&n_skl=", convocation)
  } else {
  
    links <- paste0("http://w1.c1.rada.gov.ua/pls/radan_gs09/ns_golos_print?g_id=",
                  number, "&vid=1")
  }
  
  html <- read_html(links, encoding = "Windows-1251")

  factions <- html %>% 
    html_nodes("center b") %>% 
    html_text()
  
  results <- html %>% 
    html_nodes("center") %>% 
    html_text()
  
  shortnames <- html %>% 
    html_nodes(".hcol1") %>% 
    html_text()
  
  votes <- html %>% 
    html_nodes(".hcol1+ td") %>% 
    html_text()
  
  descr <- html %>% 
    html_nodes(".f2") %>% 
    html_text()
  
  ndep_factions <- str_extract(results, "[:digit:]{1,3}")
  
  voting_df <- data.frame(shortname = shortnames, vote = votes,
                          faction = rep(factions, times = ndep_factions)) %>% 
    mutate(faction = str_replace(faction, "  ", " "),
           faction = recode(faction, 
                            `Група "Воля народу"` = "Воля народу",
                            `Група "Партія "Відродження"` = "Відродження",
                            `Група "Відродження"` = "Відродження",
                            `Група "Економічний розвиток"` = "Відродження",
                            `Фракція ПАРТІЇ "БЛОК ПЕТРА ПОРОШЕНКА"` = "Блок Петра Порошенка",
                            `Фракція політичної партії "Всеукраїнське об’єднання "Батьківщина"` = "Батьківщина",
                            `Фракція Політичної партії "НАРОДНИЙ ФРОНТ"` = "Народний фронт",
                            `Фракція Політичної партії "Опозиційний блок"` = "Опозиційний блок",
                            `Фракція Радикальної партії Олега Ляшка` = "Радикальна партія Ляшка",
                            `Фракція Політичної партії "Об’єднання "САМОПОМІЧ"` = "Самопоміч"),
           vote_simple = ifelse(vote == "За", "Голосували за", "Не голосували за"),
           vote_simple = relevel(as.factor(vote_simple), "Не голосували за"),
           shortname = as.character(shortname))
  
  voting_df$shortname[voting_df$shortname == "Тимошенко Ю.В." &
                        voting_df$faction == "Народний фронт"] <- "Тимошенко Юрій В."
  voting_df$shortname[voting_df$shortname == "Тимошенко Ю.В." &
                        voting_df$faction == "Батьківщина"] <- "Тимошенко Юлія В."
  
  if(!missing(v_name)){
    voting_df <- mutate(voting_df, v_name = v_name)
  }
  
  return(voting_df)
  Sys.sleep(0.05)
}

get_voting_results <- function(number, v_name){
  if(length(number) == 1){
    voting_df <- get_voting_results_one(number, v_name)
  } else {
    voting_df <- map2(number, v_name, get_voting_results) %>% 
      bind_rows()
  }
  return(voting_df)
}

visualize_voting <- function(voting_df, title = NULL,
                             voting_order = NULL){
  if(length(colnames((voting_df))) == 4){
    
  votes_summary <- voting_df %>% 
    group_by(faction) %>% 
    summarize(pro_n = sum(vote_simple == "Голосували за"), pro_perc = round(mean(vote_simple == "Голосували за")*100, 0)) %>% 
    arrange(pro_perc, faction)
  
  faction_order <- votes_summary %>% 
    select(faction) %>% unlist()
  
  voting_df$faction <- as.factor(voting_df$faction)
  voting_df$faction <- factor(voting_df$faction, 
                              levels = faction_order)
  levels(voting_df$vote_simple)
  voting_df$vote_simple
  
  g <- ggplot(voting_df, aes(x=faction, fill = vote_simple)) +
    geom_bar(pos = "fill") +
    coord_flip() +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c("Голосували за" = "purple 4", "Не голосували за" = "grey 90"),
                      guide = guide_legend(reverse = TRUE)) +
    ggtitle(title) +
    theme(text = element_text(family = "PF DinText Pro"),
          axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size = 13, face = "bold"),
          legend.position = "bottom",
          plot.title = element_text(size = 14, hjust = 0.5, color = "black", face = "bold")) +
    theme(strip.text.x = element_text(size = 13, color = "black")) 
  
  color_vote <- ifelse(votes_summary$pro_perc > 0, "grey 90", "purple 4")
  g + geom_text(data = votes_summary, aes (x = faction, y = 0.005, 
                                           label = c(pro_perc[1:(length(pro_perc)-1)], paste0(pro_perc[length(pro_perc)], "%"))),
                inherit.aes = FALSE, col = color_vote, hjust = 0, size = 4.5,
                family = "PF DinText Pro", fontface = "bold")
  } else {

  votes_summary <- voting_df %>%
      group_by(v_name, faction) %>%
      summarize(pro_n = sum(vote_simple == "Голосували за"), pro_perc = round(mean(vote_simple == "Голосували за")*100, 0)) %>%
      arrange(v_name, desc(pro_perc))

  if(is.null(voting_order)){
  faction_order <- votes_summary %>%
      ungroup() %>%
      filter(v_name == levels(as.factor(voting_df$v_name))[length(levels(as.factor(voting_df$v_name)))]) %>% # correct this if you need another basic order
      select(faction) %>% unlist()
  } else {
    faction_order <- votes_summary %>%
      ungroup() %>%
      filter(v_name == voting_order) %>% # correct this if you need another basic order
      select(faction) %>% unlist()
  }
    voting_df$faction <- as.factor(voting_df$faction)
    voting_df$faction <- factor(voting_df$faction,
                                levels = rev(faction_order))
    levels(voting_df$faction)

    g <- ggplot(voting_df, aes(x=faction, fill = vote_simple)) +
      geom_bar(pos = "fill") +
      coord_flip() +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(values = c("Голосували за" = "purple 4", "Не голосували за" = "grey 90"),
                        guide = guide_legend(reverse = TRUE)) +
      facet_wrap(~v_name) +
      theme(text = element_text(family = "PF DinText Pro", face = "bold"),
            axis.title = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12),
            legend.title = element_blank(),
            legend.text = element_text(size = 13),
            legend.position = "bottom",
            plot.title = element_text(size = 14, hjust = 0.5, color = "black", face = "bold")) +
      theme(strip.text.x = element_text(size = 13, color = "black"))

    color_vote <- ifelse(votes_summary$pro_perc > 0, "grey 90", "purple 4")
    percent_string <- votes_summary$pro_perc
    index_perc <- which(votes_summary$faction == faction_order[1])
    percent_string[index_perc] <- paste0(percent_string[index_perc], "%")

    g + geom_text(data = votes_summary, aes (x = faction, y = 0.005,
                                             label = percent_string),
                  inherit.aes = FALSE, col = color_vote, hjust = 0, size = 6.5,
                  family = "PF DinText Pro", fontface = "bold")

  }
}

