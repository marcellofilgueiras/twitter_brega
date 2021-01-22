library(rtweet)
library(tidyverse)


brega <- rtweet::search_tweets(q="@gabrielcafona", n=18000,include_rts = FALSE)

brega2 <- rtweet::get_timelines("gabrielcafona", n = 18000)

brega_mais_falado <- brega2%>%
  mutate(text= str_to_lower(brega2$text))

brega_mentions<- brega_mais_falado%>%
  tidyr::drop_na(reply_to_screen_name)%>%
  count(reply_to_screen_name)%>%
  filter(reply_to_screen_name!= "gabrielcafona")%>%
  arrange(desc(n))
  
  
brega_mentions15<- brega_mentions[0:15, ]


brega_mentions15%>%
  ggplot(aes(y=reorder(reply_to_screen_name,n), x= n, fill=n)) + geom_bar(stat= "identity") +
  geom_text( aes(label = n)) +
  labs(
    title= "Quais foram os 15 @'s mais mencionados por Gabriel Cafona?",
       subtitle= "Nos últimos 3235 tweets. ",
       caption = "Feito por @msilveiraaa_")+
  theme_minimal()+ scale_x_continuous(name = "Nº de Menções")+ 
scale_y_discrete(name = "") +
  scale_fill_gradient(name= "", high = "blue", low="lightblue")


table(brega_mais_falado$reply_to_screen_name)


data.table::df(brega_mentions, extensions= "responsive")



#### exemplos

scale_fill_gradient (low = "darkolivegreen1", high = "darkolivegreen"

decisões_merito_prop <- stay_final_merito%>%
  ggplot(aes(x=tipo_parte, fill=decisão)) + geom_bar(position="fill", width = 0.5) +
  labs(x = "Natureza da Parte",y="Porcentagem de Decisões em cada Parte", fill = "Decisão do Juízo",
       title = "Proporção de Provimentos ou Improvimentos em Agravos sobre Prorrogação do Stay Period", subtitle = "Porcentagem por Posição do Agravante em Credor ou Recuperando", caption = "Fonte: TJSP") +
  scale_x_discrete(labels = c("Credor", "Empresa Recuperanda")) +
  scale_fill_discrete(labels= c("Improvido", "Parcialmente Provido", "Provido")) + theme_classic()
decisões_merito_prop


stay_exclusao_credito2 <- stay_sem_homolog%>%
  filter(str_detect(terceiro,"(?i)(trabalhi)"))%>%
  pull("processo")


#naofiz stay_exclusao_essenc<- stay_sem_homolog%>%
#filter(str_detect(segundo,"(?i)(essenci)"))%>%
#pull("processo")

stay_sem_creditos <- filter(stay_sem_homolog,!processo %in%
                              c(stay_exclusao_exec, stay_exclusao_exec2, stay_exclusao_falencia, stay_exclusao_falencia2,stay_exclusao_penhora,stay_exclusao_fiduci, stay_exclusao_arrend, stay_exclusao_habilit, stay_exclusao_credito, stay_exclusao_credito2))
