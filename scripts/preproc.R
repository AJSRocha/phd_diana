library(dplyr)

ano = 2005

load(paste0(dados, 'vendas-dia/vd_',ano,'.Rdata'))

df = ls(pattern = 'vd_') %>% get;
rm(list = ls(pattern = 'vd_'))

####

df = 
df %>%
  # corrige localização da nazaré
  mutate(zona = case_when(PORTO == 'NAZARE' ~ '27.9.a.c.s',
         T ~ as.character(zona))) %>% 
  # remove os barcos camones misteriosos gays
  filter(IEMBARCA != "PRTNOREGISTR")

# Com base no df, construir uma frota

zona_kg = function(df, spp, barco){
  temp = 
  df %>% 
    filter(EESPECIE == spp & IEMBARCA == barco) %>% 
    group_by(zona) %>% 
    summarise(QVENDA = sum(QVENDA))
  
  res = ifelse(sum(temp$QVENDA) > 0,
               temp$zona[which.max(temp$QVENDA)],
               NA)
  
  return(res)
}

temp =
  df %>%
  filter(IEMBARCA %in% unique(IEMBARCA[EGRUPART == 'PS'])) %>% 
  group_by(IEMBARCA) %>% 
  summarise(nr_artes = length(unique(EGRUPART)),
            # em que zonas andou a molhar o bico?
            N = case_when(sum(QVENDA[zona == '27.9.a.c.n']) > 0 ~ 1, T ~ 0),
            C = case_when(sum(QVENDA[zona == '27.9.a.c.s']) > 0 ~ 1, T ~ 0),
            S = case_when(sum(QVENDA[zona == '27.9.a.s.a']) > 0 ~ 1, T ~ 0),
            # em que zona apanhou mais PIL
            z_pil = zona_kg(df,'PIL', unique(IEMBARCA)),
            # em que zona apanhou mais ANE
            z_ane = zona_kg(df,'ANE', unique(IEMBARCA))
            )
#TODO ficamos na linha 60 do outro script
