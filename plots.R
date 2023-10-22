library(ggplot2)
library(heatmaply)
library(RColorBrewer)
library(knitr)
library(tidyverse, warn.conflict=F)
library(scales)
library(ggh4x)

library(cowplot)


cors <- function(df){
  M <- Hmisc::rcorr(as.matrix(df), type = 'spearman')
  Mdf <- map(M, ~data.frame(.x))
  return(Mdf)
}

formatted_cors <- function(df){
    cors(df) %>%
      map(~rownames_to_column(.x, var='measure1')) %>%
      map(~pivot_longer(.x, -measure1, 'measure2')) %>% 
      bind_rows(.id = 'id') %>%
      pivot_wider(names_from = id, values_from = value,) %>%
      mutate(sig_p = ifelse(P < .05, T, F), p_if_sig = ifelse(P <.05, P, NA), r_if_sig = ifelse(P <.05, r, NA)) %>%
    mutate(measure1=gsub('\\.', ' ', measure1), measure2=gsub('\\.', ' ', measure2))
}

df <- read.delim2('data/full_leiden_wikipedia_spec_indicators.tsv', dec = '.')

agg_values <- read.delim2('results/agg_values.tsv')

biblio_ind_names <- c('P', 'Dimensions WoS', 'P collab', 'P industry collab',
                      'P int collab', 'TCS', 'TNCS', 'P top 10%', 'Topic specialisation bio',
                      'Topic specialisation eng', 'Pa f mf')
biblio_ind_names_org <- c('impact_P', 'dimensions_wos', 'collab_P', 'P_industry_collab',
                          'P_int_collab', 'TCS', 'TNCS', 'P_top10', 'topic_specialisation_bio',
                          'topic_specialisation_eng', 'PA_F_MF')

wiki_metrics_names <- c('Characters', 'Words', 'Sections', 'Edits',
                        'Editors', 'Total views', 'International views',
                        'Local views', 'Language links', 'References',
                        'Unique references')
wiki_metrics_names_org <- c('characters', 'words', 'sections', 'revisions',
                            'editors', 'total_views', 'int_views',
                            'local_views', 'langlinks', 'references',
                            'unique_references')

uni_features_name <- 'Age'
uni_features_name_org <- 'age'

df <- df[,c(uni_features_name_org, biblio_ind_names_org, wiki_metrics_names_org)]
map <- setNames(c(uni_features_name, biblio_ind_names, wiki_metrics_names), c(uni_features_name_org, biblio_ind_names_org, wiki_metrics_names_org))

names(df) <- map[names(df)]

display.brewer.pal(n = 8, name = 'RdBu')

df_cor <- formatted_cors(df)
df_cor$measure1 <- factor(df_cor$measure1, levels = biblio_ind_names, ordered = TRUE)
df_cor$measure2 <- factor(df_cor$measure2, levels = rev(wiki_metrics_names), ordered = TRUE)

df_cor %>% 
  filter(measure1 %in% biblio_ind_names & measure2 %in% wiki_metrics_names) %>%
  ggplot(aes(measure1, measure2, fill=r, label=round(r_if_sig,2))) +
  geom_tile() + 
  labs(x = NULL, y = NULL, fill = "Spearman's\nCorrelation", title='', subtitle="*Only significant Spearman's correlation coefficients shown") + 
  #scale_fill_gradient2(mid='#FBFEF9',low='#0C6291',high='#A63446', limits=c(-1,1)) +
  scale_fill_stepsn(colors = brewer.pal(n = 10, name = 'RdBu')[10:1],
                    breaks=seq(-1, 1, by=0.2),
                    limits=c(-1, 1)) +
  guides(fill=guide_colorbar(barwidth = 20, title.vjust = 1)) +
  geom_text(aes(color=ifelse(r>=0.7,'white','black'))) +
  scale_color_manual(values=c('white'='white',
                              'black'='black'),
                     guide=FALSE)+
  labs(x='Bibliometric indicators', y='Wikipedia metrics')+
  theme_classic() +
  scale_x_discrete(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title = element_text(size=16))


df_cor <- formatted_cors(df)
df_cor$measure1 <- factor(df_cor$measure1, levels = biblio_ind_names, ordered = TRUE)
df_cor$measure2 <- factor(df_cor$measure2, levels = uni_features_name, ordered = TRUE)

df_cor %>% 
  filter(measure1 %in% biblio_ind_names & measure2 %in% uni_features_name) %>%
  ggplot(aes(measure1, measure2, fill=r, label=round(r_if_sig,2))) +
  geom_tile() + 
  labs(x = NULL, y = NULL, fill = "Spearman's\nCorrelation", title='', subtitle="*Only significant Spearman's correlation coefficients shown") + 
  #scale_fill_gradient2(mid='#FBFEF9',low='#0C6291',high='#A63446', limits=c(-1,1)) +
  scale_fill_stepsn(colors = brewer.pal(n = 10, name = 'RdBu')[10:1],
                    breaks=seq(-1, 1, by=0.2),
                    limits=c(-1, 1)) +
  guides(fill=guide_colorbar(barwidth = 20, title.vjust = 1)) +
  geom_text(aes(color=ifelse(r>=0.7,'white','black'))) +
  scale_color_manual(values=c('white'='white',
                              'black'='black'),
                     guide=FALSE)+
  labs(x='Bibliometric indicators', y='Wikipedia metrics')+
  theme_classic() +
  scale_x_discrete(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title = element_text(size=16))


df_cor <- formatted_cors(df)
df_cor$measure1 <- factor(df_cor$measure1, levels = uni_features_name, ordered = TRUE)
df_cor$measure2 <- factor(df_cor$measure2, levels = rev(wiki_metrics_names), ordered = TRUE)

df_cor %>% 
  filter(measure1 %in% uni_features_name & measure2 %in% wiki_metrics_names) %>%
  ggplot(aes(measure1, measure2, fill=r, label=round(r_if_sig,2))) +
  geom_tile() + 
  labs(x = NULL, y = NULL, fill = "Spearman's\nCorrelation", title='', subtitle="*Only significant Spearman's correlation coefficients shown") + 
  #scale_fill_gradient2(mid='#FBFEF9',low='#0C6291',high='#A63446', limits=c(-1,1)) +
  scale_fill_stepsn(colors = brewer.pal(n = 10, name = 'RdBu')[10:1],
                    breaks=seq(-1, 1, by=0.2),
                    limits=c(-1, 1)) +
  guides(fill=guide_colorbar(barwidth = 20, title.vjust = 1)) +
  geom_text(aes(color=ifelse(r>=0.7,'white','black'))) +
  scale_color_manual(values=c('white'='white',
                              'black'='black'),
                     guide=FALSE)+
  labs(x='Bibliometric indicators', y='Wikipedia metrics')+
  theme_classic() +
  scale_x_discrete(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title = element_text(size=16))


df_cor <- formatted_cors(df)
df_cor$measure1 <- factor(df_cor$measure1, levels = wiki_metrics_names, ordered = TRUE)
df_cor$measure2 <- factor(df_cor$measure2, levels = rev(wiki_metrics_names), ordered = TRUE)

df_cor %>% 
  filter(measure1 %in% wiki_metrics_names & measure2 %in% wiki_metrics_names) %>%
  ggplot(aes(measure1, measure2, fill=r, label=round(r_if_sig,2))) +
  geom_tile() + 
  labs(x = NULL, y = NULL, fill = "Spearman's\nCorrelation", subtitle="Only significant Spearman's correlation coefficients shown") + 
  #scale_fill_gradient2(mid='#FBFEF9',low='#0C6291',high='#A63446', limits=c(-1,1)) +
  scale_fill_stepsn(colors = brewer.pal(n = 10, name = 'RdBu')[10:1][6:10],
                    breaks=seq(0, 1, by=0.1),
                    limits=c(0, 1)) +
  guides(fill=guide_colorbar(barwidth = 20, title.vjust = 1)) +
  geom_text(aes(color=ifelse(r>=0.7,'white','black')), size=3) +
  scale_color_manual(values=c('white'='white',
                              'black'='black'),
                     guide=FALSE)+
  labs(x='Wikipedia metrics', y='Wikipedia metrics')+
  theme_classic() +
  scale_x_discrete(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title = element_text(size=14))



agg_values <- read.delim2('results/agg_values.tsv')
agg_values <- agg_values[which(agg_values$Universities>=10),]

agg_values_c <- agg_values
  
#agg_values <- heatmaply::normalize(agg_values)
agg_values <- heatmaply::percentize(agg_values)

row.names(agg_values) <- agg_values[,1]
agg_values <- agg_values[,-1]

agg_values <- as.matrix(agg_values)

agg_values <- as.table(agg_values)
agg_values <- as.data.frame(agg_values, stringsAsFactors = FALSE)

agg_values$DB <- 'Wikipedia'

df <- read.delim2('data/full_leiden_wikipedia_spec_indicators.tsv', encoding = 'UTF-8')

top_countries <- as.data.frame(table(df$Country), stringsAsFactors = FALSE)
top_countries <- top_countries[order(top_countries$Freq, decreasing = TRUE)[1:10],]

df <- read.delim2('data/local_international_views.tsv', dec='.', encoding = 'UTF-8')

df_countries <- data.frame()

for (country in c('zh', 'de', 'ja', 'ko', 'it', 'es', 'hi', 'fa')) {
  df_aux <- df[which(df$lang == country),]
  names(df_aux)[which(names(df_aux)=='total')] <- 'local_views'
  df_aux <- inner_join(df_aux, df[which(df$id %in% df_aux$id & df$lang == 'en'), c('id', 'total')], by='id')
  names(df_aux)[which(names(df_aux)=='total')] <- 'int_views'
  
  df_aux$type = 'Rest'
  df_aux$type[which(df_aux$lang == df_aux$Language.code)] = 'Local'
  
  df_countries <<- rbind.data.frame(df_countries,
                                    df_aux[, c('lang', 'university', 'int_views', 'local_views', 'type')])
}

ivy <- c('Harvard Univ', 'Yale Univ', 'Univ Penn', 'Princeton Univ',
         'Columbia Univ', 'Brown Univ', 'Cornell Univ', 'Dartmouth Coll')
df_countries$type[df_countries$university %in% ivy] <- 'Ivy League'


unique(df_countries$lang)
df_countries$lang[which(df_countries$lang=='es')] <- '**Spanish Wikipedia**<br />es.wikipedia.org'
df_countries$lang[which(df_countries$lang=='de')] <- '**German Wikipedia**<br />de.wikipedia.org'
df_countries$lang[which(df_countries$lang=='fa')] <- '**Persian Wikipedia**<br />fa.wikipedia.org'
df_countries$lang[which(df_countries$lang=='hi')] <- '**Hindi Wikipedia**<br />hi.wikipedia.org'
df_countries$lang[which(df_countries$lang=='it')] <- '**Italian Wikipedia**<br />it.wikipedia.org'
df_countries$lang[which(df_countries$lang=='ja')] <- '**Japanese Wikipedia**<br />ja.wikipedia.org'
df_countries$lang[which(df_countries$lang=='ko')] <- '**Korean Wikipedia**<br />ko.wikipedia.org'
df_countries$lang[which(df_countries$lang=='zh')] <- '**Chinese Wikipedia**<br />zh.wikipedia.org'

ggplot() +
  geom_point(data=df_countries[which(df_countries$type!='Local' & df_countries$type!='Ivy League'),], aes(x=int_views+1, y=local_views+1, color=type), alpha=0.8) +
  geom_point(data=df_countries[which(df_countries$type=='Local' | df_countries$type=='Ivy League'),], aes(x=int_views+1, y=local_views+1, color=type), alpha=0.8) +
  scale_x_log10(labels = trans_format('log10', math_format(10^.x))) +
  scale_y_log10(labels = trans_format('log10', math_format(10^.x))) +
  scale_color_manual(values=c('Rest'='#4494c7','Local'='#e81b2f', 'Ivy League'='#00553d'),
                     labels=c('Ivy League', 'Located in a country\nwhere the language\nis official', 'Located in a country\nwith a foreign language'),
                     guide=guide_legend(override.aes = list(size=4))) +
  labs(x='International views', y='Language edition views', color='Universities') +
  annotation_logticks() +
  theme_bw()+
  facet_wrap(.~lang, scales = 'free', nrow = 2) +
  theme(legend.position = 'top',
        legend.text = element_text(size=11),
        legend.title = element_text(face='bold'),
        axis.title = element_text(size=16, face='bold'),
        strip.text = ggtext::element_markdown(size = 11))

