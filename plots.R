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

cor(agg_values$gini_production, agg_values$words, method = 'spearman')

formatted_cors <- function(df){
    cors(df) %>%
      map(~rownames_to_column(.x, var="measure1")) %>%
      map(~pivot_longer(.x, -measure1, "measure2")) %>% 
      bind_rows(.id = "id") %>%
      pivot_wider(names_from = id, values_from = value) %>%
      mutate(sig_p = ifelse(P < .05, T, F), p_if_sig = ifelse(P <.05, P, NA), r_if_sig = ifelse(P <.05, r, NA))
  }


df <- read.delim2("data/full_leiden_wikipedia_multirank_indicators.tsv")
df$dimensions_wos <- 100-df$dimensions_wos
agg_values <- read.delim2("results/agg_values.tsv")
biblio_ind <- c(names(agg_values)[c(2:15,29:32)],'dimensions_wos')
wiki_metrics <- names(agg_values)[c(16:28, 34)]
df <- df[,c(biblio_ind, wiki_metrics)]


display.brewer.pal(n = 8, name = 'RdBu')

formatted_cors(df) %>% 
  filter(measure1 %in% biblio_ind & measure2 %in% wiki_metrics) %>%
  ggplot(aes(measure1, measure2, fill=r, label=round(r_if_sig,2))) +
  geom_tile() + 
  labs(x = NULL, y = NULL, fill = "Spearman's\nCorrelation", title='', subtitle="*Only significant Spearman's correlation coefficients shown") + 
  #scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1)) +
  scale_fill_stepsn(colors = brewer.pal(n = 10, name = 'RdBu')[10:1],
                    breaks=seq(-1, 1, by=0.1),
                    limits=c(-1, 1)) +
  guides(fill=guide_colorbar(barwidth = 35, title.vjust = 1)) +
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

formatted_cors(df) %>% 
  filter(measure1 %in% biblio_ind & measure2 %in% biblio_ind) %>%
  ggplot(aes(measure1, measure2, fill=r, label=round(r_if_sig,2))) +
  geom_tile() + 
  labs(x = NULL, y = NULL, fill = "Spearman's\nCorrelation", title='(a) Bibliometric indicators', subtitle="Only significant Spearman's correlation coefficients shown") + 
  #scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1)) +
  scale_fill_stepsn(colors = brewer.pal(n = 10, name = 'RdBu')[10:1],
                    breaks=seq(-1, 1, by=0.1),
                    limits=c(-1, 1)) +
  guides(fill=guide_colorbar(barwidth = 25, title.vjust = 1)) +
  geom_text(aes(color=ifelse(r>=0.7,'white','black')), size=3) +
  scale_color_manual(values=c('white'='white',
                              'black'='black'),
                     guide=FALSE)+
  labs(x='Bibliometric indicators', y='Bibliometric indicators')+
  theme_classic() +
  scale_x_discrete(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title = element_text(size=14))


formatted_cors(df) %>% 
  filter(measure1 %in% wiki_metrics & measure2 %in% wiki_metrics) %>%
  ggplot(aes(measure1, measure2, fill=r, label=round(r_if_sig,2))) +
  geom_tile() + 
  labs(x = NULL, y = NULL, fill = "Spearman's\nCorrelation", title='', subtitle="Only significant Spearman's correlation coefficients shown") + 
  #scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1)) +
  scale_fill_stepsn(colors = brewer.pal(n = 10, name = 'RdBu')[10:1],
                    breaks=seq(-1, 1, by=0.1),
                    limits=c(-1, 1)) +
  guides(fill=guide_colorbar(barwidth = 35, title.vjust = 1)) +
  geom_text(aes(color=ifelse(r>=0.7,'white','black'))) +
  scale_color_manual(values=c('white'='white',
                              'black'='black'),
                     guide=FALSE)+
  labs(x='Wikipedia metrics', y='Wikipedia metrics')+
  theme_classic() +
  scale_x_discrete(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title = element_text(size=16))


# https://www.royfrancis.com/a-guide-to-elegant-tiled-heatmaps-in-r-2019/
agg_values <- read.delim2("results/agg_values.tsv")
agg_values <- agg_values[which(agg_values$Universities>=10),]

agg_values_c <- agg_values
  
#agg_values <- heatmaply::normalize(agg_values)
agg_values[,-35] <- heatmaply::percentize(agg_values[,-35])

row.names(agg_values) <- agg_values[,1]
agg_values <- agg_values[,-1]

agg_values <- as.matrix(agg_values)

agg_values <- as.table(agg_values)
agg_values <- as.data.frame(agg_values, stringsAsFactors = FALSE)

agg_values$DB <- 'Wikipedia'
agg_values$DB[c(1:294, 568:651)] <- 'Bibliometrics'
agg_values$DB[which(agg_values$Var2 %in% c('Universities', 'established_year'))] <- 'Univ.'


agg_values$Var1 <- factor(agg_values$Var1, levels = agg_values_c$Country_x[order(agg_values_c$Universities, decreasing = FALSE)], ordered = TRUE)
agg_values$DB <- factor(agg_values$DB, levels = c('Univ.', 'Bibliometrics', 'Wikipedia'), ordered = TRUE)


# V1
ggplot(agg_values, aes(x=Var2, y=Var1, fill=Freq))+
  geom_tile(colour="white", size=0.2)+
  scale_x_discrete(expand=c(0, 0))+
  scale_y_discrete(expand=c(0, 0))+
  scale_fill_gradientn(colours = c('white', '#e32229'), na.value = NA, breaks=seq(0,1,0.2))+ 
  #scale_fill_gradientn(colours = brewer.pal(7, "YlGnBu"), na.value = NA)+            
  theme_grey(base_size=10)+
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.title = element_text(size = 11, vjust = 1),
        legend.title.align = 1,
        legend.margin=margin(grid::unit(0, "cm")),
        legend.key.height=grid::unit(0.7, "cm"),
        legend.key.width=grid::unit(2, "cm"),
        axis.text.x = element_text(size = 11, angle = 90, vjust = 0.5, hjust=1, color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7, 0.4, 0.1, 0.2, "cm"),
        strip.background = element_rect(fill = 'black'),
        strip.text = element_text(color = 'white', size = 16)
  )+
  labs(x='', y='', fill='Percentile of average\nmetric value') +
  facet_grid(.~DB, scales = 'free') +
  force_panelsizes(cols = c(0.2, 1, 1))





p1 <- ggplot(agg_values[which(!(agg_values$Var2 %in% c('Universities', 'established_year'))),], aes(x=Var2, y=Var1, fill=Freq))+
  geom_tile(colour="white", size=0.2)+
  scale_x_discrete(expand=c(0, 0))+
  scale_y_discrete(expand=c(0, 0))+
  scale_fill_gradientn(colours = c('white', '#e32229'), na.value = NA, breaks=seq(0,1,0.2))+ 
  #scale_fill_gradientn(colours = brewer.pal(7, "YlGnBu"), na.value = NA)+            
  theme_grey(base_size=10)+
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.title = element_text(size = 11, vjust = 1),
        legend.title.align = 1,
        legend.margin=margin(grid::unit(0, "cm")),
        legend.key.height=grid::unit(0.7, "cm"),
        legend.key.width=grid::unit(2, "cm"),
        axis.text.x = element_text(size = 11, angle = 90, vjust = 0.5, hjust=1, color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7, 0, 0.1, 0.2, "cm"),
        strip.background = element_rect(fill = 'black'),
        strip.text = element_text(color = 'white', size = 16)
  )+
  labs(x='', y='', fill='Percentile of average\nmetric value') +
  facet_grid(.~DB, scales = 'free') +
  force_panelsizes(cols = c(1/14, 1/18))

library(ggpubr)
library(patchwork)

p2 <- ggplot(agg_values[which(agg_values$Var2 == 'Universities'),], aes(x=Var1, y=Freq))+
  geom_col(fill='grey30')+
  coord_flip()+
  scale_x_discrete(expand=c(0, 0))+
  scale_y_continuous(expand=c(0, 0))+
  #scale_fill_gradientn(colours = brewer.pal(7, "YlGnBu"), na.value = NA)+            
  theme_grey(base_size=10)+
  theme(panel.background = element_rect(fill = 'white'),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.title = element_text(size = 11, vjust = 1),
        legend.title.align = 1,
        legend.margin=margin(grid::unit(0, "cm")),
        legend.key.height=grid::unit(0.7, "cm"),
        legend.key.width=grid::unit(2, "cm"),
        axis.text.x = element_text(size = 11, angle = 90, vjust = 0.5, hjust=1, color = 'black'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7, 0.4, 0.1, 0, "cm"),
        strip.background = element_rect(fill = 'black'),
        strip.text = element_text(color = 'white', size = 16)
  )+
  labs(x='', y='', fill='Percentile of average\nmetric value') +
  facet_grid(.~DB, scales = 'free')

p1 + p2 + plot_layout(ncol = 2, nrow = 1, widths = c(9, 1), heights = c(1, 7))



  scale_fill_manual(values=rev(brewer.pal(7, "YlGnBu")), na.value="grey90")            

textcol <- "grey40"

# further modified ggplot
ggplot(m4, aes(x=year, y=state, fill=countfactor))+
  geom_tile(colour="white", size=0.2)+
  guides(fill=guide_legend(title="Cases per\n100,000 people"))+
  labs(x="", y="", title="Incidence of Measles in the US")+
  scale_y_discrete(expand=c(0, 0))+
  scale_x_discrete(expand=c(0, 0), breaks=c("1930", "1940", "1950", "1960", "1970", "1980", "1990", "2000"))+
  scale_fill_manual(values=c("#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#e6f598", "#abdda4", "#ddf1da"), na.value = "grey90")+
  #coord_fixed()+
  theme_grey(base_size=10)+
  theme(legend.position="right", legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0, "cm")),
        legend.text=element_text(colour=textcol, size=7, face="bold"),
        legend.key.height=grid::unit(0.8, "cm"),
        legend.key.width=grid::unit(0.2, "cm"),
        axis.text.x=element_text(size=10, colour=textcol),
        axis.text.y=element_text(vjust=0.2, colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7, 0.4, 0.1, 0.2, "cm"),
        plot.title=element_text(colour=textcol, hjust=0, size=14, face="bold")
  )


df <- read.delim2('data/full_leiden_wikipedia_multirank_indicators.tsv', encoding = 'UTF-8')

top_countries <- as.data.frame(table(df$Country_x), stringsAsFactors = FALSE)
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

ggplot() +
  geom_point(data=df_countries[which(df_countries$type!='Local'),], aes(x=int_views+1, y=local_views+1, color=type), alpha=0.8) +
  geom_point(data=df_countries[which(df_countries$type=='Local'),], aes(x=int_views+1, y=local_views+1, color=type), alpha=0.8) +
  scale_x_log10(labels = trans_format('log10', math_format(10^.x))) +
  scale_y_log10(labels = trans_format('log10', math_format(10^.x))) +
  scale_color_manual(values=c('Rest'='#4494c7','Local'='#e81b2f')) +
  labs(x='International views', y='Local views', color='Universities') +
  #annotation_logticks() +
  theme_bw()+
  facet_wrap(.~lang, scales = 'free', nrow = 2) +
  theme(legend.position = 'bottom',
        axis.title = element_text(face='bold'))


ivy <- c('Harvard Univ', 'Yale Univ', 'Univ Penn', 'Princeton Univ',
         'Columbia Univ', 'Brown Univ', 'Cornell Univ', 'Dartmouth Coll')
df_countries$type[df_countries$university %in% ivy] <- 'Ivy League'

ggplot() +
  geom_point(data=df_countries[which(df_countries$type!='Local' & df_countries$type!='Ivy League'),], aes(x=int_views+1, y=local_views+1, color=type), alpha=0.8) +
  geom_point(data=df_countries[which(df_countries$type=='Local' | df_countries$type=='Ivy League'),], aes(x=int_views+1, y=local_views+1, color=type), alpha=0.8) +
  scale_x_log10(labels = trans_format('log10', math_format(10^.x))) +
  scale_y_log10(labels = trans_format('log10', math_format(10^.x))) +
  scale_color_manual(values=c('Rest'='#4494c7','Local'='#e81b2f', 'Ivy League'='#00553d'),
                     guide=guide_legend(override.aes = list(size=4))) +
  labs(x='International views', y='Local views', color='Universities') +
  annotation_logticks() +
  theme_bw()+
  facet_wrap(.~lang, scales = 'free', nrow = 2) +
  theme(legend.position = 'top',
        axis.title = element_text(face='bold'))

