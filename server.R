# Define server logic required to draw a histogram ----
library(tidyverse)
library(lubridate)
library(scales)
library(lemon)
library(DT)
library(extrafont)
extrafont::loadfonts()
library(httr)
library(padr)
library(readr)
library(plotly)
library(tidytext)
library(textrank)
library(udpipe)
library(lattice)

library(igraph)
library(ggraph)
library(ggplot2)
library(data.table)

path <- "czech2" 

GroupedMedian <- function(frequencies, intervals, sep = NULL, trim = NULL) {
  # If "sep" is specified, the function will try to create the 
  #   required "intervals" matrix. "trim" removes any unwanted 
  #   characters before attempting to convert the ranges to numeric.
  if (!is.null(sep)) {
    if (is.null(trim)) pattern <- ""
    else if (trim == "cut") pattern <- "\\[|\\]|\\(|\\)"
    else pattern <- trim
    intervals <- sapply(strsplit(gsub(pattern, "", intervals), sep), as.numeric)
  }

  Midpoints <- rowMeans(intervals)
  cf <- cumsum(frequencies)
  Midrow <- findInterval(max(cf)/2, cf) + 1
  L <- intervals[1, Midrow]      # lower class boundary of median class
  h <- diff(intervals[, Midrow]) # size of median class
  f <- frequencies[Midrow]       # frequency of median class
  cf2 <- cf[Midrow - 1]          # cumulative frequency class before median class
  n_2 <- max(cf)/2               # total observations divided by 2

  unname(L + (n_2 - cf2)/f * h)
}




df_regiony_tmp <- fread(paste0(path, "/df_region.csv", sep=""))
df_demographics_tmp <- fread(paste0(path, "/df_demographics_unnested.csv", sep=""))
df_ads_tmp <- fread(paste0(path, "/df_imp.csv", sep=""))
df_ads_corpus <- fread(paste0(path, "/ads_corpus.csv", sep=""))
df_locals <- fread(paste0(path, "/total_region.csv", sep=""))

locals <- as.vector(df_locals$region)

df_ads_tmp$V1 <- NULL
df_ads_tmp$ad_snapshot_url <- NULL
    
server <- function(input, output) {

# generate specific pages
  dataInput <- reactive({

    date_observation_start <- input$dateRange[1] #start date
    date_observation_end <- input$dateRange[2] #eleciton date

    specific_page <- input$specific_page

    funds_by <- input$specific_funding_entity
    funds_by <- filter(df_ads_tmp, funding_entity %in% funds_by)
    funds_by <- unique(funds_by$page_name)


    specific_page <- c(specific_page, funds_by)

#    df_ads <- df_ads_tmp %>%
#      mutate_at(vars(contains("time")), lubridate::ymd_hms) %>%
#      filter(ad_creation_time %within% interval(date_observation_start,
#                                                date_observation_end))


    df_ads <- df_ads_tmp %>%
      mutate_at(vars(contains("time")), as.Date) %>%
      filter(ad_creation_time %within% interval(date_observation_start,
                                                date_observation_end))
   

    if(input$search!="") {
          df_ads <- df_ads %>%
            filter(str_detect(ad_creative_body, input$search))
        }
    if(length(specific_page)!=0) {
        df_ads <- df_ads %>%
          filter(page_name %in% specific_page)
    }

    df_ads <- df_ads %>%
      filter(spend_lower_bound >= (input$range[1]))

    df_ads <- df_ads %>%
      filter(spend_upper_bound < (input$range[2]))
      
    total_ads_per_page_tmp <- df_ads %>%
      group_by(page_name) %>% 
      summarise(n_ads=n()) %>% arrange(desc(n_ads))
      
    total_ads_per_page_tmp <- filter(total_ads_per_page_tmp, n_ads >= input$minAds[1])

    filterPage <- total_ads_per_page_tmp$page_name

    if(input$selectGender!="nonactive") {
      id_ads <- df_demographics_tmp  %>%
         filter(gender==input$selectGender & percentage > input$rangeGender)
      id_ads <- as.vector(unique(id_ads$ad_id))
      df_ads <- df_ads %>%
            filter(ad_id %in% id_ads)
    }

    df_ads <- df_ads %>%
      filter(page_name %in% filterPage)

    

})


  output$overview <- DT::renderDataTable({

    total_ads_per_page <- dataInput() %>%
      group_by(page_name) %>% 
      summarise(n_ads=n()) %>% arrange(desc(n_ads))

    DT::datatable(total_ads_per_page, options = list(orderClasses = TRUE), caption = 'Selected pages')

  })

  output$overviewSpend <- DT::renderDataTable({

      df_ads <- dataInput()

   df_spend_total <- df_ads %>%
              group_by(page_name) %>%
              summarise(spend_min_total=sum(spend_lower_bound, na.rm = T),
                        spend_mid_total=sum(spend_mid, na.rm=T),
                        spend_max_total=sum(spend_upper_bound, na.rm=T))
    DT::datatable(df_spend_total, options = list(orderClasses = TRUE), caption = 'Selected pages')

  })

output$overviewAll <- DT::renderDataTable({


    total_ads_per_page <- df_ads_tmp %>%
      group_by(page_name) %>%
      summarise(n_ads=n())  %>% arrange(desc(n_ads))

    DT::datatable(total_ads_per_page, options = list(orderClasses = TRUE), caption = 'All pages.')
    
  })

output$overviewAllFunding <- DT::renderDataTable({


    total_ads_per_page <- df_ads_tmp %>%
      group_by(funding_entity) %>%
      summarise(n_ads=n())  %>% arrange(desc(n_ads))

    DT::datatable(total_ads_per_page, options = list(orderClasses = TRUE), caption = 'All funding entities')
    
  })

  output$agePlot <- renderPlot({
 
      df_ads <- dataInput()

      df_demographics <- df_demographics_tmp %>%
        filter(ad_id %in% df_ads$ad_id)

      df_regiony <- df_regiony_tmp %>%
        filter(ad_id %in% df_ads$ad_id)

      df_age <- df_demographics %>% 
        group_by(page_name, age, impressions_mid, ad_id) %>% 
        summarise(sum_percentage=sum(percentage, na.rm = T)) 

      df_age_stats <- df_age %>% 
        group_by(page_name, age) %>% 
        summarise(median_ad=median(sum_percentage, na.rm = T),
                sd_ad=sd(sum_percentage, na.rm = T))

      p <- df_age %>% 
        ggplot()+
        geom_boxplot(aes(x=age, 
                         y=sum_percentage,
                         #weight=impressions_mid,
                         color=page_name
                         ),
                     fill="transparent")+
        geom_text(data=df_age_stats,
                   aes(x=age,
                       label=paste0(round(median_ad*100, 1),
                                   "\n(",
                                   round(sd_ad*100, 1),
                                   ")")),
                  label.padding=0,
                  lineheight=0.8,
                  size=4,
                  y=1.3)+
        labs(title="Distribution of ads'audience over age groups",
             subtitle="Each ad has an audience of 100 % over all age groups. Each dot represents the age group's share of the total 100 %. Top: Median and standard deviation (in brackets).",
             y="Share of age group in ad's overall audience",
             x="Age group",
             caption="")+
        facet_rep_wrap(page_name~.,
                       ncol=2,
                      # labeller=as_labeller(function(x) paste("age group:", x)),
                       repeat.tick.labels = T)+
        scale_y_continuous(labels=scales::percent,
                           breaks=seq(0,1, .25),
                           expand = expand_scale(mult=c(0, 0.5)))+
        theme(panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
           #   panel.spacing.y = unit(0, "cm"),
              axis.text.x = element_text(size = 10),
              legend.position="none",
              strip.text = element_text(colour = "black"),
              panel.grid.minor.x = element_blank())

      p


    })  

output$agePlotMidpoint <- renderPlot({
 
      df_ads <- dataInput()

      df_demographics <- df_demographics_tmp %>%
        filter(ad_id %in% df_ads$ad_id)

      x <- data.frame(df_demographics$ad_id, df_demographics$age, df_demographics$gender, df_demographics$impressions_mid, df_demographics$percentage, df_demographics$page_name)
      colnames(x) <- c("ad_id", "age", "gender", "impressions_mid", "percentage", "page_name")
      x <- filter(x, impressions_mid!="NA")
      x <- filter(x, page_name!="NA")
      x <- filter(x, age!="NA")

      x$impressions_mid <- as.vector(x$impressions_mid)
      x$percentage <- as.vector(x$percentage)
      x$age <- as.vector(x$age)
      index <- x$age == "65+"
      x$age[index] <- "65-75"
      x$per_age_mid <- x$percentage * x$impressions_mid

      pages <- unique(x$page_name)   

demo <- data.frame()
for(page in pages) {

    tmp <- filter(x, page_name==page)
    age.agg <- aggregate(tmp$per_age_mid, by=list(Category=tmp$age), FUN=sum)

    MedianAge <- GroupedMedian(intervals = age.agg$Category, frequencies = age.agg$x, sep = "-")

    gender.agg <- aggregate(tmp$per_age_mid, by=list(Category=tmp$gender), FUN=sum)
    gender.agg <- filter(gender.agg, Category %in% c("male", "female"))
    gender.agg.sum <- sum(gender.agg$x)
    gender.agg$percentage <- gender.agg$x/gender.agg.sum

    GenderMale <- filter(gender.agg, Category=="male")
    GenderMale <- as.numeric(GenderMale$percentage[1])
    demo.tmp.df <- data.frame(page, MedianAge, GenderMale)
    demo <- rbind(demo, demo.tmp.df)

}

plot(demo$GenderMale, demo$MedianAge, type="n", xlab = "Female - Male proportion", ylab = "Median age", xlim=c(0,1))
text(demo$GenderMale, demo$MedianAge, demo$page)

    })  


output$heatmaps <- renderPlot({

      df_ads <- dataInput()

      df_demographics <- df_demographics_tmp %>%
        filter(ad_id %in% df_ads$ad_id)

      df_regiony <- df_regiony_tmp %>%
        filter(ad_id %in% df_ads$ad_id)

      df_demographics_all_ads <- df_demographics %>% 
        group_by(page_name, gender, age) %>% 
        filter(!is.na(impressions_mid)) %>% 
        summarise(mean_percentage=mean(x=percentage, 
                                         w=impressions_mid,
                                         na.rm = T)) %>% 
        ungroup()

      check <- df_demographics_all_ads %>% 
        group_by(page_name) %>% 
        summarise(sum_mean_percentage=sum(mean_percentage))

      #plot
      df_demographics_all_ads %>% 
        filter(gender %in% c("male", "female")) %>% 
        ggplot()+
        geom_tile(aes(x=gender, y=age,
                      fill=mean_percentage))+
        geom_text(aes(x=gender, y=age,
                      label=scales::percent(mean_percentage)),
                  size=4)+
        scale_color_manual(values=c("1"="orange", "0"="grey"))+
        labs(title="Average audience share by age and gender",
             caption="")+
        theme(legend.position="none",
              panel.grid = element_blank())+
        lemon::facet_rep_wrap(vars(page_name),
                              repeat.tick.labels = T,
                              ncol=3)

    })

    output$totalSpend <- renderPlot({
          df_ads <- dataInput()


          # pocet reklam per page
          total_ads_per_page <- df_ads %>%
              group_by(page_name) %>%
              summarise(n_ads=n())

          # pocet reklam per funding
          total_ads_per_funding <- df_ads %>%
              group_by(funding_entity) %>%
              summarise(n_ads=n())

          df_spend_total_funding <- df_ads %>%
              group_by(page_name) %>%
              summarise(spend_min_total=sum(spend_lower_bound, na.rm = T),
                        spend_mid_total=sum(spend_mid, na.rm=T),
                        spend_max_total=sum(spend_upper_bound, na.rm=T))


          max_spend <- max(df_spend_total_funding$spend_max_total)
            
         df_spend_total_funding %>%   
            ggplot()+
            geom_errorbar(aes(x=reorder(page_name, spend_mid_total), 
                             xend=reorder(page_name, spend_mid_total),
                          ymin=spend_min_total, ymax=spend_max_total,
                         color=page_name))+
            geom_point(aes(x=reorder(page_name, spend_mid_total),
                           y=spend_mid_total,
                           color=page_name))+
            geom_text(aes(x=reorder(page_name, spend_mid_total),
                          y=max(df_spend_total_funding$spend_max_total)+50000,
                          label=paste(dollar(spend_min_total, prefix=" "))),
                      size=4,
                      hjust=1,
                      color="black")+
              geom_text(aes(x=reorder(page_name, spend_mid_total),
                          y=max(df_spend_total_funding$spend_max_total)+100000,
                          label=paste(dollar(spend_mid_total, prefix=" "))),
                      size=4,
                      hjust=1,
                      color="black")+
              geom_text(aes(x=reorder(page_name, spend_mid_total),
                          y=max(df_spend_total_funding$spend_max_total)+150000,
                          label=paste(dollar(spend_max_total, prefix=" "))),
                      size=4,
                      hjust=1,
                      color="black")+
            
            geom_text(data=data.frame(y = c(max_spend+50000,
                                         max_spend+100000,
                                         max_spend+150000),
                                   label = c("min", "mid", "max")),
                 x=length(unique(df_spend_total_funding$page_name))+1,
                 aes(y=y,
                     label=label),
                 size=4,
                 hjust=1,
                 vjust=1,
                 color="black")+
            
            labs(title="Total spending on FB ads",
                 subtitle="Instead of exact amounts, Facebook's API only provides lower and upper bounds of each ad's price.",
                 caption="")+
            theme(axis.title = element_blank(),
                  panel.grid.major.y = element_blank(),
                  legend.position="none")+
            scale_y_continuous(labels=scales::dollar_format(prefix=""), 
                               breaks=seq(0, 200000, 50000),
                               minor_breaks = seq(25000, 175000, 50000),
                               expand=expand_scale(mult=c(0, 0.02)))+
            scale_x_discrete(expand=expand_scale(add=c(0,1.1)))+
            coord_flip()



    })

    output$genderDifference <- renderPlot({

            df_ads <- dataInput()

            df_demographics <- df_demographics_tmp %>%
              filter(ad_id %in% df_ads$ad_id)

            df_regiony <- df_regiony_tmp %>%
              filter(ad_id %in% df_ads$ad_id)

            df_demographics_all_ads <- df_demographics %>% 
              group_by(page_name, gender, age) %>% 
              filter(!is.na(impressions_mid)) %>% 
              summarise(mean_percentage=mean(x=percentage, 
                                               w=impressions_mid,
                                               na.rm = T)) %>% 
              ungroup()

            check <- df_demographics_all_ads %>% 
              group_by(page_name) %>% 
              summarise(sum_mean_percentage=sum(mean_percentage))



            df_female_male <- df_demographics_all_ads %>% 
              pivot_wider(., names_from = "gender", values_from = "mean_percentage") %>% 
              mutate(diff_female_male=male-female) 

            df_female_male %>% 
              ggplot()+
              geom_bar(aes(x=age, y=diff_female_male,
                           fill=ifelse(diff_female_male>0.000, "female surplus", "male surplus"),
                           color=ifelse(diff_female_male>0.000, "female surplus", "male surplus")),
                       stat="identity",
                       key_glyph= "point")+
              labs(y="difference between female and male mean audience share (percentage points)",
                   title="Difference between female and male mean audience share",
                   subtitle=stringr::str_wrap("Almost all candidates feature a larger mean male audience share across all age segments. Difference of means weighted by impression count (mid-point) of each ad.", 75))+
              scale_y_continuous(limits=c(-0.12,0.12), 
                                 labels=function(x) scales::percent(abs(x), accuracy = 1))+
              facet_wrap(vars(page_name), nrow=3)+
              coord_flip()+
              theme(panel.grid.major.y = element_blank(),
                    panel.grid.minor = element_blank(),
                    legend.position = "bottom",
                    legend.justification = "right",
                    legend.title=element_blank(),
                    axis.text.y = element_text(size=9))+
              scale_fill_manual(values=c("female surplus"="steelblue",
                                         "male surplus"="orange"))+
              scale_color_manual(values=c("female surplus"="steelblue",
                                         "male surplus"="orange"))+
              guides(fill = guide_legend(override.aes = list(size = 5), reverse=T),
                     color=guide_legend(reverse=T))  


      })  

      output$regions <- renderPlot({

              df_ads <- dataInput()

              df_demographics <- df_demographics_tmp %>%
                  filter(ad_id %in% df_ads$ad_id)

              df_regiony <- df_regiony_tmp %>%
                  filter(ad_id %in% df_ads$ad_id)

              df_region <- df_regiony %>% 
                  mutate(percentage=as.numeric(percentage)) %>% 
                  mutate(region=as_factor(region))

              df_region <- df_region %>% 
                mutate(region=forcats::fct_infreq(region, ordered=T)) %>% 
                complete(ad_id, nesting(region), fill=list(percentage=0)) %>% 
                group_by(ad_id) %>% 
                arrange(page_name) %>% 
                fill(page_name, .direction = c("down"))  %>% 
                filter(region %in% locals) %>% 
                ungroup()

              #median and weighted mean + indicator for coloring text
              df_mean_median <- df_region %>% 
                left_join(., 
                          df_demographics %>% select(ad_id, impressions_mid), 
                          by=c("ad_id")) %>% 
                group_by(page_name, region) %>% 
                summarise(ad_mean=weighted.mean(percentage, 
                                                w=impressions_mid,
                                                na.rm=T),
                          ad_median=median(percentage, na.rm = T)) %>% 
                mutate_at(vars(contains("ad")), round, 2)


              df_region %>% 
                ggplot()+
                geom_boxplot(aes(x=page_name,
                                 y=percentage,
                                 color=page_name),
                             fill="transparent")+
                geom_text(data=df_mean_median,
                          aes(x=page_name,
                              y=1.2,
                              label=scales::percent(ad_median, suffix = "%", accuracy = 1)
                              ),
                          check_overlap=T,size=3,
                          hjust=1)+
                geom_text(data=df_mean_median,
                          aes(x=page_name,
                              y=1.35,
                              label=scales::percent(ad_mean, suffix= "%", accuracy = 1)
                              ),
                          check_overlap=T,
                          hjust=1,
                          size=3)+
                geom_text(y=c(1.2),
                          x=length(unique(df_region$page_name))+1,
                          label=c("median"),
                          angle=45,
                          hjust=0.5,
                          check_overlap=T,
                          size=3)+
                lemon::facet_rep_wrap(vars(region),
                                      repeat.tick.labels = T,
                                      ncol=2)+
                  geom_text(y=c(1.35),
                          x=length(unique(df_region$page_name))+1,
                          label=c("w. mean"),
                          angle=45,
                          hjust=0.5,
                          check_overlap=T,
                          size=3)+
                coord_flip()+
                labs(title="Regional distribution of ads' average audiences",
                     subtitle="",
                     y="Regional share of an ad's audience",
                     caption="")+
                theme(panel.grid.minor.x = element_blank(),
                      panel.grid.major.y = element_blank(),
                      axis.text.y=element_text(size = 10),
                      legend.position="none",
                      panel.spacing = unit(0, "cm"),
                      axis.title.y = element_blank(),
                      strip.text = element_text(colour = "black"),
                      panel.grid.minor.y = element_blank())+
                scale_y_continuous(labels=scales::percent,
                                   breaks=seq(0, 1, .25),
                                   expand = expand_scale(mult=c(0, 0.1)))+
                scale_x_discrete(expand=expand_scale(mult=c(0.01, 0.15)))



      })  

      output$perCategory <- renderPlot({


              date_observation_start <- input$dateRange[1] #start date
              date_observation_end <- input$dateRange[2] #eleciton date
              
              df_ads <- dataInput()

              df_spend_total_funding <- df_ads %>%
                  group_by(page_name) %>%
                  summarise(spend_min_total=sum(spend_lower_bound, na.rm = T),
                            spend_mid_total=sum(spend_mid, na.rm=T),
                            spend_max_total=sum(spend_upper_bound, na.rm=T))

              # spend v kategoriich
              ads_per_category <- df_ads %>%
                unite(spend_interval, c("spend_lower_bound", "spend_upper_bound"), sep="-") %>%
                group_by(page_name, spend_interval, .drop=F) %>%
                summarise(n_ads=n()) %>%
                arrange(page_name)

              ads_per_category %>% 
                  ggplot()+
                  geom_bar(aes(x=spend_interval,
                               y=n_ads,
                               fill=page_name),
                           color="transparent",
                           stat="identity")+
                  geom_text(data=. %>% filter(n_ads>0),
                            aes(x=spend_interval,
                                 y=n_ads,
                                 label=n_ads),
                            nudge_x=0.05,
                             hjust=0)+
                  labs(title="Number of Facebook ads per spending category",
                       subtitle=paste0("Note different scales on x axis; ", "Period from ",
                                      format(date(date_observation_start), "%a, %d %b %Y"), " to ",
                                      format(date(date_observation_end), "%a, %d %b %Y")),
                       x="Number of ads",
                       y="Spending category",
                       caption="Roland Schmidt | @zoowalk | http://werk.statt.codes")+
                  scale_y_continuous(expand=expand_scale(mult=c(0, 0.2)),
                                     breaks=waiver(),
                                     minor_breaks = NULL)+
                  
                  theme(axis.title=element_blank(),
                        panel.grid.minor.y = element_blank(),
                        panel.grid.major.y = element_blank(),
                        legend.position = "none")+
                  facet_wrap(vars(page_name),
                                 scales = "free_x",
                                 ncol=3)+
                  coord_flip()



        })  

    output$udpipeCooc <- renderPlot({


        df_ads <- dataInput()
        df_ads <- df_ads[!duplicated(df_ads$ad_creative_body), ]
        texts <- as.vector(df_ads$ad_id)

        x <- filter(df_ads_corpus, doc_id %in% texts)

        cooc <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
                             term = "lemma", 
                             group = c("doc_id", "paragraph_id", "sentence_id"))

        library(igraph)
        library(ggraph)
        library(ggplot2)
        wordnetwork <- head(cooc, 100)
        wordnetwork <- graph_from_data_frame(wordnetwork)
        ggraph(wordnetwork, layout = "fr") +
          geom_edge_link(aes(width = cooc, edge_alpha = cooc/2), edge_colour = "pink") +
          geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
          theme_graph(base_family = "Arial Narrow") +
          theme(legend.position = "none") +
          labs(title = "Cooccurrences within sentence")

      })  

    output$udpipeFollow <- renderPlot({


        df_ads <- dataInput()
        df_ads <- df_ads[!duplicated(df_ads$ad_creative_body), ]
        texts <- as.vector(df_ads$ad_id)

            x <- filter(df_ads_corpus, doc_id %in% texts)
        
            cooc <- cooccurrence(x$lemma, relevant = x$upos %in% c("NOUN", "ADJ", "PROPN"), skipgram = 1)
            wordnetwork <- head(cooc, 50)
            wordnetwork <- graph_from_data_frame(wordnetwork)
            ggraph(wordnetwork, layout = "fr") +
                geom_edge_link(aes(width = cooc/2, edge_alpha = cooc, edge_colour = "pink")) +
                geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
                theme_graph(base_family = "Arial Narrow") +
                labs(title = "Words following one another", subtitle = "Nouns & Adjective & Proper name")

      })  

    output$udpipeNouns <- renderPlot({


        df_ads <- dataInput()
        df_ads <- df_ads[!duplicated(df_ads$ad_creative_body), ]
        texts <- as.vector(df_ads$ad_id)

        x <- filter(df_ads_corpus, doc_id %in% texts)
        stats <- subset(x, upos %in% c("NOUN")) 
        stats <- txt_freq(stats$lemma)
        stats$key <- factor(stats$key, levels = rev(stats$key))
        barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
                 main = "Most occurring nouns", xlab = "Freq")

      }) 


    output$udpipeNounsTable <- DT::renderDataTable({


        df_ads <- dataInput()
        df_ads <- df_ads[!duplicated(df_ads$ad_creative_body), ]
        texts <- as.vector(df_ads$ad_id)

        x <- filter(df_ads_corpus, doc_id %in% texts)
        stats <- subset(x, upos %in% c("SYM")) 
        stats <- txt_freq(stats$lemma)
        stats$key <- factor(stats$key, levels = rev(stats$key))
        head(stats, 20)
      }) 

    output$udpipeAdj <- renderPlot({


        df_ads <- dataInput()
        df_ads <- df_ads[!duplicated(df_ads$ad_creative_body), ]
        texts <- as.vector(df_ads$ad_id)

        x <- filter(df_ads_corpus, doc_id %in% texts)
        stats <- subset(x, upos %in% c("ADJ")) 
        stats <- txt_freq(stats$lemma)
        stats$key <- factor(stats$key, levels = rev(stats$key))
        barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
                 main = "Most occurring nouns", xlab = "Freq")

      }) 


    output$udpipeProp <- renderPlot({


        df_ads <- dataInput()
        df_ads <- df_ads[!duplicated(df_ads$ad_creative_body), ]
        texts <- as.vector(df_ads$ad_id)

        x <- filter(df_ads_corpus, doc_id %in% texts)
        stats <- subset(x, upos %in% c("PROPN")) 
        stats <- txt_freq(stats$lemma)
        stats$key <- factor(stats$key, levels = rev(stats$key))
        barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
                 main = "Most occurring proper names", xlab = "Freq")

      }) 


    output$udpipeThree <- renderPlot({


        df_ads <- dataInput()
        df_ads <- df_ads[!duplicated(df_ads$ad_creative_body), ]
        texts <- as.vector(df_ads$ad_id)

        x <- filter(df_ads_corpus, doc_id %in% texts)
     
        stats <- cooccurrence(x = x$lemma, 
                     relevant = x$upos %in% c("NOUN", "ADJ", "PROP"), skipgram = 2)

        wordnetwork <- head(stats, 50)
        wordnetwork <- graph_from_data_frame(wordnetwork)
        ggraph(wordnetwork, layout = "fr") +
          geom_edge_link(aes(width = cooc/2, edge_alpha = cooc), edge_colour = "pink") +
          geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
          theme_graph(base_family = "Arial Narrow") +
          theme(legend.position = "none") +
          labs(title = "Cooccurrences within 3 words distance", subtitle = "Nouns, Adjective and Proper name")

      }) 

 output$rake <- renderPlot({


        df_ads <- dataInput()
        df_ads <- df_ads[!duplicated(df_ads$ad_creative_body), ]

        texts <- as.vector(df_ads$ad_id)

        x <- filter(df_ads_corpus, doc_id %in% texts)

        stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                               relevant = x$upos %in% c("NOUN", "ADJ", "PROPN"))
        stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
        barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "cadetblue", 
                 main = "Keywords identified by RAKE (nouns, adjs, propns)", 
                 xlab = "Rake")
      })  

 output$phrases <- renderPlot({


        df_ads <- dataInput()
        df_ads <- df_ads[!duplicated(df_ads$ad_creative_body), ]

        texts <- as.vector(df_ads$ad_id)

        x <- filter(df_ads_corpus, doc_id %in% texts)

       x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Keywords - simple noun phrases", xlab = "Frequency")

      })  




 output$pmi <- renderPlot({


        df_ads <- dataInput()
        df_ads <- df_ads[!duplicated(df_ads$ad_creative_body), ]

        texts <- as.vector(df_ads$ad_id)

        x <- filter(df_ads_corpus, doc_id %in% texts)

        x$word <- tolower(x$lemma)
        stats <- keywords_collocation(x = x, term = "word", group = "doc_id")
        stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
        barchart(key ~ pmi, data = head(subset(stats, freq > 3), 20), col = "cadetblue", 
                 main = "Keywords identified by PMI Collocation", 
                 xlab = "PMI (Pointwise Mutual Information)")
      })  


  output$tfidf <- renderPlot({

                df_ads <- dataInput()

                doc_id_to_page <- data.frame(df_ads$ad_id, df_ads$page_name)
                colnames(doc_id_to_page) <- c("doc_id","page_name")

                        texts <- as.vector(df_ads$ad_id)
                        x <- filter(df_ads_corpus, doc_id %in% texts)

                ads <- data.frame(x$doc_id, x$lemma)
                colnames(ads) <- c("doc_id", "text")

                ads <- merge(ads,doc_id_to_page)
                ads <- data.frame(ads$text, ads$page_name)
                colnames(ads) <- c("text", "book")
                ads$text <- as.vector(ads$text)
                ads$page_name <- as.vector(ads$page_name)

                ads_words <- ads %>%
                  unnest_tokens(word, text) %>%
                  count(book, word, sort = TRUE)

                total_words <- ads_words %>% 
                  group_by(book) %>% 
                  summarize(total = sum(n))

                ads_words <- left_join(ads_words, total_words)

                ads_words <- ads_words %>%
                  bind_tf_idf(word, book, n)

                ### vizualizace
                ads_words %>%
                  arrange(desc(tf_idf)) %>%
                  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
                  group_by(book) %>% 
                  top_n(20) %>% 
                  ungroup() %>%
                  ggplot(aes(word, tf_idf, fill = book)) +
                  geom_col(show.legend = FALSE) +
                  labs(x = NULL, y = "tf-idf") +
                  facet_wrap(~book, ncol = 2, scales = "free") +
                  coord_flip()

        })

  output$bitfidf <- renderPlot({

                df_ads <- dataInput()

                doc_id_to_page <- data.frame(df_ads$ad_id, df_ads$page_name)
                colnames(doc_id_to_page) <- c("doc_id","page_name")

                texts <- as.vector(df_ads$ad_id)
                x <- filter(df_ads_corpus, doc_id %in% texts)

                ads <- data.frame(x$doc_id, x$token)
                colnames(ads) <- c("doc_id", "text")

                ads <- merge(ads,doc_id_to_page)
                ads <- data.frame(ads$text, ads$page_name)
                colnames(ads) <- c("text", "book")
                ads$text <- as.vector(ads$text)
                ads$page_name <- as.vector(ads$page_name)

                austen_bigrams <- ads %>%
                  unnest_tokens(bigram, text, token = "ngrams", n = 2)

                bigrams_separated <- austen_bigrams %>%
                  separate(bigram, c("word1", "word2"), sep = " ")

                bigrams_filtered <- bigrams_separated 
                # %>%
                #   filter(!word1 %in% stop_words$word) %>%
                #   filter(!word2 %in% stop_words$word)

                # new bigram counts:
                bigram_counts <- bigrams_filtered %>% 
                  count(word1, word2, sort = TRUE)

                bigrams_united <- bigrams_filtered %>%
                  unite(bigram, word1, word2, sep = " ")

                bigram_tf_idf <- bigrams_united %>%
                  count(book, bigram) %>%
                  bind_tf_idf(bigram, book, n) %>%
                  arrange(desc(tf_idf))

                bigram_tf_idf %>%
                  arrange(desc(tf_idf)) %>%
                  mutate(word = factor(bigram, levels = rev(unique(bigram)))) %>% 
                  group_by(book) %>% 
                  top_n(15) %>% 
                  ungroup() %>%
                  ggplot(aes(word, tf_idf, fill = book)) +
                  geom_col(show.legend = FALSE) +
                  labs(x = NULL, y = "tf-idf bigram") +
                  facet_wrap(~book, ncol = 2, scales = "free") +
                  coord_flip()

        })


      output$dailySpend <- renderPlot({

        df_ads <- dataInput()

         date_observation_start <- input$dateRange[1] #start date
              date_observation_end <- input$dateRange[2] #eleciton date
              
              

    ads_per_day <- df_ads %>% 
      mutate(ad_created_day=lubridate::date(ad_creation_time)) %>% 
      group_by(page_name, ad_created_day) %>% 
      summarise(n_ads_per_day=n())

    ads_time_line <- df_ads %>% 
      mutate(ad_created_day=lubridate::date(ad_creation_time)) %>% 
      select(-spend_interval) %>% 
      group_by(page_name, ad_created_day) %>% 
      summarise_at(vars(contains("spend")), .funs=list(sum=~sum(., na.rm=T))) %>% 
      ungroup()
 
ads_time_line %>% 
    ggplot()+
    geom_bar(aes(x=ad_created_day,
                 y=spend_mid_sum,
                 fill=page_name),
             color="transparent",
             stat="identity",
             show.legend=F) +
    geom_errorbar(aes(x=ad_created_day,
                      ymin=spend_lower_bound_sum,
                      ymax=spend_upper_bound_sum),
                  color="grey",
                  show.legend=F)+
    labs(title="Amount spent on newly created ads per day",
         x="day of creation of ad",
         y="Amount in thousands",
      #   caption=paste(my_caption),
         subtitle=paste("Period from", format(date_observation_start, "%a, %d %b %y"),
                        "to",
                        format(date_observation_end, "%a, %d %b %y"))) +
    
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.title = element_blank(),
          legend.justification = "right",
          legend.position = "bottom",
          panel.spacing = unit(0,"cm"))+
    scale_x_date(breaks=scales::date_breaks(width="1 week"),
                 labels=scales::date_format("%b %d"))+
    scale_y_continuous(minor_breaks = NULL,
                       labels = scales::comma_format(scale=0.001))+
    facet_rep_wrap(vars(page_name),
               ncol=2,
               repeat.tick.labels = c("all"))



      })  

      output$adstext <- DT::renderDataTable({

        df_ads <- dataInput()
        df_ads <- select(df_ads, c("page_name", "ad_creative_body"))
        DT::datatable(df_ads, options = list(orderClasses = TRUE))
      })

}