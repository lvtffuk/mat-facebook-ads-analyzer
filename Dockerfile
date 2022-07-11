FROM rocker/r-ver:latest

ARG NPM_GITHUB_READ
ENV NPM_GITHUB_READ=$NPM_GITHUB_READ
WORKDIR /usr/src/app
COPY . .

RUN apt-get update && apt-get -y install libglu1 libxml2 libglpk-dev libxt-dev

RUN install2.r --error shiny
RUN install2.r --error plotly
RUN install2.r --error shinycssloaders
RUN install2.r --error readr
RUN install2.r --error shinythemes    
RUN install2.r --error tidyverse      
RUN install2.r --error lubridate      
RUN install2.r --error scales
RUN install2.r --error lemon
RUN install2.r --error DT
RUN install2.r --error extrafont      
RUN install2.r --error httr
RUN install2.r --error padr
RUN install2.r --error readr
RUN install2.r --error plotly
RUN install2.r --error tidytext       
RUN install2.r --error textrank
RUN install2.r --error udpipe
RUN install2.r --error lattice
RUN install2.r --error igraph
RUN install2.r --error ggraph
RUN install2.r --error ggplot2
RUN install2.r --error data.table

ENV PORT=8080
LABEL org.opencontainers.image.source https://github.com/lvtffuk/mat-facebook-ads-analyzer
EXPOSE 8080

CMD [ "Rscript", "main.R" ]
