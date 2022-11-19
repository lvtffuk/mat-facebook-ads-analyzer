# mat-facebook-ads-analyzer
The visualization of affinity data from [mat-facebook-downloader](https://github.com/lvtffuk/mat-facebook-downloader). It's based on shiny app.

## Development
### Requirements
- R (Rscript)
#### Ubuntu
- libglu1
- libxml2
- libbglpk-dev
- libxt-dev

### Installation & test run
```bash
git clone git@github.com:lvtffuk/mat-facebook-ads-analyzer.git
cd mat-facebook-ads-analyzer
Rscript -e "install.packages(c('shiny', 'plotly', 'shinycssloaders', 'readr', 'shinythemes', 'tidyverse', 'lubridate', 'scales', 'lemon', 'DT', 'extrafont', 'httr', 'padr', 'readr', 'plotly', 'tidytext', 'textrank', 'udpipe', 'lattice', 'igraph', 'ggraph', 'ggplot2', 'data.table'))"
Rscript main.R
```
The app will be accessible on `http://localhost:8080`.

### Dockerfile
Make sure all new packages are installed in `Dockerfile` with `RUN install2.r --error [package]`.

## Settings
No additional configuration is needed.

## Docker
The [image](https://github.com/lvtffuk/mat-facebook-ads-analyzer/pkgs/container/mat-facebook-ads-analyzer) is stored in GitHub packages registry and the app can be run in the docker environment.
```bash
docker pull ghcr.io/lvtffuk/mat-facebook-ads-analyzer:latest
```

```bash
docker run \
-p 8080:8080 \
-d \
--restart=unless-stopped \
--name=mat-facebook-ads-analyzer \
-v '/absolute/path/to/input/dir:/usr/src/app/input' \
ghcr.io/lvtffuk/mat-facebook-ads-analyzer:latest  
```
The app will be accessible on `http://localhost:8080`.

*This work was supported by the European Regional Development Fund-Project “Creativity and Adaptability as Conditions of the Success of Europe in an Interrelated World” (No. CZ.02.1.01/0.0/0.0/16_019/0000734)."*
![Logo](logolink_OP_VVV_hor_bar_eng.jpg?raw=true "Logo")
