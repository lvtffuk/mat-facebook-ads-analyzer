# mat-facebook-ads-analyzer
The visualization of affinity data from [mat-facebook-downloader](https://github.com/zabkwak/mat-facebook-downloader). It's based on shiny app.

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
git clone git@github.com:zabkwak/mat-facebook-ads-analyzer.git
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
The [image](https://github.com/zabkwak/mat-facebook-ads-analyzer/pkgs/container/mat-facebook-ads-analyzer) is stored in GitHub packages registry and the app can be run in the docker environment.
```bash
docker pull ghcr.io/zabkwak/mat-facebook-ads-analyzer:latest
```

```bash
docker run \
-p 8080:8080 \
-d \
--restart=unless-stopped \
--name=mat-facebook-ads-analyzer \
-v '/absolute/path/to/czech2/dir:/usr/src/app/czech2' \
ghcr.io/zabkwak/mat-facebook-ads-analyzer:latest  
```
The app will be accessible on `http://localhost:8080`.
