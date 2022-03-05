# Build and run "ppn viewer" shiny app with all suggested features
#
# Build "bleeding edge" version of shiny app:
#   docker build -t trevorld/ppn-viewer .
#
# To run Shiny app (via port 3000) (also open http://localhost:3000 in browser):
#   docker run --rm -p 3000:3838 trevorld/ppn-viewer
#
# To publish to hub.docker.com:
#   docker push trevorld/ppn-viewer

FROM rocker/shiny

RUN apt-get update && apt-get install -y  \
    cargo \
    fonts-dejavu \
    libfontconfig1-dev \
    libfreetype6-dev \
    libglu1-mesa-dev \
    libgl1-mesa-dev \
    libicu-dev \
    libjpeg-dev \
    libpng-dev \
    libxml2-dev \
    rustc \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e 'install.packages(c("fansi", "gifski", "readobj", "remotes", "rgl", "shiny", "systemfonts", "tweenr"))'
RUN Rscript -e 'remotes::install_github("piecepackr/piecepackr")'
RUN Rscript -e 'remotes::install_github("piecepackr/ppgames")'
RUN Rscript -e 'remotes::install_github("piecepackr/piecenikr")'
RUN Rscript -e 'remotes::install_github("piecepackr/tradgames")'

# copy the app directory into the image
RUN rm -r /srv/shiny-server/*
RUN echo "dir <- system.file(\"shiny/ppn_viewer\", package = \"ppgames\")" > /srv/shiny-server/app.R
RUN echo "shiny::shinyAppDir(dir)" >> /srv/shiny-server/app.R

# run app
CMD ["/usr/bin/shiny-server"]
