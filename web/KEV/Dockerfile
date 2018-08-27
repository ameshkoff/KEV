FROM r-base

MAINTAINER KEV

# system libraries of general use
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
	libxml2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.1 \
	&& rm -rf /var/lib/apt/lists/*

# RUN R -e "Sys.setenv(PKG_CONFIG_PATH='/usr/lib/x86_64-linux-gnu/pkgconfig')"
	
# dependencies
RUN R -e "install.packages(c('shiny', 'rmarkdown', 'plotly', 'readr', 'openxlsx', 'data.table', 'stringr', 'stringi', 'Hmisc', 'rhandsontable'), repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/KEV
COPY KEV /root/KEV

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/KEV')"]