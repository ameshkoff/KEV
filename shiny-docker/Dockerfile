FROM nuest/mro:3.5.0

MAINTAINER KEV

RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    wget \
	gfortran \
	zip \
	unzip \
	xtail \
	libssl-dev \
	&& rm -rf /var/lib/apt/lists/*

# Download and install shiny server
RUN wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    . /etc/environment && \
    R -e "install.packages(c('shiny', 'rmarkdown', 'openxlsx', 'data.table', 'stringr', 'rhandsontable', 'Hmisc', 'dplyr', 'httr', 'plotly', 'RcppRoll', 'nloptr'), repos='https://mran.microsoft.com/snapshot/2018-08-01')" && \
    cp -R /opt/microsoft/ropen/3.5.0/lib64/R/library/shiny/examples/* /srv/shiny-server/ \
	&& rm -rf /var/lib/apt/lists/*

EXPOSE 3838

COPY shiny-server.sh /usr/bin/shiny-server.sh

RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]
CMD ["/usr/bin/shiny-server.sh"]