# If you decide to docker build, you will require at least 4 GB of memory allocated to docker to compile all the R packages below.
# Get shiny serves plus tidyverse packages image
FROM rocker/shiny-verse:latest

# System libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev
    
# Install R packages required 
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinydashboard', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('lubridate', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('dygraphs', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('reshape2', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('quantmod', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('gridExtra', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('prophet', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('CausalImpact', repos='http://cran.rstudio.com/')"

# Copy shiny app files
COPY app.R /srv/shiny-server/
COPY www /srv/shiny-server/www

# Speed up prophet library by doing intial stan model compilation.
COPY data /usr/local/src/data
COPY prophet-prep.R /usr/local/src/
RUN Rscript /usr/local/src/prophet-prep.R

EXPOSE 3838

# Allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server
RUN sudo chown -R shiny:shiny /usr/local/lib/R/site-library/prophet

# Run app
CMD ["/usr/bin/shiny-server.sh"]
