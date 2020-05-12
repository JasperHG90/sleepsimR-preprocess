# From R-base
FROM rocker/tidyverse

# Install R packages
RUN R -e "install.packages('logger',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('edfReader',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('assertthat',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('stringr',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('reticulate',dependencies=TRUE, repos='http://cran.rstudio.com/')"
# Install python and numpy
RUN apt-get -y install apt-utils
RUN apt-get -y update 
RUN apt-get -y upgrade
RUN apt-get -y install python3 python3-pip
RUN pip3 install numpy

# Copy
COPY prepare_data.R prepare_data.R

# Set entrypoint
ENTRYPOINT ["Rscript", "prepare_data.R"]
