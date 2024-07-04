# Use the rocker/rstudio base image
FROM rocker/rstudio:latest

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies for R and Stan
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libglu1-mesa-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libboost-all-dev \
    cmake \
    gfortran \
    screen \
    && apt-get clean

# Install RStan and other necessary R packages
RUN R -e "install.packages('rstan', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('devtools', 'shiny', 'rmarkdown', 'knitr', 'tidyverse'), repos='https://cloud.r-project.org/')"

# Expose the port for RStudio Server
EXPOSE 8787

# Add a user for RStudio
RUN useradd -m -d /home/vagrant -G sudo -s /bin/bash vagrant \
    && echo 'vagrant:vagrant' | chpasswd

# Add the project directory
RUN mkdir -p /home/vagrant/project

# Set permissions for /home/vagrant
RUN chown -R vagrant:vagrant /home/vagrant

# Set the default command to launch RStudio Server
CMD ["/init"]

# Add the vagrant user to the sudoers file
RUN echo 'vagrant ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
