#!/bin/bash

# Start the container (-p to forward a port
#                      -v to map a directory)
sudo docker run -d \
    -p 8789:8787 \
    -v $(pwd)/..:/home/vagrant/project \
    --name rstudio-stan rstudio-stan

echo "Docker container started."
echo "Access RStudio by navigating to http://localhost:8789/ in your web browser."
echo "The username:password is vagrant:vagrant"
