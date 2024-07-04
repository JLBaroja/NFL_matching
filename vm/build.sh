#!/bin/bash

echo -n "Now we will build the Docker image.  This will take a while." && \
   sleep 1 && echo -n "." && sleep 1 && echo "." && sleep 1

# Build the image (-t to give it a nice name)
sudo docker build -t rstudio-stan .

echo "Docker image built."
