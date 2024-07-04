#!/bin/bash

echo -n "First we need to make sure your system is up to date." && \
   sleep 1 && echo -n "." && sleep 1 && echo "." && sleep 1 # ...

# Update list of linux packages (-y to skip asking for confirmation)
sudo apt -y update

# Install available upgrades
sudo apt -y upgrade

echo -n "Now we install Docker." && \
   sleep 1 && echo -n "." && sleep 1 && echo "." && sleep 1

# Install the docker builder
sudo apt -y install docker-buildx

echo "Done with setup."
