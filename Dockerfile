FROM rstudio/plumber
LABEL Parker Combs <parker.combs@und.edu>

# For creating the Docker container for the Tox21 Enricher Plumber API
# (***) denotes something that needs to be configured!

# Create main user & group
# (***) First, you should replace "username" in these next four lines with the name of the user you want to run the API in the Docker Container. Replace "password" with whatever password you want this user to have.
RUN groupadd -g 1001 username 
RUN useradd -m username -p password -u 1001 -g 1001
USER username
ENV HOME /home/username/

# Create directories for Tox21 Enricher application
# Base application directory
RUN mkdir $HOME/tox21enricher/
# Directory to store user input files
RUN mkdir $HOME/tox21enricher/Input/
# Directory to store enrichment results
RUN mkdir $HOME/tox21enricher/Output/
# R application paths
RUN mkdir $HOME/tox21enricher/src/
RUN mkdir $HOME/tox21enricher/src/main/
RUN mkdir $HOME/tox21enricher/docs/
        
# Update package index and install necessary libraries/dependencies
USER root
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    # Install libpq-dev, which is required to install the RPostgreSQL package
    "libpq-dev" \
    # Install libz-dev, which is required to install the haven package, which is a dependency of the tidyverse package
    "libz-dev" \
    # Install Java and some additional libraries, which are required to install rJava and xlsxjars, which are dependencies for the xlsx package
    "openjdk-11-jdk" \
    "r-base-dev" \
    "libpcre2-dev" \
    "libbz2-dev"

# Install necessary R packages from source
# Set HOME environment variable again (this is required for COPY to work correctly)
# (***) Replace "username" with the name of the user you want to run the API in the Docker Container.
ENV HOME /home/username/
# Copy RScript to install R packages from source from local filesystem to image
COPY ./requirements-src.R $HOME/tox21enricher/requirements-src.R
RUN Rscript $HOME/tox21enricher/requirements-src.R

# Clean up package registry
RUN rm -rf /var/lib/apt/lists/*

# (***) Replace "username" with the name of the user you want to run the API in the Docker Container.
USER username

# Copy over needed application files here to speed up build time - won't have to redownload the above packages if you just want to change config or something
    # Set HOME environment variable again (this is required for COPY to work correctly)
    # (***) Replace "username" with the name of the user you want to run the API in the Docker Container.
    ENV HOME /home/username/
    # Copy R configuration file from local filesystem to image
    COPY ./config.yml $HOME/tox21enricher/config.yml
    # Copy API .R file from local filesystem to image
    COPY ./plumber.R $HOME/tox21enricher/plumber.R
    # Copy HClusterLibrary directory from local filesystem to image and set permissions on executables
    COPY ./HClusterLibrary/ $HOME/tox21enricher/HClusterLibrary/
    # Copy Python script
    COPY ./calcReactiveGroups.py $HOME/tox21enricher/calcReactiveGroups.py

    USER root
    RUN chmod +x $HOME/tox21enricher/HClusterLibrary/clusterLinux64
    RUN chmod +x $HOME/tox21enricher/HClusterLibrary/hclimage-o.jar

    # (***) Replace "username" with the name of the user you want to run the API in the Docker Container.
    USER username
    
    # Copy Tox21 Enricher manual
    #COPY ./docs/Tox21Enricher_Manual_v3.0.pdf $HOME/tox21enricher/docs/Tox21Enricher_Manual_v3.0.pdf

# Pass R Plumber API as argument
# (***) Replace "username" with the name of the user you want to run the API in the Docker Container.
CMD ["/home/username/tox21enricher/plumber.R"]