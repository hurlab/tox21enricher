### For creating the Docker container for the Tox21Enricher Shiny application. ###
FROM rocker/shiny
LABEL desc="This is an example Docker container that contains everything needed to run a locally-hosted instance of the Tox21Enricher-Shiny Shiny application."

USER root
ENV HOME /srv/shiny-server/

# Create directories for the Tox21Enricher application
    # Base application directory
    RUN mkdir $HOME/tox21enricher/

# Update package index and install necessary libraries/dependencies
USER root
RUN apt-get update && apt-get install -y --no-install-recommends \
    "libxml2-dev" \
    "libcurl4-openssl-dev" \
    "libssl-dev"

# Install necessary R packages from source
# Set HOME environment variable again (this is required for COPY to work correctly)
ENV HOME /srv/shiny-server/
# Copy RScript to install R packages from source from local filesystem to image
COPY ./requirements-src.R $HOME/tox21enricher/requirements-src.R
RUN Rscript $HOME/tox21enricher/requirements-src.R

# Clean up package registry
RUN rm -rf /var/lib/apt/lists/*

# Copy over needed application files here to speed up build time - won't have to redownload the above packages if you just want to change config or something
    # Set HOME environment variable again (this is required for COPY to work correctly)
    ENV HOME /srv/shiny-server/
    # Copy R configuration file from local filesystem to image
    COPY ./config.yml $HOME/tox21enricher/config.yml
    # Copy Shiny .R files from local filesystem to image
    COPY ./global.R $HOME/tox21enricher/global.R
    COPY ./server.R $HOME/tox21enricher/server.R
    COPY ./ui.R $HOME/tox21enricher/ui.R
    # Copy /www/ directory from local filesystem to image
    COPY ./www/ $HOME/tox21enricher/www/

# Enable logging
ENV SHINY_LOG_STDERR=1

USER shiny
# Pass R Plumber API as argument
CMD ["/usr/bin/shiny-server"]
