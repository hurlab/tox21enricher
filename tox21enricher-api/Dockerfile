### For creating the Docker container for the Tox21Enricher Plumber API. ###

FROM rstudio/plumber
LABEL desc="This is an example Docker container that contains everything needed to run a locally-hosted instance of the Tox21Enricher-Shiny Plumber API."

# Accept arguments to be used as environment variables during build
# Username of user in container that will run the database process
ARG USER_USERNAME
# Password of user in container that will run the database process
ARG USER_PASSWORD

ENV ENV_USER_USERNAME=$USER_USERNAME
ENV ENV_USER_PASSWORD=$USER_PASSWORD

# Create user that will own and run the database & API
RUN useradd -m $ENV_USER_USERNAME -p $ENV_USER_PASSWORD
USER $ENV_USER_USERNAME
ENV HOME=/home/$ENV_USER_USERNAME/

# Create directories for the Tox21Enricher application
    # Base application directory
    RUN mkdir $HOME/tox21enricher/
    # Directory to store user input files
    RUN mkdir $HOME/tox21enricher/Input/
    # Directory to store enrichment results
    RUN mkdir $HOME/tox21enricher/Output/
    # Directory to store archived enrichment results
    RUN mkdir $HOME/tox21enricher/Archive/

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
    "libbz2-dev" \
    "libfontconfig1-dev" \
    "libharfbuzz-dev" \
    "libfribidi-dev" \ 
    "libfreetype6-dev" \
    "libpng-dev" \
    "libtiff5-dev" \
    "libjpeg-dev"

# Install necessary R packages from source
# Set HOME environment variable again (this is required for COPY to work correctly)
ENV HOME=/home/$ENV_USER_USERNAME/
# Copy RScript to install R packages from source from local filesystem to image
COPY ./requirements-src.R $HOME/tox21enricher/requirements-src.R
RUN Rscript $HOME/tox21enricher/requirements-src.R

# Clean up package registry
RUN rm -rf /var/lib/apt/lists/*

USER $ENV_USER_USERNAME
# Copy over needed application files here to speed up build time - won't have to redownload the above packages if you just want to change config or something
    # Set HOME environment variable again (this is required for COPY to work correctly)
    ENV HOME=/home/$ENV_USER_USERNAME/
    # Copy R configuration file from local filesystem to image
    COPY ./config.yml $HOME/tox21enricher/config.yml
    # Copy API .R file from local filesystem to image
    COPY ./plumber.R $HOME/tox21enricher/plumber.R

# Pass R Plumber API as argument
CMD ["~/tox21enricher/plumber.R"]
