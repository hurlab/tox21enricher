### For creating the Docker container for the Tox21Enricher Postgres database. ###

FROM ubuntu:20.04
LABEL desc="This is an example Docker container that contains everything needed to run a locally-hosted instance of the Tox21Enricher-Shiny Postgres database."

# Accept arguments to be used as environment variables during build
# Username of user in container that will run the database process
ARG USER_USERNAME
# Password of user in container that will run the database process
ARG USER_PASSWORD
# Username of Postgres user who will have read access to the "tox21enricher" database
ARG POSTGRES_DATABASE_USERNAME
# Password of Postgres user who will have read access to the "tox21enricher" database
ARG POSTGRES_DATABASE_PASSWORD
# Username of Postgres user who will have read/insert/update access to the "tox21queue" database
ARG POSTGRES_QUEUE_USERNAME
# Password of Postgres user who will have read/insert/update access to the "tox21queue" database
ARG POSTGRES_QUEUE_PASSWORD

ENV ENV_USER_USERNAME=$USER_USERNAME
ENV ENV_USER_PASSWORD=$USER_PASSWORD
ENV ENV_POSTGRES_DATABASE_USERNAME=$POSTGRES_DATABASE_USERNAME
ENV ENV_POSTGRES_DATABASE_PASSWORD=$POSTGRES_DATABASE_PASSWORD
ENV ENV_POSTGRES_QUEUE_USERNAME=$POSTGRES_QUEUE_USERNAME
ENV ENV_POSTGRES_QUEUE_PASSWORD=$POSTGRES_QUEUE_PASSWORD

# Create user that will own and run the databases
RUN useradd -m $ENV_USER_USERNAME -p $ENV_USER_PASSWORD
USER $ENV_USER_USERNAME
ENV HOME=/home/$ENV_USER_USERNAME/

# Create directories for the Tox21Enricher application
    # Base application directory
    RUN mkdir $HOME/tox21enricher/
# Create directories for the database
    # Miniconda directory
    RUN mkdir $HOME/miniconda/
    # Main database directory
    RUN mkdir $HOME/tox21enricher/db/

# Update package index and install necessary libraries/dependencies
USER root
RUN rm -rf /var/lib/apt/lists/* && rm -rf /etc/apt/sources.list.d && apt-get update && apt-get install -y "wget" "curl"

# Install and configure Miniconda
RUN wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O $HOME/miniconda/miniconda.sh
RUN chmod +x $HOME/miniconda/miniconda.sh
RUN $HOME/miniconda/miniconda.sh -b -u -p $HOME/miniconda
RUN rm -rf $HOME/miniconda/miniconda.sh
RUN $HOME/miniconda/bin/conda init bash

# Clean up package registry
RUN rm -rf /var/lib/apt/lists/*

USER $ENV_USER_USERNAME
# Copy over needed application files here to speed up build time - won't have to redownload the above packages if you just want to change config or something
    # Set HOME environment variable again (this is required for COPY to work correctly)
    ENV HOME=/home/$ENV_USER_USERNAME/
    # Copy Tox21Enricher database files
    COPY ./db/tox21enricher_db.sql $HOME/tox21enricher/tox21enricher_db.sql
    COPY ./db/tox21enricher_queue.sql $HOME/tox21enricher/tox21enricher_queue.sql

# Use Conda to install Postgres
USER root
ENV HOME=/home/$ENV_USER_USERNAME/
RUN $HOME/miniconda/bin/conda update -n base -c defaults conda --repodata-fn=repodata.json && $HOME/miniconda/bin/conda create -c conda-forge -n my-rdkit-env rdkit
RUN $HOME/miniconda/bin/activate my-rdkit-env && $HOME/miniconda/bin/conda install requests && $HOME/miniconda/bin/conda install -c rdkit rdkit-postgresql
USER $ENV_USER_USERNAME
# Initialize db directory
RUN $HOME/miniconda/bin/initdb -D $HOME/tox21enricher/db/
# Start Postgres server, create necessary databases and users, and set privileges
RUN $HOME/miniconda/bin/pg_ctl -D $HOME/tox21enricher/db/ -l $HOME/tox21enricher/db/logfile start && \
    $HOME/miniconda/bin/psql -c "CREATE ROLE $ENV_POSTGRES_DATABASE_USERNAME WITH LOGIN PASSWORD '$ENV_POSTGRES_DATABASE_PASSWORD';" postgres && \
    $HOME/miniconda/bin/psql -c "CREATE ROLE $ENV_POSTGRES_QUEUE_USERNAME WITH LOGIN PASSWORD '$ENV_POSTGRES_QUEUE_PASSWORD';" postgres && \
    $HOME/miniconda/bin/createdb tox21enricher && $HOME/miniconda/bin/psql -U $ENV_USER_USERNAME -d tox21enricher -a -f $HOME/tox21enricher/tox21enricher_db.sql && \
    $HOME/miniconda/bin/createdb tox21queue && $HOME/miniconda/bin/psql -U $ENV_USER_USERNAME -d tox21queue -a -f $HOME/tox21enricher/tox21enricher_queue.sql && \
    $HOME/miniconda/bin/psql -U $ENV_USER_USERNAME tox21enricher -c "REVOKE ALL ON ALL TABLES IN SCHEMA public FROM $ENV_POSTGRES_DATABASE_USERNAME;" && \
    $HOME/miniconda/bin/psql -U $ENV_USER_USERNAME tox21queue -c "REVOKE ALL ON ALL TABLES IN SCHEMA public FROM $ENV_POSTGRES_DATABASE_USERNAME;" && \
    $HOME/miniconda/bin/psql -U $ENV_USER_USERNAME tox21enricher -c "REVOKE ALL ON ALL TABLES IN SCHEMA public FROM $ENV_POSTGRES_QUEUE_USERNAME;" && \
    $HOME/miniconda/bin/psql -U $ENV_USER_USERNAME tox21queue -c "REVOKE ALL ON ALL TABLES IN SCHEMA public FROM $ENV_POSTGRES_QUEUE_USERNAME;" && \
    $HOME/miniconda/bin/psql -U $ENV_USER_USERNAME tox21enricher -c "GRANT SELECT ON ALL TABLES IN SCHEMA public TO $ENV_POSTGRES_DATABASE_USERNAME;" && \
    $HOME/miniconda/bin/psql -U $ENV_USER_USERNAME tox21queue -c "GRANT SELECT, INSERT, UPDATE ON ALL TABLES IN SCHEMA public TO $ENV_POSTGRES_QUEUE_USERNAME;" && \
    $HOME/miniconda/bin/pg_ctl -D $HOME/tox21enricher/db/ stop

# Copy and set up necessary config files
USER root
COPY ./pg_hba.conf $HOME/tox21enricher/db/pg_hba.conf
RUN chown $ENV_USER_USERNAME $HOME/tox21enricher/db/pg_hba.conf 
RUN echo "\nlisten_addresses = '*'" >> $HOME/tox21enricher/db/postgresql.conf
RUN cat $HOME/tox21enricher/db/postgresql.conf
USER $ENV_USER_USERNAME

# Copy & set exec permissions on entrypoint.sh file
COPY ./entrypoint.sh /entrypoint.sh
USER root
ENV HOME=/home/$ENV_USER_USERNAME/
RUN chmod +x /entrypoint.sh
USER $ENV_USER_USERNAME

# Pass database launch as argument
ENTRYPOINT ["/entrypoint.sh"]
CMD ["/bin/bash"]
