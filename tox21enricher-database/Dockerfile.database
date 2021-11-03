FROM mcs07/postgres-rdkit
LABEL Parker Combs <parker.combs@und.edu>

# For creating the Docker container for the Tox21 Enricher PostgreSQL database
# (***) denotes something that needs to be configured!

# Create user who will own PostgreSQL process and switch to it
# (***) First, you should replace "username" in these next two lines with the name of the user you want to own and run the database in the Docker Container. Replace "password" with whatever password you want this user to have.
RUN useradd -m username -p password
USER username

# Make directory for database and initialize
RUN mkdir $HOME/tox21enricher-database/ && initdb $HOME/tox21enricher-database/

# Copy database backup from local machine
# (***) Replace "username" with the name of the user you want to own and run the database in the Docker Container.
ENV HOME /home/username/
COPY ./tox21enricher-database.sql $HOME/tox21enricher-database/tox21enricher-database.sql

# Start PostgreSQL server, create tox21enricher database, and restore database content from backup file
# (***) Replace "username" with the name of the user you want to own and run the database in the Docker Container.
RUN pg_ctl -D $HOME/tox21enricher-database/ -l $HOME/tox21enricher-database/logfile start && \
    createdb tox21enricher -O username && \
    psql tox21enricher < $HOME/tox21enricher-database/tox21enricher-database.sql

# Copy entrypoint script from local machine
# (***) Replace "username" with the name of the user you want to own and run the database in the Docker Container.
ENV HOME /home/username/
COPY ./entrypoint.sh $HOME/tox21enricher-database/entrypoint.sh
USER root
RUN chmod +x $HOME/tox21enricher-database/entrypoint.sh
USER username

# Edit PostgreSQL configuration to allow connections
# (***) Replace "username" with the name of the user you want to own and run the database in the Docker Container.
RUN ls $HOME/tox21enricher-database/
RUN echo "host\ttox21enricher\tusername\t\t0.0.0.0/0\t\ttrust" >> $HOME/tox21enricher-database/pg_hba.conf

# Entrypoint
# (***) Replace "username" with the name of the user you want to own and run the database in the Docker Container.
ENTRYPOINT ["/home/username/tox21enricher-database/entrypoint.sh"]
