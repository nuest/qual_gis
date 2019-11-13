FROM rocker/binder:3.5.2
LABEL maintainer='Daniel Nüst'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}



RUN wget https://github.com/nuest/qual_gis/raw/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2019-11-13/')); devtools::install_deps()"

RUN rm DESCRIPTION.1; exit 0
