FROM facilebio/facilebio:latest
LABEL maintainer="Steve Lianoglou (slianoglou@gmail.com)"

RUN --mount=type=secret,id=GITHUB_PAT \
    GITHUB_PAT="$(cat /run/secrets/GITHUB_PAT)" \
    Rscript -e "BiocManager::install(c(\
        'facilebio/FacileDashboard@wtf-docker' \
    ))" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN addgroup --system app \
    && adduser --system --ingroup app app
WORKDIR /home/app
# COPY app .
# RUN chown app:app -R /home/app

USER app
COPY Rprofile.site /home/app/.Rprofile

# CMD ["R", "-e", "options(tz='America/Los_Angeles');shiny::runApp('/home/app', port = 3838, host = '0.0.0.0')"]
CMD ["R", "-e", "FacileDashboard::run('/data', gdb_idtype = 'ensembl')"]

