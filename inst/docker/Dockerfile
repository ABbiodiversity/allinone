FROM rocker/geospatial:latest

ENV _R_SHLIB_STRIP_=true
RUN install2.r -e remotes jsonlite plumber Matrix
RUN R -q -e "remotes::install_github('ABbiodiversity/allinone')"

RUN addgroup --system app && adduser --system --ingroup app app
WORKDIR /home/app
RUN git clone https://github.com/ABbiodiversity/allinone-coefs.git
COPY index.R .

RUN chown app:app -R /home/app
USER app

EXPOSE 8080
CMD ["Rscript", "index.R"]
