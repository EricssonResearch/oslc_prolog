FROM ubuntu:18.04 as build

WORKDIR /opt

RUN apt-get update && \
    apt-get -yq install git && \
    git clone -b 'V3.1.1' --depth 1 https://github.com/ClioPatria/ClioPatria.git && \
    git clone https://github.com/EricssonResearch/oslc_prolog.git && \
    mkdir -p /opt/server/cpack && \
    mv oslc_prolog /opt/server/cpack

FROM ubuntu:18.04
LABEL maintainer "leonid.mokrushin@ericsson.com"

ARG PUBLIC_HOST=localhost
ARG PUBLIC_PORT=3020
ARG PREFIX_PATH=/
ARG EXPOSED_PREFIXES=*

WORKDIR /opt

COPY --from=build /opt/ /opt/
COPY users.db .
COPY settings.db .

RUN apt-get update && \
    apt-get -yq --no-install-recommends install software-properties-common && \
    apt-add-repository ppa:swi-prolog/devel && \
    apt-get update && \
    apt-get -yq --no-install-recommends install swi-prolog && \
    apt-get -y remove --purge software-properties-common && \
    apt-get autoremove -y && \
    rm -rf /var/lib/apt/lists/* && \
    cd server && \
    sh ../ClioPatria/configure && \
    mv ../users.db ../settings.db . && \
    sed -i 's|%PUBLIC_HOST%|'$PUBLIC_HOST'|g' settings.db && \
    sed -i 's/%PUBLIC_PORT%/'$PUBLIC_PORT'/g' settings.db && \
    sed -i 's|%PREFIX_PATH%|'$PREFIX_PATH'|g' settings.db && \
    sed -i 's/%EXPOSED_PREFIXES%/'$EXPOSED_PREFIXES'/g' settings.db && \
    swipl run.pl --after_load='cpack_configure(oslc_prolog), halt'

WORKDIR /opt/server

EXPOSE 3020

CMD ["swipl","run.pl"]
