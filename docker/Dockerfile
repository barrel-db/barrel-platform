FROM ubuntu:wily

RUN apt-get update && apt-get install -y \
    curl            \
    erlang-nox      \
    erlang-dev      \
    g++             \
    git             \
    libcurl4-openssl-dev \
    libicu55 \
    libicu-dev      \
    libmozjs185-dev \
    make \
    vim

RUN mkdir /src
RUN cd /src && git clone --branch docker https://github.com/barrel-db/barrel.git

RUN cd /src/barrel && \
    make update && \
    make rel

RUN cp -r /src/barrel/_build/prod/rel/barrel /

EXPOSE 5984

VOLUME ["/barrel/data", "/barrel/log", "/barrel/etc"]



