FROM haskell:8.2.1

ENV PATH "/root/.local/bin:/$PATH"

RUN apt-get update && \
    apt-get install xz-utils make && \
    stack upgrade

RUN stack update
RUN stack install hfmt
