FROM haskell:8.4

ENV PATH "/root/.local/bin:/$PATH"

RUN apt-get update && apt-get install -y xz-utils make vim

RUN stack setup
