FROM debian

COPY . /app
WORKDIR /app

RUN apt-get update && apt-get -y install cabal-install hpack libghc-zlib-dev  libghc-zlib-bindings-dev libpq-dev

RUN cd /app/FormatMeBot && rm FormatMeBot.cabal && hpack && cabal update && cabal build

# EXPOSE 8080 
# EXPOSE 8081
# EXPOSE 443
# EXPOSE 80
# EXPOSE 88
# EXPOSE 8443

# ENTRYPOINT [ "/bin/bash", "-l", "-c"]


# CMD ./FormatMeBot-exe


# cd Schwertbot
# rm Schwertbot.cabal
# hpack 
# cabal update
##  cabal install record-dot-preprocessor
# cabal build