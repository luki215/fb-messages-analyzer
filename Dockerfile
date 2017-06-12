FROM haskell:8
RUN stack install ghc-mod
RUN mkdir /var/www
