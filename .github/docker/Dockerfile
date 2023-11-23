FROM toxchat/toktok-stack:haskell AS build

RUN ["rm", "-rf", "/work/hs-apigen"]
COPY --chown=builder:users . /work/hs-apigen
RUN ["stack", "install", "apigen"]

FROM scratch
COPY --from=build /home/builder/.local/ /
