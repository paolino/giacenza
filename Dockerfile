FROM alpine:latest

WORKDIR /app

# Copy /nix/store
COPY ./tmp /nix/store
COPY ./tmp/giacenza /app
CMD ["/app/giacenza"]