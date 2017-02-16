FROM scratch
ADD example-app.tar.gz /
EXPOSE 8081
ENTRYPOINT ["/bin/example-app"]
