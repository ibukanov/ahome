FROM debian:bullseye

RUN apt-get -qqy update && apt-get -qqy --no-install-recommends install \
  init

# Start the systemd Init service by default
CMD ["/sbin/init"]
