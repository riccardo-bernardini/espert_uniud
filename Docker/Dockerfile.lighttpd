FROM debian
RUN apt-get update
RUN apt-get install -y lighttpd git gprbuild gnat libpng-dev
# RUN apt-get install -y  libpng16-16

WORKDIR /tmp
RUN git clone https://github.com/riccardo-bernardini/espert_uniud.git
WORKDIR espert_uniud/accumulator
RUN gprbuild
