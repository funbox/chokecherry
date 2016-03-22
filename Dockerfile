FROM erlang:17

RUN mkdir /mylib
WORKDIR /mylib
ADD . /mylib

RUN make
