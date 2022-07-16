FROM fukamachi/sbcl:latest-alpine

RUN apk add git file && \
    ros install fukamachi/rove fukamachi/cl-coveralls
RUN ros install alexandria
RUN ros install cl-custom-hash-table

ENTRYPOINT ["/bin/sh"]
