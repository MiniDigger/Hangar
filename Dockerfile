# This Dockerfile is designed to be used for production
FROM openjdk:8-jdk-alpine

LABEL maintainer="MiniDigger <admin@minidigger.me>"

# Temporary build folder for the 'stage' task
WORKDIR /home/ore/build
# The .dockerignore file on the project root avoids build cache and personal configuration to be included in the image
ADD . ./

# TODO use Docker secrets for the app key and passwords (and any other sensible information)
ENV SBT_VERSION=1.2.1 \
    SBT_HOME=/usr/local/sbt \
    SBT_OPTS="-Xss4m -Xms2048m -Xmx2048m -XX:ReservedCodeCacheSize=256m -XX:MaxMetaspaceSize=512m"

ENV PATH=${PATH}:${SBT_HOME}/bin

# TODO a shell script to extract the SBT version from project/build.properties and set SBT_VERSION to the output value
RUN cp ore/conf/application.conf.template ore/conf/application.conf && \
 cp jobs/src/main/resources/application.conf.template jobs/src/main/resources/application.conf

RUN apk update && \
 apk add --virtual --no-cache curl ca-certificates bash yarn

# Downloads SBT with the version given above and extracts it
RUN curl -sL "https://piccolo.link/sbt-$SBT_VERSION.tgz" -o "sbt-$SBT_VERSION.tgz" && \
 tar -xvzf "sbt-$SBT_VERSION.tgz" -C /usr/local

# Compiles Ore and makes a production distribution (but not in an archive, unlike 'dist')
RUN sbt stage

# Move the 'stage' task result _content_ into the production directory
RUN mkdir -p /home/ore/prod && \
 mv /home/ore/build/ore/target/universal/stage/* /home/ore/prod && \
 # Cleans the temporary build directory, as we don't need it in the final image
 rm -rf /home/ore/build && \
 # SBT is no longer needed too
 rm -rf $SBT_HOME && \
 apk del curl

WORKDIR /home/ore/prod/bin

# Ore runs on port 9000
EXPOSE 9000

CMD ["./ore"]
