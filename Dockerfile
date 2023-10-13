FROM openjdk:17-ea-11-jdk-slim as build
# Create working directory
RUN mkdir /app
COPY . /app/
WORKDIR /app
RUN chmod +x ./gradlew
RUN ./gradlew clean build --exclude-task test

#FROM openjdk:17 as builder
#FROM openjdk:17-jdk-slim
#COPY . /app
#COPY --from=builder build/libs/*.jar /app/core.jar
EXPOSE 8080

#RUN chmod +x app/core.jar