FROM gradle:latest AS BUILD
# Create working directory
RUN mkdir /app
COPY . /app/
WORKDIR /app
RUN ./gradlew clean build

#FROM openjdk:17 as builder
#FROM openjdk:17-jdk-slim
#COPY . /app
#COPY --from=builder build/libs/*.jar /app/core.jar
EXPOSE 8080

#RUN chmod +x app/core.jar