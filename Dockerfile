FROM java:8-alpine
MAINTAINER Your Name <you@example.com>

ADD target/uberjar/arcanium.jar /arcanium/app.jar

EXPOSE 3000

CMD ["java", "-jar", "/arcanium/app.jar"]
