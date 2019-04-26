FROM nginx:latest

COPY ./build /var/www
COPY nginx.conf /etc/nginx/nginx.conf
COPY default_location /etc/nginx/vhost.d/default_location

EXPOSE 80
ENTRYPOINT ["nginx", "-g", "daemon off;"]
