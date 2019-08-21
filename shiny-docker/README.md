Start something like this:

```
docker run --rm -p 80:3838 \
    -v /home/app/:/srv/shiny-server/ \
    -v /home/app_log/:/var/log/shiny-server/ \
    ameshkov/mro-shiny

```

Where `/home/shiny/` is the folder with your app and `/home/shiny_log/` is the folder for the logs.
Use `sudo` with `docker` when it needed.

You can also compose it with reversed proxy using `docker-compose` in this way (content of the `docker-compose.yml` for Shiny Server + Caddy):

```
version: "0.1"
services:
  caddy:  
    image: joshix/caddy
    links:
      - mro-shiny
    volumes:
      - /home/www/:/var/www/html
    ports:
      - 80:80
      - 443:443
    restart: always

  mro-shiny:
    image: ameshkov/mro-shiny
    volumes:
      - /home/app/:/srv/shiny-server/
      - /home/app_log/:/var/log/shiny-server/
    ports:
      - 3838:3838
    restart: always

```

Do not forget to add the `Caddyfile` to your `/home/www/` directory.