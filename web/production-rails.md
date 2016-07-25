0. You'll have to create the prod database and migrate it.
1. You have to `rails -e production -p 8080`
2. You have to setup nginx to proxy.
    * `/etc/nginx/sites-enabled/default`
    * https://www.nginx.com/resources/wiki/start/topics/examples/loadbalanceexample/
    * service nginx reload
3. Have to compile assets for production.
    * Also need to install `rails_serve_static_assets`, unless you want
      to configure nginx for this...
