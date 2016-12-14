* To setup resque, you need to install redis.
* You write a job in `app/jobs`. That's simple.
* You configure rails to use resque. Simple.
* You need to run a resque worker to do the job. You can do this via:

    QUEUE=reminder_* rake environment resque:work

* (reminder is the name of my app).
* But to run it as a service, that involves systemd. So I wrote
  `/etc/systemd/system/reminder_resque.service`:

```
[Unit]
Description=Resque worker for reminder

[Service]
ExecStart=/bin/bash -c 'cd /home/ubuntu/reminder \
  && RAILS_ENV=production QUEUE=reminder_production_* \
  /home/ubuntu/.rbenv/versions/2.3.3/bin/bundle exec \
  /home/ubuntu/.rbenv/versions/2.3.3/bin/rake environment resque:work'
ExecStop=/bin/bash -c 'kill $MAINPID && wait $MAINPID'
```

* This is pretty weaksauce probably, but it does what I need.
* You can start by `sudo systemctl start reminder_resque.service`.
