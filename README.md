# RTS Parlometre Generator

HTTP server written in R to handle maps generation for [parlometre.ch](http://parlometre.ch).

## Install

On Ubuntu/Debian, install R with:

	sudo apt-get install r-base

Then run:

	make install

An invite may ask you to choose a CDN to download external packages.

## Run

Simply enter this command:

	make run

The go to [127.0.0.1:8080/custom/distance?hash=XXXX](http://127.0.0.1:8080/custom/distance?hash=3231333123324133312321321).
If you want to run the server on a different port, change the environment variable `$PORT`value.

	export PORT=8181
	make run

## Deploy on heroku

First, you need to create an app using an Heroku Buildpack for R:

	heroku create --stack cedar-14 --buildpack http://github.com/virtualstaticvoid/heroku-buildpack-r.git\#cedar-14

As you can see a Procfile is already present in the projet. It will take care of
launching the server for you.

Now that your application is ready, you juste have to push it to heroku:

	git push heroku master