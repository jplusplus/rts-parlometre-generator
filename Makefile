install:
	R -f init.r

run:
	R -f server.r

deploy:
	git push heroku master
