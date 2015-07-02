install:
	sh init.sh
	
run:
	R -f server.r

deploy:
	git push heroku master
