#!/usr/bin/env Rscript

library(Rook)
library(RJSONIO)

source('resultats.r')

myPort <- Sys.getenv('PORT', 8080)
myInterface <- "0.0.0.0"
status <- -1

# Main endpoint
R.hash <- function(env) {
	# Retreive the request
    request <- Rook::Request$new(env)
    # Convert the hash to a vector
	hash = strsplit( request$GET()$hash, '')
	# Get the distance
	result = result <- tryCatch({
		distance(hash[[1]])
	}, error = function(err) {
		list(error=err$message)
	})
	# Add the hash to the resuling JSON
	body = RJSONIO::toJSON(result)
	# Returns a list
	list(
    	status=200L,
    	headers=list(
    		'Content-Type'='application/json',
    		'Access-Control-Allow-Methods'='*',
			'Access-Control-Allow-Credentials'='true',
			'Access-Control-Allow-Origin'='*',
			'Access-Control-Allow-Headers'='Content-Type, *'
    	),
	    body=body
	)
}

# R 2.15.1 uses .Internal, but the next release of R will use a .Call.
# Either way it starts the web server.
if (as.integer(R.version[["svn rev"]]) > 59600) {
    status <- .Call(tools:::startHTTPD, myInterface, myPort)
} else {
    status <- .Internal(startHTTPD(myInterface, myPort))
}

if (status == 0) {
    unlockBinding("httpdPort", environment(tools:::startDynamicHelp))
    assign("httpdPort", myPort, environment(tools:::startDynamicHelp))

    s <- Rhttpd$new()
    s$listenAddr <- myInterface
    s$listenPort <- myPort

	# Add the main endpoint to the server
    s$add(app=R.hash, name="distance")
    # Now make the console go to sleep. Of course the web server will still be
    # running.
    while (TRUE) Sys.sleep(24 * 60 * 60)
}

# If we get here then the web server didn't start up properly
warning("Oops! Couldn't start Rook app")