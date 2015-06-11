
library(Rook)
library(RJSONIO)

source('resultats.R')

# Creates an HTTP server
R.server <- Rhttpd$new()
# Main endpoint
R.hash <- function(env) {
	# Retreive the request
    request <- Rook::Request$new(env)
    # Retreive the hash from query parameters
	hash = request$GET()$hash
	# Add the hash to the resuling JSON
	body = RJSONIO::toJSON( distance(c(3, 5, 3, 1)) )
	# Returns a list
	list(
    	status=200L,
    	headers=list('Content-Type'='application/json'),
	    body=body
	)
}
# Add the main endpoint to the server
R.server$add(app=R.hash, name="distance")
R.server$start(port=8080)

while (TRUE) Sys.sleep(24 * 60 * 60)