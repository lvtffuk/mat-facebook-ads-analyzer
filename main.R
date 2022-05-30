library(shiny)

port <- Sys.getenv("PORT")

if (exists(port)) {
	port <- strtoi(port)
} else {
	port <- 8080
}

shiny::runApp(getwd(), port, FALSE, "0.0.0.0")
