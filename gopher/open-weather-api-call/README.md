# README #

Make sure you have the open weather api token. Replace it to `{token}` below.

## In terminal ##

I made this for my testing the major function of this project. 

`go run ./bin/weather-call-terminal -coord=33.44,-94.04 -token={token}`

`env OPEN_WEATHER_API={token} go run ./bin/weather-call-terminal`

`env OPEN_WEATHER_API={token} go run ./bin/weather-call-terminal -coord=33.44,-94.04`


## In http server ##

In terminal one:

`go run ./bin/http-server`

In terminal two:

`curl http://localhost:8080?coord=33.44,-94.04&token={token}`

Or just visit `http://localhost:8080/?coord=40.7128,-74.0060&token={token}`

