# README #


## In terminal ##

`go run ./bin/weather-call-terminal -coord=33.44,-94.04 -token={token}`

`env OPEN_WEATHER_API={token} go run ./bin/weather-call-terminal`

`env OPEN_WEATHER_API={token} go run ./bin/weather-call-terminal -coord=33.44,-94.04`


## In http server ##

`go run ./bin/http-server`

`curl http://localhost:8080?coord=33.44,-94.04&token={token}`
