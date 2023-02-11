package src

import "os"

type APIToken string

func (token *APIToken) String() string {
	return string(*token)
}

// Read token from env var or shell argv
func ReadAPIToken(argv string) APIToken {
	if argv != "" {
		return APIToken(argv)
	}

	return APIToken(os.Getenv("OPEN_WEATHER_API"))
}
