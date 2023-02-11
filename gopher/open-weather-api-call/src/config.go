package src

import "os"

type APIToken string

// Read token from env var or shell argv
func ReadAPIToken(argv *string) APIToken {
	if argv != nil {
		return APIToken(*argv)
	}

	return APIToken(os.Getenv("OPEN_WEATHER_API"))
}
