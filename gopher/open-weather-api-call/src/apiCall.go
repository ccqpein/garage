package src

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"net/url"
	"strings"
)

type Request struct {
	lat, lon string
	token    *APIToken
}

func NewWeatherRequest(coord string, token *APIToken) (*Request, error) {
	lat, lon, err := ParseCoord(coord)
	if err != nil {
		return nil, err
	}
	return &Request{
		lat:   lat,
		lon:   lon,
		token: token,
	}, nil
}

func CallAPI(req *Request) ([]byte, error) {
	baseURL := "https://api.openweathermap.org"
	//resource := "/data/2.5/weather"
	resource := "/data/3.0/onecall"

	data := url.Values{}
	data.Set("lat", req.lat)
	data.Set("lon", req.lon)
	data.Set("appid", req.token.String())

	u, _ := url.ParseRequestURI(baseURL)
	u.Path = resource
	u.RawQuery = data.Encode()

	log.Printf("endpoint: %s\n", u.String())

	res, err := http.Get(u.String())
	if err != nil {
		return nil, err
	}

	defer res.Body.Close()

	body, err := ioutil.ReadAll(res.Body)
	if err != nil {
		return nil, err
	}

	return body, nil
}

type Response struct {
	condition, feel string
	alerts          []struct {
		event, description string
	}
}

func (resp *Response) Byte() []byte {
	var b strings.Builder
	fmt.Fprintf(&b, "Condition: %s\nFeel: %s\n", resp.condition, resp.feel)
	if resp.alerts != nil {
		b.WriteString("Alerts:\n")
		for i, a := range resp.alerts {
			fmt.Fprintf(&b, "alert %d: %s. %s\n", i, a.event, a.description)
		}
	}

	return []byte(b.String())
}

func HandleResponse(body []byte) *Response {
	var resp map[string]any
	json.Unmarshal(body, &resp)

	var feelLikeTemp float64 = resp["current"].(map[string]any)["feels_like"].(float64)
	var weather = resp["current"].(map[string]any)["weather"].([]any)
	var condition string = weather[0].(map[string]any)["main"].(string)

	var re = Response{condition: condition}

	// temp
	if feelLikeTemp-273 < 5 {
		re.feel = "Cold"
	} else if feelLikeTemp-273 < 25 {
		re.feel = "Moderate"
	} else {
		re.feel = "Hot"
	}

	// alerts
	if v, ok := resp["alerts"]; ok {
		allAlerts := v.([]any)
		for _, a := range allAlerts {

			re.alerts = append(re.alerts, struct {
				event, description string
			}{
				event:       a.(map[string]any)["event"].(string),
				description: a.(map[string]any)["description"].(string),
			})
		}
	}

	return &re
}
