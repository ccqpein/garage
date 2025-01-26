package lib

import (
	"fmt"
)

type User struct {
	Username    string
	TOTPSecret  string
	TOTPEnabled bool
}

type UserService struct {
	TotpService *TOTPService
	Users       map[string]*User // In practice, use a real database
}

func NewUserService() *UserService {
	return &UserService{
		TotpService: NewTOTPService("MyApp"),
		Users:       make(map[string]*User),
	}
}

// EnableTOTP sets up 2FA for a user
func (s *UserService) EnableTOTP(username string) (string, string, error) {
	user, exists := s.Users[username]
	if !exists {
		return "", "", fmt.Errorf("user not found")
	}

	key, err := s.TotpService.GenerateSecret(username)
	if err != nil {
		return "", "", err
	}

	// Store the secret
	user.TOTPSecret = key.Secret()

	// Return both the secret and QR code URL
	return key.Secret(), key.URL(), nil
}

// ValidateAndEnableTOTP verifies the initial TOTP setup
func (s *UserService) ValidateAndEnableTOTP(username, code string) error {
	user, exists := s.Users[username]
	if !exists {
		return fmt.Errorf("user not found")
	}

	if s.TotpService.ValidateCode(user.TOTPSecret, code) {
		user.TOTPEnabled = true
		return nil
	}
	return fmt.Errorf("invalid code")
}

// Login validates a user's TOTP code during login
func (s *UserService) Login(username, totpCode string) error {
	user, exists := s.Users[username]
	if !exists {
		return fmt.Errorf("user not found")
	}

	if user.TOTPEnabled {
		if !s.TotpService.ValidateCode(user.TOTPSecret, totpCode) {
			return fmt.Errorf("invalid TOTP code")
		}
	}

	return nil // Login successful
}
