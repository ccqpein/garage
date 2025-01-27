# README #

this is the example of One Time Password

use lib [otp](https://github.com/pquerna/otp)

## Steps ##

in `main.go` just show how to do it. I use okta as my OTP app. 

my security is hardcode in `lib/service.go`. it is easy to test.

### Step 1 ###

> secret, qrURL, err := userService.EnableTOTP("john")

show the qrURL and the secret (I guess it is the base64 encoding of my hardcode secret). The qrURL actually just the URL, I think it is used to generate the qrcode. But now, just the URL. And when I click it, it open the password app on my mac. So I think I just ignore it for now. 

### Step 2 ###

Open Okta (I guess a lot other app can do the same thing too), and add the new account, and input the secret. 

### Step 3 ###

Then the code from okta is the code of `ValidateAndEnableTOTP`. Looks like from now, the code are sync. The `loginCode` code can be the same as code used in `ValidateAndEnableTOTP`, as long as okta doesn't expired it
