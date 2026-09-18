# Yubikey

- I bought 2x YubiKey 5C NFC on 2026-08-27 for $126.01.
  - Motivation was paranoia after attack on father's financial accounts.
  - As of 2026-09-18, one is already bent!!
- The Yubikey can plug in via USB-C. It can also be used through NFC
  with a Yubikey app on the phone (my iPhone 13 has only Lightning as of
  2026-09-18).
- One Yubikey is a primary and the other is a spare.
- Here are some things you can store on a Yubikey...
- You can store normal TOTP second factors in a Yubikey. Any website
  that gives you a TOTP can have that TOTP stored in the Yubikey. I
  believe this is called OATH.
  - But you will have to copy the TOTP out of Yubikey Authenticator app
    (iPhone or MacOS) and copy that into the website.
  - So it is ergonomically worse than using 1Password to store TOTP and
    fill them via browser plugin.
  - If you fear your machine getting owned to the point where all the
    TOTPs can be ripped out of 1Password, then it *might* be safer to
    keep the TOTP on the Yubikey.
- You can store passkeys on the Yubikey.
  - This allows passwordless sign-on.
  - The browser is told to request the passkey. It gives you an option
    of picking a password manager, or a hardware key. You pick Yubikey.
  - I believe this method of the server asking the browser to ask an
    authenticator for a passkey is called "FIDO".
  - I believe the passkey is basically very similar to a TOTP. Except
    passkeys are intended as password replacements, not exactly *second*
    factors.
- There is also "FIDO security-key credential"
  - This is very similar to a passkey. It's a time-based OTP. It is
    stored on the Yubikey. It cannot be accessed by the user directly.
    It is only delivered to a host via FIDO.
  - But the intent is that you will still have a username and password
    (passkey doesn't need *either*!). So this is more of a *second*
    factor.
  - These are "nondiscoverable". That is: the Yubikey doesn't list out
    the nondiscoverable/non-passkey credentials. I guess that's a
    security plus because no one can see what services you enroll in.
  - Whereas passkeys must be discoverable to allow usernameless sign-in.
- One feature of the Yubikey is that it will not provide a code to the
  machine without a physical touch. This prohibits a compromised
  environment from ripping off all of the passkeys in a plugged-in
  Yubikey without your intervention.
- My overall feeling is that the entire system is poorly designed. You
  have a variety of credentials that can be stored on the key: TOTPs
  (OATH), passekys, nondiscoverable FIDO security-credentials. You have
  multiple "apps". You have different PINs/passwords for each. You have
  different policies for reset after PIN failure...
  - And, on top of everything, websites can have different policies
    about whether they really *require* the credential (e.g., whether it
    is even required on *every login*), and especially policies on
    *recovery* after (alleged) loss of the credential.

# PINs

- There are *two separate* PINs that you can set.
- One for time-based TOTPs, and one for passkeys and nondiscoverable
  FIDO security-key credentials.
- I don't know they bother with two PINs, that seems dumb.
  - A legacy of two applications running on the Yubikey.
  - The FIDO2 app will reset after 8 failed PINs.
  - The OATH app does not reset after repeated failure. I believe the
    intent is that you are supposed to use a longer password similar to
    what you would use with a password manager.
- Some versions of Yubikey use a biometric touch.
- A service storing a credential on the key can tell the key that the
  user must verify themselves. This is a request for the key to perform
  some verification of the user. A key that has a fingerprint sensor
  could require a fingerprint touch. But on a standard key like I have
  you provide the pin or password. If a pin is not configured, the key
  will not store and will reject credentials which request user
  verification.

# Services Stored

- Highest value cloud services (Apple, Dropbox, Google, AWS, GitHub) are
  enrolled in two factor. These are typically nondiscoverable FIDO
  security credentials.
  - Google in particular has "Advanced Protection Program" enrolled
    which requires FIDO token and makes account recovery more
    restrictive.
- Many banks allow passkey logon, while still allowing username/password
  flow.
  - That is mostly useless. Because 1Password protects me from my
    password being phished by phony websites already...
  - And almost no financial institutions actually support TOTP.
  - And I'm sure financial institutions have weak password reset
    policies that can be social engineered anyway...
