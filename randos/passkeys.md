## iPhone Passcode

You unlock your computer with a password. You unlock your iPhone with a
PIN passcode.

To stop passcode cracking, the iPhone applies a timeout when you input
the password incorrectly for four times. It stops more password attempts
for a few minutes. It will then allow 10 more incorrect password
attempts and then apply the timeout again. Eventually, it will require
you to unlock the phone with "iTunes". I think these folks are Windows
users... I think the point is that you have to login using a computer
that is signed into your Apple account.

Source: https://www.reddit.com/r/applehelp/comments/m15kuv/purpose_of_erase_data_after_10_failed_attempts/

You can flip a setting that will erase the phone after 10 incorrect
attempts. This means that maybe the phone is slightly more resistent to
passcode cracking or coercion of the owner. However, it is annoying in
case a toddler or someone types the wrong passcode ten times.

On the plus side, the phone data can still be recovered from backups. I
believe a backup is synced to iCloud about every 24hrs (probably only
via WiFi, not cell network).

For me, it probably doesn't make sense to turn on automatic deletion. I
could still be coerced to logging into my iCloud account, and I sync all
data to that. Also, the timeout feature for passcode cracking already
seems fairly effective.

I could in theory upgrade my passcode to be longer/have more entropy.
That might be smart.

## TouchID and FaceID

I believe that TouchID and FaceID both work by storing some defining
attributes in the device's secure enclave. This should not be readable
by anyone else. It is not uploaded to the cloud.

The phone projects a infrared grid onto your face to make a 3d map of
your face. It then checks this against the stored characteristics in the
secure enclave.

The passcode is required after iPhone device restart, 48hrs of activity,
or 5 unsuccessful scans. You can also disabled FaceID by holding power
and a side button; that forces you to open with the passcode.

## Application Use Of TouchID/FaceID

The application can make a request to the operating system to verify.
The operating system will then say whether TouchID/FaceID authenticated.
