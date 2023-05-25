**This article actually has nothing about passkeys!**

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

But if that's the case: why bother with making you enter the Apple
account password on a computer? Why not directly into the phone?

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
seems fairly effective against the state.

In May 2023, I upgraded my iPhone passcode to have more entropy.

## TouchID and FaceID

I believe that TouchID and FaceID both work by storing some defining
attributes in the device's secure enclave. This should not be readable
by anyone else. It is not uploaded to the cloud.

The phone projects a infrared grid onto your face to make a 3d map of
your face. It then checks this against the stored characteristics in the
secure enclave.

The passcode is required after iPhone device restart, 48hrs of activity,
or 5 unsuccessful scans. You can also disable FaceID by holding power
and a side button; that forces you to open with the passcode.

Consistent with this, MacOS requires the passphrase on device brestart,
every 48hrs, or after five unsuccessful scans. The best way to disable
TouchID quickly is probably to press and hold the power button/TouchID
sensor for five seconds. Passphrase will be required on restart.

Presumably there is some verification that makes sure the physical
TouchID/FaceID hardware is being used. You wouldn't want someone to be
able to scan a face and then offline send a scan of that face to the
trusted module.

**Application Use Of TouchID/FaceID**

The application can make a request to the operating system to verify.
The operating system will then say whether TouchID/FaceID authenticated.

An encrypted application like 1Password will store an encrypted version
of the password with iOS. When FaceID authenticates the user, the
password is decrypted and presented to 1Password.

It is described that the password will be deleted after a certain time
window, or number of FaceID attempts fail. If you remove a passcode from
the device, all passwords in the iOS Keychain are deleted.

Source: https://support.1password.com/face-id-security/

1Password will ask for the password every two weeks on iOS. Until then,
it will use FaceID. It's kind of annoying you can't make it ask for the
passphrase every 24hr or so. It appears the 9th circuit has said you
can't be compelled to open your phone with FaceID. But that seems
relatively new (2019), and of course it won't apply in all countries.

You can always lock your phone quickly by pressing the side and power
buttons. Then the passcode will be required.
