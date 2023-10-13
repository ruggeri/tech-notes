HDCP (High-bandwidth Digital Content Protection) is by
Intel.

Basically: a device manufacturer requests a key from Intel. Intel
signs a key for the device manufacturer.

When you plug the device into an Intel machine, the Intel chip will
first ask the device to present its signed key. After verifying the
key is signed properly, the Intel chip negotiates a session key with
the device. The Intel chip can now encrypt and stream the
high-definition data to the device.

Because the key must be signed by Intel, it means that Intel can
require that devices make it hard to access the content. In addition,
because of the encryption, it's hard to steal the media in transit.

However, Intel's master key somehow leaked (Intel claims that it must
have been reverse engineered). That means that anyone can sign a key
as Intel.

Presumably not all 4k UHD content needs to be sent with HDCP 2.2. This
is required for licensed Blu Ray UHD hardware/software players. But rips
of 4k UHD content can be played by an unlicensed player which presumably
doesn't need to encrypt the data. In theory, 4k UHD can be played by a
TV/monitor that doesn't support HDCP 2.2, but this would need to have
HDMI 2.0, which tends to imply HDCP 2.2 conformance also.

A few notes. HD DVD and Blu-Ray content is encrypted, and the required
decryption keys have already leaked.

## CSS Aside

DVDs were protected by CSS. DVD content was encrypted by a single-use
key. This key was itself encrypted 100s of times on each disc; once
for each model of DVD player. Each DVD player model was provisioned a
key to decrypt the content-key.

So this means you can't decrypt the DVD without a provisioned
decryption key. The best way to get the provisioned key: extract it
somehow from a device or steal it from a compromised manufacturer.

In theory, the industry could subsequently revoke a key that got
compromised. On newly issued DVDs, you would stop encrypting the
content key with the device key.

However, that means invalidating all devices that use that key! That
means effectively bricking a bunch of devices! So it wasn't really
feasible to do this.

## AACS

Under AACS, _every_ device gets its own provisioned key. If any one
key is extracted/compromised, then that single key can be
revoked. That means only one device is bricked.

AACS also uses a strategy where scenes are encoded several times, and
a given player can only decrypt one of the versions. Presumably if
there are 32 such segments and 2 options of each, this allows unique
identification of 2\*\*32 devices. Digital watermarks are put into each
of the scene versions, so that if content is leaked then the device
responsible can be identified.

**TODO**: How do the device keys decrypt the title key?

Software players of DVD content are inherently vulnerable, because the
decryption keys can be stolen from main memory. Thus Trusted Computing
et cetera where the chip does not allow access to decryption keys. For
this reason, Blu Ray association requires that a licensed Blu Ray UHD
software player use SGX (Intel Software Guard Extensions) to protect the
memory. However, Intel dropped SGX starting 2022, so now no current
Intel machine can play 4k UHD Blu Ray disks. Of course, it can play
_rips_, because those have already decrypted the Blu Ray disk and
extracted the video content.

## Practical

It looks like Netflix restricts Firefox **and** Chrome to 720p! Safari
can play 1080p on OSX. Edge can play up to 4K on OSX (weird!).

Amazon Prime Video will only play in standard definition on
Firefox. On Safari HD is played.

HBO Now doesn't explicitly say, but it appears to be playing in SD on
Firefox vs HD in Safari.
