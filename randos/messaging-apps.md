# WhatsApp

## Authentication and End-To-End

- Traditionally, WhatsApp had *no* username and *no* password. Not even
  any *email* address on file.
- You log in with your *phone number*. A text code is sent to the
  number. You confirm control of the number by inputting the code to
  WhatsApp. That logs you in.
- After authentication, a public/private key pair is established on
  device. The public key is uploaded to WhatsApp's servers.
  - The private key is not intended to be extracted from the WhatsApp
    app. You can't take it to another device.
- When a sender wants to send a message, it encrypts the message with
  the public key.
- WhatsApp stores the encrypted message on the server. It can't view the
  message contents. The message is eventually transmitted to the client
  (and deleted from the server on confirmation of send). On the client
  is can be stored and read.
  - WhatsApp will queue messages for up to 30 days.
- You typically *never* are logged out of the app. At least, not until
  your phone number is used to sign into a new client.
- When your phone number is used to re-register to WhatsApp, a new
  public/private key will be generated.
  - New device can't read chat history, since not stored on servers.
  - The new device doesn't have the old key, so cannot read queued
    messages on server.
  - Conversations with your phone number continue with the new
    individual, but correspondents are notified in the message thread
    that a new device was registered. They are made aware that they may
    be talking to someone new.
  - Actually, you must turn on "security notifications" to be informed
    when the public key for a correspondent changes. It's not on by
    default!
- Mostly, WhatsApp doesn't care, after initial registration, whether you
  can receive SMS messages. Thus you can roam with WhatsApp and retain
  ability to send/receive messages. You could even give up your phone
  number and still use WhatsApp until someone tries to reregister your
  phone number.
  - So you can roam indefinitely.
  - Even if you eventually need to re-register, often you can still do
    this without being able to receive SMS by using either a registered
    email address, or receiving a challenge on your old phone via
    WhatsApp (see more below).
- When a new device logs in with your phone number, you will be logged
  out of your WhatsApp on your old device. That stops you from reading
  your historical messages. Of course, you cannot send/receive any
  messages either.
  - Still, you can retain a local backup which can be used to
    restore/continue conversation history if you re-log-in on the old
    device.

## WhatsApp Backup and Migration

- If the phone is lost/destroyed, you cannot restore prior chat history
  onto a new device from data on WhatsApp servers, because they don't
  store any historical messages. Also, you will not be able to
  regenerate the secret key. Thus you cannot read any recently queued
  messages either.
- Cloud backup is supported (Google Drive or iCloud).
- You can set it to automatically backup. Or just trigger manual
  backups.
- Backups are unencrypted by default. You can enable encryption, in
  which case WhatsApp generates a high entropy password which you must
  record in a password manager.
- You can restore from a backup without the old phone.
  - You must be signed into the cloud service.
  - You must provide the encryption password (if you used encryption).
  - This works Android->Android, or iPhone->iPhone.
  - To switch Android->iPhone or iPhone->Android, you must use a
    direct-transfer method (WiFi or cable). Cloud backups are not
    compatible across different phone ecosystems.
  - So if you lose an iPhone and buy an Android, you have no easy way to
    use your iCloud backup to setup the Android WhatsApp. You'd have to
    commission a temporary iPhone WhatsApp! It can be done but very
    clunky.
- You can port over conversation history from one phone to the next
  either via cloud backup (as discussed, for iPhone->iPhone or
  Android->Android), or via data transfer (WiFi or cable).
  - I did this with no loss of history in 2026 iPhone->Android.
- As of 2026-11-XX, I have encrypted backups set up daily on WhatsApp.
  - You must also make sure you enable *cloud* backup. Else it only
    makes a *local* backup.
  - You must select the schedule on which it performs cloud backup.
  - A local backup has a pretty narrow use case... I'm not sure I
    understand the use.
- The backup encryption password *cannot* be recovered! It is not stored
  on WhatsApp's servers.
  - However, if you still have the logged in WhatsApp client, you can
    *reset* the backup encryption password locally.
  - Effectively, you just start saving backups with a new encryption
    password.

## Data Export/Archive

- My WhatsApp history as of 2026 and extends back to 2019. But would I
  count on another 7yrs uninterrupted?
- Google Drive backups expire after 5mo. iCloud backup doesn't have a
  specified expiration period.
  - Neither allows you to directly download the file to store on your
    own machine.
  - The intent is that WhatsApp continues to make backups, and the
    backup is only supposed to exist in between the time you lose a
    phone and replace it.
  - But this isn't suitable for long-term archival purposes.
- On Android, you may be able to copy the encrypted local backup from
  the filesystem. But that isn't exactly supported.
  - On iPhone, you can create a local iPhone backup (of the whole
    device), which should capture the local db. A tool called iMazing
    can help do this.
  - It sounds like you absolutely can get the
    `ChatStorage.sqlite`/`msgstore.db` file, which you can then of
    course use to get all the chats.
  - These can then beg provided to a tool like `whatsapp-backup-tools`
    to view/export/etc the conversations.
- The point is: the cloud backup is really meant as disaster recovery to
  restore the WhatsApp client chat history. It isn't meant as an
  archive.
- WhatsApp has very limited export functionality built-in. You can
  export only individual conversations via the app. There is a limit to
  the number of messages exported (at one point specified as 40k w/o
  media, 10k with media).
- You can't expect that archival exports like this could be re-imported
  back into the WhatsApp client. But that's not the point.

## App Lock and Chat Lock

- If the phone is stolen/confiscated, someone who can use the phone can
  open WhatsApp to send/receive messages as you.
- There are two features meant to deter this.
- First is *app lock*. This requires biometric data to open the WhatsApp
  app. This stops spies from stealing an open phone and then just
  opening WhatsApp and dumping all the chat history. I guess they'd need
  your thumb too.
- *Chat Lock* is another feature for specific chats. It hides them in a
  hidden menu, and you can require phone PIN, biometric data
  (fingerprint/face id), or a separate secret code.
- This is all entirely *local*.

## Two Step PIN/Password

- Someone who gains control of your phone number logs you out of your
  WhatsApp account. New messages to your phone number will be sent to
  them (correspondents will be warned).
- This is simply how WhatsApp works, so there is nothing wrong with
  this. If you cancel your cell phone service, and the cell provider
  allocates your number to someone new, they need to be allowed to
  register for WhatsApp.
- However, sometimes people can "steal" your phone number temporarily.
  Sensitive data sent through WhatsApp could be temporarily redirected
  to the attacker. Similar to how people steal phone numbers temporarily
  to obtain SMS two factor codes.
- To stop this, you can set up a *two-step verification PIN*.
  - This PIN is registered with WhatsApp's servers.
  - When a two-step verification PIN is registered, WhatsApp imposes a
    seven day delay on re-registration of a phone number.
  - The client is logged out as soon as a new number tries to
    re-register, but the *new* client cannot publish the public key and
    start receiving messages until either (1) the two-step verification
    PIN is provided or (2) the delay expires.
- In 2026-08-XX, WhatsApp now allows a **password**.
  - This has same use as PIN, but allows for higher entropy.
  - As of 2026-11-XX, I have set up a password.
- In the past, two-step PIN was required by the WhatsApp client about
  every seven days.
  - I think the idea was to help you *remember* the PIN by using it
    frequently.
  - I don't believe there is documentation about whether you will be
    prompted for your password.

## Email Recovery

- You may register an email address with WhatsApp to allow for resetting
  a forgotten two-step PIN/password.
  - WhatsApp will send an email sending you through a flow to reset the
    PIN.
- Your email may also be used when registering a new device.
  - Normally, you are sent a challenge via SMS.
  - However, it is possible that you have access to WiFi but not SMS.
  - Most usual case: maybe you are roaming in a foreign country and
    cannot receive texts to your home country phone number.
  - Or maybe you are in the wilderness and maybe have Starlink, but not
    cellular service.
  - Or maybe SMS is being suppressed in your country.
  - WhatsApp will allow a new device to register with your phone number
    by sending an email to the address on file. You may authorize the
    new device.
  - As usual, registration of a new device generates a new
    public/private keypair. Correspondents with "security notifications"
    turned will see your public key has been updated. As usual,
    registration does not allow the new device to read chat history
    (unless you connect cloud service), nor receive previously queued
    messages.

## Phone Number Change

- You might eventually want to change your phone number associated with
  your WhatsApp account.
  - Maybe you want to change phone number to have an area code where you
    have moved to. Especially if you have moved to a new country.
  - Largely, within the United States, people port their number if they
    change cell phone providers. So change of phone number is fairly
    uncommon short of moving abroad.
- You tell the WhatsApp client that you want to change your phone
  number. It verifies that you control the new one.
- Account settings (profile picture, security settings) will be
  retained. Chat history will be retained.
- However, if your correspondents continue to message the old number,
  the messages will not be delivered to you.
  - The correspondent will see that the message isn't being delivered.
  - I'm not sure how explicit WhatsApp is that there is no user anymore
    associated with this number.
  - Of course, if someone else registers your number (after number
    recycling), your correspondent's messages will go to the new user.
- There is an option to notify your contacts of a number change.
  - Anastassia initially registered WhatsApp using a Russian phone
    number, but then she changed her WhatsApp number to her Miami
    number.
  - This sent me a message about the change and suggested that I start
    messaging the new number.
  - I believe it *did not* merge threads. Conversation with the old
    number stayed in the old thread. Eventually, I started to see the
    new Russian user's profile next to the old thread.
  - Asymmetrically, the person changing their number does *not* see a
    break in the thread with the correspondent. Because the
    counterparty's number did not change for them.

## Multiple Clients

- You link a client (for instance, a MacOS desktop client) by scanning a
  QR code from your phone.
- I believe that the new client generates a new public/private keypair,
  and uploads an additional public key to the server.
- Contacts now send *multiple* copies of each message, encrypted for
  each of the public keys to the server. These are fetched and decrypted
  by the associated respective clients.
- The linked devices can be used even without the phone being switched
  on or in your possession.
  - After 14 days of no primary-device activity, linked devices will be
    logged out.
  - After 30 days of no linked-device activity, linked device is logged
    out.
- How is chat history synced?
  - Your device will transfer recent message history to the linked
    device.
  - It doesn't transfer a full message history.
  - Messages sent to you are encrypted once-per-device and queued on
    WhatsApp servers.
  - Messages *you* send will *also* be encrypted and sent for your
    linked devices.
- In 2026-11-XX, I *have not* set up WhatsApp on my MBP 2026. It has
  been years I think since I did that.

# Google Messages

I began using Google Messages in 2026 when I purchased a Google Pixel. I
did not attempt to port over any conversation history from iPhone.

## SMS, MMS, RCS, E2E

Google Messages is *not* a messaging *network* like WhatsApp or Signal
or iMessages (blue bubbles). It is a messaging app for SMS, MMS, and
RCS. As a reminder, SMS is for traditional texts, MMS for media and
group conversation, and RCS the latest protocol (high-resolution, typing
indicators, read receipts, and supported end-to-end encryption).

Basically, Google Messages is *similar* to iMessage, except it doesn't
offer the "blue bubbles" feature (iMessage network) and instead relies
on RCS. Perhaps blue bubbles would never have been invented by Apple if
RCS existed and was rolled out at the time. Note that Google Messages
only uses phone numbers as an identifier (iMessage allows phone number
or email).

## International Travel

Google Messages is explicit that RCS chat may continue to work for about
eight days after phsyical removal of a SIM card. If you merely *disable*
the SIM card in Android settings, RCS delivery is still not guaranteed
indefinitely.

The recommendation while traveling internationally is to disable *data
roaming*, but leave the domestic SIM activated. That will maintain RCS
(and SMS/MMS) delivery. However, that gives the possibility of incurring
voice or SMS roaming charges. As discussed elsewhere, you can avoid
voice roaming charges by not answering calls to your domestic number
(and not making any). Incoming SMS (which you cannot decline) are billed
at $0.05/SMS and $0.25/MMS. It appears receiving an SMS or unanswered
phone call *does not* incur TravelPass roaming.

An advantage of receiving SMS while traveling is that you can still get
two-factor codes.

iMessage not only allows using your email as an address (always works
even when domestic SIM is deactivated), but (as of iOS 26) also
maintains your phone number registration when the SIM is deactivated in
the settings. So that might be more convenient for international travel
(if you are okay not receiving SMS challenges)...

## End-to-End Encryption

Of course, regular SMS and MMS are never encrypted end-to-end. The
carrier can read current traffic, law enforcement can wiretap it and
collect it. Even RCS does not *mandate* end-to-end encryption, only
*support* it. It requires both endpoints be able to negotiate
encryption. Only as of 2026-05-XX (with release of iOS 26) are
Android<->iPhone RCS connections end-to-end encrypted.

Google Messages will show the transport that will be used to send the
message (SMS, MMS, unencrypted RCS, E2E RCS). You may have to request
iPhone user turn on RCS (`Settings > Apps > Messages > RCS Messaging`).

Google Messages I believe will automatically upgrade/downgrade a
one-to-one thread between SMS and RCS. However, I believe that MMS
groups may require you to start a new conversation with RCS used from
the start.

**SIM Stealing**

Someone can steal your phone number and start receiving texts sent to
them. WhatsApp warns you when a contact's e2e key changes. But Google
Messages does *not* by default. So Google Messages could say you're
using E2E encrypted RCS, but *you're messaging the wrong person*.

You can do `Details > Verify keys` to "verify" a contact's e2e
credentials. It will mark the contact as "verified." If the key changes,
it will mark them as "unverified". That's good. But this is *only*
available for other Android users (as of 2026)! So you can't do it with
an iPhone user.

## Web Client Use

You can set up a Google messages web client, but it requires phone to be
on and connected to either the internet or cell network. Even SMS and
MMS messages can often be sent through IP (so cell connection is not
totally necessary), but Google Messages web client always routes through
the cellphone. The cellphone does not need to be on the same *local*
network as the laptop, though.

Likewise, the web client can only receive messages through a cellphone
connected to either internet or cell network.

In theory, web client should be able to send messages even when phone is
offline. But it would be tricky to ensure that client remains authorized
to send messages (what if phone number is concurrently deregistered from
cellphone?). And harder still to make sure that messages are delivered
to both the phone and the web client (*and* e2e encrypted for both).
It's technically feasible (WhatsApp does it), and Google has said it
*wants* to provide phone-offline functionality, but hasn't yet. I
suspect Google is slowed because it doesn't control the message
network/protocol like WhatsApp does.

## No App Lock/Chat Lock

Unlike WhatsApp, as of 2026-11-XX, there is no way to require biometric
authentication to open the app, nor hide/lock individual conversations.

## Cloud Backup

Android automates backups for Google messages. It generates a high
entropy password and then encrypts it with your device pin. It encrypts
the backups with the high entropy password and it uploads them to Google
backup. It also uploads the high entropy password encrypted with your
pin to Google's own special hardware. This hardware is designed to not
allow many guesses to the encrypted password. If it allowed many guesses
then it could be brute forced, but the hardware doesn't allow that.

After retry limit is exceeded, the encrypted password is effectively
*destroyed*. There is a sort-of danger that your backup could be
destroyed by an attacker trying many random PINs.

To restore from an old backup, you must provide the old device pin. If
you've lost the old device pin, I believe there's no recovery mechanism
and you cannot restore from the backup.

## Data Export/Archive

Android backup (including messages) is deleted after 57 days of device
inactivity. Surprising! It isn't really a long term solution for
archival purposes. Restoration is primarily through Android device setup
process.

There are some tools (SMS Backup & Restore, SMS Import/Export) for
export of SMS/MMS, but Android puts the RCS messages in a *special*
database which I believe is more encrypted. I believe these tools *may*
be able to retrieve these messages but it's not entirely clear.

There *is* a means to get the db (`bugle_db`) which includes evens the
RCS conversation history. However, you need *root* to do this, and that
requires unlocking the bootloader which typically wipes the phone.

BTW, transfer of message data to iPhone (if you buy a new one, switch
back) appears possible. But there is confusion about whether only
SMS/MMS gets transferred, or also RCS conversation history. Worse, it's
particularly unclear whether you can import Google Messages history to
iPhone Messages *if you already have Apple Messages history*! What a
mess!

# iMessage

As of 2026, I am transitioning away from iMessage as I use Google
Messages on my Pixel more. I have between 10-15yrs of conversation
history in iMessage as of 2026.

## Identity, E2E

There are three levels of identity. First, is your Apple Account.
Second, are your phone numbers and emails registered into iMessage. You
use an SMS/email challenge to prove you control the number/email. Third,
are *devices* (iPhone and Mac Book, for instance).

iPhone verifies the phone number associated with the SIM card almost
invisibly to the user, so you might not even notice that a phone number
gets published into your Apple identity.

To reach you, users can message any number/email. You can of course use
the system with *no* phone number and only email. You can select a
"default" address to start iMessage conversations from. To select what
address is used for a correspondent, you might have to start a new
thread and select the desired address. The Messages app normally merges
the two conversations.

Each device gets its own keypair. Sending a message on iMessage requests
the list of devices from Apple Identity Service and encrypts for each
device. This is like WhatsApp.

Messages sent via the iMessage service are queued for up to 30 days, and
then deleted.

For RCS, E2E is used when mutually enabled/supported. As always, SMS/MMS
are not E2E encrypted.

## Contact Key Verification

Users are *not* by default notified of added E2E keys. So an attacker
(of the Apple ID service) could add a device and start receiving your
messages.

In 2023 with iOS 17.2 Apple added "Contact Key Verification". (I checked
and it is enabled for me as of 2026). A top-level keypair is associated
with the Apple account. It is used to sign new device keys.

If both users are enrolled, you are warned if a correspondent's device
is added without the proper signature. You are not warned if a device is
added with a proper signature. This is to reduce noise. Note that this
is *different* from WhatsApp.

You may also *manually* verify the correspondent's top-level account
key. If this is done, the correspondent's account key is *pinned*. You
will be notified of *any* change to the account key, even one initiated
in good faith by the user or Apple Corp.

Last, there is something called *key transparency*. The idea is that
there is a publicly auditable log of account key changes. An attacker
cannot merely compromise an Apple ID server and report a new account
key. They must also compromise the log. The log is *also* controlled by
Apple, but it is presumably designed to be harder to attack, and
resistant to a single instance being compromised.

Ultimately, the government can compel Apple to publish a new account key
into the KT log. Your *own* device consumes the KT log though, and can
warn you of the change. But your correspondents might not be warned; the
change could look legitimate.

**iCloud Keychain**

Your account level private key used for CKV is saved in the iCloud
keychain. When you authorize devices (like iPhone or Mac) to Apple,
those device identities are registered. Those devices can decrypt the
iCloud keychain. This works via a private key stored in the secure
enclave. Use of an authorized device is the most reliable way to recover
the keychain.

If you lose all your trusted devices, you can *still* recover the iCloud
keychain. There is a secure, strong password generated by Apple and
stored on special hardware (similar to Google's Titan). I believe Apple
calls this "Escrow security." This password is encrypted with an iPhone
PIN or a Mac password. Retries are limited (to protect against low
entropy PIN or password); eventually the stored password is destroyed.

It *isn't* documented what PIN or password might be requested. It could
possibly be a PIN you set up on old device, even one deregistered from
your Apple Account! There have been documented cases of this.

I believe that you *must* enable iCloud passwords sync in order to
participate in CKV. So it is important to enroll in iCloud passwords on
each new Apple Device you own.

## SIM (or email) Stealing

If someone steals your phone number, they can re-register it with a
different Apple account. Nothing looks wrong from a CKV or KT
perspective. This can look like a simple phone number recycling. There
is no two-step PIN to slow phone number re-registration similar to
WhatsApp.

CKV/KT surfaces *no error or warning* to correspondents. They can be
messaging an entirely new account and not realize it.

The *only* way to be reliably warned is to use manual verification.

The model is sort of the opposite of WhatsApp: avoid *all* spurious
warnings (from new device registration), unless you manually enroll in
the highest level of protection.

For legitimate phone number changes, there isn't an automated way to
"push" correspondents to use your new, preferred number/email.

## SMS, MMS, RCS

"Blue bubble" messaging is when iMessage uses its own transport network.
You can message either phone number of email identities through the
iMessage network.

If, for some reason, iMessage network delivery fails, you can fall back
to SMS/MMS/RCS. This only works for phone numbers (of course). Apple
relies on your device being a gateway to the cellular carrier (similar
to Google Messages). For fallback to work, you must have your iPhone on
and be connected on internet or cell network. Likewise for *receiving*
SMS/MMS/RCS and displaying it on another device.

Messages on a Mac desktop/laptop can send SMS/MMS/RCS to phone numbers
via the cellphone gateway. The cell doesn't need to be on the local
network, but it does need to be on.

This is pretty similar to how Google Messages works, except that with
iMessage you can *also* send through the iMessage network even without a
phone.

## International Travel

Messages sent through iMessage to your email address will work no matter
where you are in the world. They can be received through internet (WiFi
or cell data). It might be most convenient if correspondent's select
your email as the recipient address in iMessage.

You can request they do this before you travel, but there is no way to
*force* correspondent's to use your email.

Back in the days of physical SIMs, if you removed your SIM card from
your phone, iMessage continues to associate your phone number to your
account for up to 30 days. Of course, another account can claim your
phone number if they complete the challenge (e.g. if number is
recycled).

For some time, with eSIMs, iMessage would immediately de-register your
phone number if you deactivate the eSIM. That was a problem when I
traveled to Japan and deactivated my eSIM to avoid roaming charges. I
couldn't receive any iMessage sent to my phone number. iMessage to your
email address obviously continues to work; in Japan A and I messaged
each other this way.

As discussed previously, probably the best play is to turn off data
roaming, but keep the SIM otherwise activated. Avoid phone calls, hope
that you don't receive too many $0.25/msg MMS messages. You'll continue
to receive $0.05/msg SMS, which is actually useful for SMS two factor
challenges.

As of iOS 26, you don't get immediately deregistered when deactivating
an eSIM. It appears that as long as you retain the eSIM in a deactivated
state, you can hold onto the phone number registration indefinitely.

## No App Lock/Chat Lock Equivalent

There is no way to specially encrypt/restrict/hide conversations in
iMessage. You *can* suppress *notifications* from specific
conversations.

On iPhone, you can require FaceID to open the Messages app. But you
can't on MacOS.

## Messages in iCloud

You can turn on "Messages in iCloud" which will sync messages across
devices. If this is off, your messages are only stored in a local DB
(which could still be synced if device iCloud backup is enabled). If
Messages in iCloud is enabled, messages are not included in device
iCloud backup. There is little reason *not* to turn on "Messages in
iCloud". Reasons not to might include (1) privacy, not wanting to upload
data to iCloud, (2) storage costs especially for media. An edge case is
that you might want to delete messages on one device but not another.

The Messages in iCloud are encrypted with a "message key." Each device
gets a copy of the message key. The enrolled devices have access to the
message key because it is stored in the *iCloud keychain*. As discussed,
each device enrolled in your account can access this keychain.

But there is a (major) wrinkle. Independent of "Messages in iCloud",
your iPhone can enable "iCloud backup". This stores an image of your
phone for recovery or migration to a new iPhone. But it *also* stores
the message key. And Apple can access the iCloud backups.

I believe the thinking is: if the user loses their iPhone, it may be
their only Apple device. If they also can't remember their PIN, then
they can't get access to the iCloud keychain, and thus can't get the
message key. So, to help the user, they put the message key in a device
backup.

Thus, we get to Apple **Advanced Data Protection** (ADP). Basically, the
encryption key used for iCloud (including iCloud backup) is moved into
the iCloud Keychain. And this can be accessed either by an enrolled
device, or your recovery key, or an old device password/PIN (protected
by hardware against retries).

## Data Export

On a Mac, you can access `~/Library/Messages/chat.db`, which is a copy
of your message history. Apple doesn't promise that `chat.db` is the
complete history (maybe some messages are stored only in cloud and
fetched on need), but in practice it appears to be complete.

The other question is *attachments*. These are stored in
`~/Library/Messages/Attachments/`. This is definitely not promised to be
complete, because media files can take up lots of space. It is basically
a local cache, with references to media stored in `chat.db`, and fetched
when needed.

# Signal

These notes are brief/superficial because I haven't/don't really use
Signal as of 2026.

As of 2026 registration is via phone number, but they appear to be
experimenting with no-phone-number registration. Phone numbers prevented
spam, but also made it easy to find your friends by phone number lookup.
No phone registration may cost $2.99 to prevent abuse. Signal tries hard
not to be able to associate an account with a payment (right now this is
done via Google Play in-app system). But security researchers do feel
you could still associate the payment with the account, *if* you know
the google play account *and* the account.

You can communicate via username, without revealing your phone number to
the counterparty. Presumably they are associated on Signal's servers.

But very little data is associated with the Signal account. They don't
know your real name (cell company may though). They don't have your
contacts or chat partners. Of course they don't have your message
contents.

In fact, given a cell number, Signal cannot look up a username. Given a
username, it *can* lookup the phone number/account. But once username is
removed from an account, Signal deletes that record.

Messages are E2E encrypted. Recipient must be known for delivery, but
sender is "sealed" so that service can't know who is sending the
message. This is part of reducing metadata collection.

Strong notifications by default when a correspondent's identity changes.

Disappearing messages is offered as a routine setting. This can be
disrespected on the other side (for instance, you can just take a photo
of a message). But in practice it can prevent a lot of routine
accumulation of sensitive information on correspondents' devices. They
could even build a version of the open source client that strips out the
deletion of messages sent disappearing style. Ultimately you trust the
correspondent to honor your deletion request.

There is a registration PIN to slow down someone who has stolen your
phone number (similar to WhatsApp two step PIN).

Signal distributes source, and you can even build from source and
participate on the network. Source can be audited. Of course, you can't
know that they actually run the software on their servers... But how
could you?

Backups are not enabled by default, but if you use them encryption is
not optional.

# TODOs

- Are there any cross-client import capabilities? For instance from
  iMessage to Google Messages? Would I want that? Probably creates a
  mess (especially if you switch back and try to re-import).
