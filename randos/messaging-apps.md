## WhatsApp

- Interesting model. No username. No password. No email (by default).
- When you log in you give a phone number, they send you a code by text
  to authenticate that you control the phone number.
- You typically never log out of the phone app.
- I believe that on authentication, a public/private key pair is
  established on device. The public key is uploaded to WhatsApp.
- When a sender wants to send a message, it encrypts the message with
  the public key.
- WhatsApp stores the encrypted message on the server. It can't view the
  message contents. The message is eventually transmitted to the client
  (and deleted on send). Here it can be read and stored.
- If you lose the phone, you cannot restore the conversation history
  from the WhatsApp servers, because they don't store the messages.
  Also, you will not be able to regenerate the secret key.
- When you lose the phone, if the thief can open WhatsApp they can
  send/receive messages as you.
- One way to stop them is to use a WhatsApp PIN. This simply exists
  locally to prevent opening the app on the phone.
  - You can add an email address to allow PIN recovery. If you forget
    your PIN, you can request to be sent a new one. Then you can log in
    using this PIN.
  - The simplest way to achieve this is for the client to bypass the PIN
    if the WhatsApp server tells the client to do so.
  - If someone is able to access both your email and your phone, they
    can still read your WhatsApp messages.
  - So on device theft tell your email server to log out all sessions to
    prevent thief from getting PIN.
- When you add an email, you can also use this for account recovery.
  - An email can be sent to you, allowing you to re-register a new
    device, even if you can't receive SMS.
  - I guess that might matter if you are roaming and can't receive
    texts.
  - Otherwise I feel like it increases attack surface area because if
    someone compromises your email they can register a new client.
  - I presume that if you setup a new client a new public/private
    keypair is generated and uploaded. This should be detectable.
- How to handle change of phone number
  - You can tell the WhatsApp client that you are changing your phone
    number.
  - This tears down your old account (under the old phone number) and
    creates a new one.
  - I believe your contacts will need to update their contact info for
    you, otherwise they will be talking to no one - or even the new
    owner of the old phone number.
  - I believe there is an option to notify contacts. Indeed, it looks
    like A initially registered WhatsApp using a Russian phone number,
    but then she changed her WhatsApp phone number to her Miami number,
    which sent me a message about the change and suggested I start
    messaging the new number (though did not merge threads). Weirdly, I
    see a new user's photo next to the old thread.
- Phone Number Recycling
  - If people newly re-register to WhatsApp, I believe it tears down the
    old account and instantiates a new one.
  - The new user will not see any of the old messages. First, those were
    never stored. Second, they are encrypted for receipt by your device;
    they can't even decrypt them.
  - I do believe your contacts could start talking to this new person
    unwittingly, if they do not have the security code change
    notifications turned on.
  - Actually, it only tears down the old account if the account goes
    unused for 45 days and then is re-activated on a new device.
    Otherwise, it will keep the old profile photo and group chat
    subscriptions.
  - There must be more to the PIN, because if you have a PIN setup then
    it takes 7 days (since last communication to WhatsApp from old
    client) before someone new with your phone number can re-register
    the phone number with WhatsApp. Presumably that's meant to slow down
    people from stealing your phone number and receiving messages sent
    by dumb contacts who don't check the thread security code.
  - Does that mean the server has the PIN? Maybe it's as simple as
    having a flag of "PIN set" on the server and not allowing
    re-registrations quickly.
  - The PIN is required by the app every 7 days.
- I believe that WhatsApp doesn't care, after initial registration,
  whether you can receive SMS messages. Thus you can roam with WhatsApp
  and retain ability to send/receive messages. You could even give up
  your phone number and still use WhatsApp until it is recycled and
  someone new tries to use it.
- **TODO**: Multiple clients
  - You link a client (like a desktop or web client) by scanning a QR
    code.
  - I believe that the new client generates a new public/private
    keypair, and uploads an additional public key to the server.
  - Contacts now send multiple copies of each message, encrypted for
    each of the public keys to the server. These are fetched and
    decrypted by the associated respective clients.
- WhatsApp _does_ appear to have some means to migrate your chat history
  to a new device, or across devices. It can daily save an unencrypted
  (by default) backup to your iCloud account. You can even encrypt this
  backup with a password in case your iCloud gets compromised. You
  should be able to migrate this to an Android or new phone. I believe
  it will be auto-migrated for you if you upgrade your iPhone but remain
  logged into the same iCloud.
  - By default, it appears to save photos in the backup.
- You can set WhatsApp to require FaceID to unlock the app ("App Lock").
  You can use a feature called "Chat Lock" which just locks some
  individual chats. App Lock will by default use FaceID/iOS passcode. So
  will Chat Lock, though you can configure it to require a code selected
  just for this purpose, which feels more secure.

## iMessage

- I believe there are a lot of similarities to WhatsApp.
- One difference is that you can message phone numbers or emails.
  Presumably only phone numbers will fall back to SMS.
- The iPhone can verify that the phone number associated with the SIM
  card. So it can setup iMessage almost without you noticing. You don't
  need to receive an initial code to authenticate.
- One interesting feature is that if you create an Apple contact with
  multiple emails or phone numbers, all conversations with those
  numbers/emails will be merged into one conversation thread.
  - There is a way to select which of a contact's phone numbers/emails
    to text to from a thread.
- It appears that iMessage used to give a 30day grace period where you
  could still receive iMessages on the old number even with the physical
  SIM card removed.
  - However, with eSIMs, it appears to immediately de-register your
    phone number if you turn off the eSIM.
- Thus, if you travel internationally and turn off your home eSIM to
  avoid roaming charges, you will stop receiving iMessages sent to the
  home country phone number.
  - Thus it may be preferable to use a network that has no roaming
    charges, even if it offers no roaming voice or data connectivity.
    Then you don't have to deactivate an eSIM and your phone number is
    not de-registered from iMessage.
- You could request contacts start messaging your email address. But it
  appears there is no way on your end to push them toward a new contact
  method, except manually asking.
- iMessage will queue undelivered messages for up to 30 days, then
  delete them. So if you are traveling you should eventually get the
  messages.
- A and I communicated via iMessage while traveling by using our emails
  as the contact recipient.
