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

# iMessage

- **TODO**: I must review this in 2026!!
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
  - The behavior changed *again* with **iOS 26 2025-09-15**. It appears
    that you won't get immediately logged out of iMessage anymore when
    turning off an eSIM. Notably, this change came after our trip to
    Japan.
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



# TODOs

- Review Google Messages (using on Android in 2026)
- Review iMessage (have used for ~15yrs as of 2026).
- Are there any cross-client import capabilities? For instance from
  iMessage to Google Messages? Would I want that?
- Consider Signal.
