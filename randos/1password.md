The first thing to note is that 1Password 8 no longers allows you to
store the passwords in Dropbox. I believe 1Password 8 was released in
Aug 2021.

- Link: https://news.ycombinator.com/item?id=28145247
  - HN article describing how 1Password 8 will not support local vaults.

You may still be able to use 1Password 7, which I think still receives
security updates, but not feature updates (as of May 2023). But I
upgraded to 1Password 8.

So where are they stored? They are stored in the 1Password cloud. How
does that work?

You login using three things:

1. Email
2. Secret key (previously known as "account key")
3. Master Password

A couple thoughts:

- Do you need to maintain control over your email in order to access
  your account? Probably eventually for billing invoicing purposes? But
  it looks like you can log in simply listing your email as usual, and
  it's fine.
  - Per an email with them, it appears no, you do not need to maintain
    control over your email?
- Master password is just your usual master password used to open the
  app.
- What is the secret key?
  - It's a randomly generated 128 bit key. It's used to login to the
    1Password server.
  - It's important for them so that people don't brute force at the
    server. This should be basically uncrackable.
  - Using your master password just unlocks the secret key stored in the
    client, I think.
    - So the secret key is stored on your devices. It is encrypted with
      your master password.
    - Can people crack it directly then? Not exactly: after testing a
      master password, I believe they'll get a decrypted secret key, but
      this could be wrong if the wrong master password was used.
    - They will then need to try to decrypt the vault with the secret
      key. Only if that is successful will they know if the password has
      been cracked.
    - That adds at least one step. Maybe a quite slow one.
    - Also: the secret key is never stored on the server. So even though
      they can steal the _vault_ from 1Password, they can't steal the
      secret key, which means the vault should be basically impossible
      to decrypt.
  - Link: https://support.1password.com/secret-key-security

## Security Model

The 1Password server stores all your passwords. I don't love that.

They say they never see nor store your master password or your secret
key.

Since your password vault is encrypted by a key derived from both of
these, people should not be able to decrypt it.

But they do need to do some authentication in order to know to send you
the (encrypted) vault. They do that using SRP (secure remote password).
That basically lets you authenticate without sending your password over
the network. Note this is even better than just TLS alone; the server
never even sees the password, and the password is never transmitted over
the (encrypted) channel.

However, the verifier for SRP is stored on the server, and this can be
used to test whether a password is valid. That's why the verification
uses not only a user password, but also an uncrackable random 128bit
secret key, which are combined to derive the verifier key.

**Vaults**

Looks like vaults are encrypted with AES. The key is encrypted with a
public key. The private key is encrypted with the key encryption key
(KEK) derived from the master password and secret key.

They say that they never have access to your private key.

## Backups

Okay - what if 1Password fucks up and loses all data?

Remember that every 1Password client has a full copy of the vault.
That's how 1Password can work _offline_.

Source: https://www.reddit.com/r/1Password/comments/tjldd6/how_to_create_a_local_backup_of_my_vault/

But here is my question: what happens if I forget to pay 1Password? Or I
cancel my subscription? Can I still access my vault in the 1Password
cloud? Can I still access my local copy of the vault? Does my copy of
the 1Password app just get crippled?

## Account Freezing

- 1Password will let you access your account even if you stop paying
  them. Your account is in "freezed" mode.
- You can still get your data our of 1Password through either the app or
  their website. You can export data as usual.
- You just can't add new data to a vault.
- They claim that they will _never_ delete a freezed account!
- Source: I contacted 1Password directly to ask them about this.

## Account loss?

- No one can ever reset your password on you. So that's good! Without
  the password, they can never take your account!
- I believe that if your email account is hijacked, that means nothing
  to 1Password. Control of email is not sufficient to reset your account
  password (1Password can't do that).
- As far as I can tell, you can still log into 1Password without the
  email address.
  - Source: 1Password appears to have verified this in an email I sent
    to them May 2023.

## Device Stolen?

- They do possess your encrypted secret key, I believe.
- I believe it should still be hard to crack if your master password is
  secure and contains enough entropy.
  - In fact, I believe that the white paper suggests that the secret key
    is stored in the device password manager, so hopefully remains
    secure.
  - But it is considered "lightly obfuscated." "it should be assumed
    that an attacker who gains read access to the user’s disk will acquire
    the Secret Key."
- They clearly say that the secret key is used to protect the vault
  stored on 1Password's server, if compromised. But if the device is
  compromised, it looks like security absolutely comes down to the
  strength of the password.
  - Source: https://1passwordstatic.com/files/security/1password-white-paper.pdf
- So, if your device is stolen (or someone gains read access to your
  disk), the only thing protecting you is the strength of this password.
  - They actually don't recommend changing the password if you lose your
    device.
  - They _do_ suggest changing the secret key. That will keep the data
    stored on the server secure.
- Cracking the password is slow; they use hashing in the key-derivation
  method that derives the vault key from the (1) account password and
  (2) secret key.
- But ultimately, it comes down to password strength. If you have a
  stolen device, and a weak password, it can be cracked.
- If they do that, they can steal all the passwords in the vault.
- They _can't_ use the cracked account password to access the online
  service, since they won't have the reset secret key.
- Okay. But if you _don't_ change your account password, doesn't that
  mean that if the attacker gains a _second_ device which uses the
  updated secret-key, they know the exact account password needed to
  open the vault?
- It seems unwise not to update the account password in that case.
- I did email them about this but I'm not sure the answer I got was very
  clear.
- But anyway: if you have a weak passphrase, then sure they can crack it
  on one stolen device, or they can crack it on the second device they
  steal. If your password is good, the fact that they can start cracking
  it today doesn't really matter. They could wait until they get your
  new device and start cracking it.

## TODO

- Look, there are some holes in my understanding of 1Password's entire
  security model.
