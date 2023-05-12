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
- Master password is just your usual master password used to open the
  app.
- What is the secret key?
  - It's a randomly generated 128 bit key. It's used to login to the
    1Password server.
  - It's important for them so that people don't brute force at the
    server. This should be basically uncrackable.
  - Using your master password just unlocks the secret key stored in the
    client, I think.
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

## TODO

- Look, there are some holes in my understanding of 1Password's entire
  security model.
- I would like to know what happens if my subscription lapses.
- I would like to know what happens if my email is lost.
