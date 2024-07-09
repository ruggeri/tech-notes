## Generating a Key

You can generate an SSH key like this:

    ssh-keygen -t ed25519 -C "your_email@example.com"

I think ed25519 is preferred over RSA nowadays. The "-C" flag lets you
give a "comment" that describes the purpose of the key. You can specify
a file-name with "-f"; that is typically helpful to know what this key
is for!

You will be given an option to use a passphrase to encrypt the key on
disk.

SSH will look for keys to use to login to servers in `./ssh`. The files
come in pairs: a private version like `.ssh/id_rsa` and a public version
like `.ssh/id_rsa.pub`.

## known_hosts

Each time you use SSH to connect to a new domain name, you will be asked
to confirm a public key. This prevents a MITM attack against you. You
should ideally verify the public key. Regardless, if the server key ever
changes (for instance, if the domain is compromised and a MITM attack is
run), SSH will you will notify you.

This is basically a personal certificate authority. It is stored in
`./ssh/known_hosts`.

## Host and Hostname

When you SSH, you typically specify at least a user and a hostname. For
instance:

    ssh admin@ec2-xx-xxx-xx-xx.us-west-2.compute.amazonaws.com

Here, `admin` is the user, and `ec2-...` is the hostname. You will do a
DNS resolve on this hostname to find the server to connect to.

Almost. `ec2-...` is actually what SSH calls a "Host." If the host is
not configured in `.ssh/config`, it will be used as the hostname, which
is the hostname that will be resolved to identify the IP to connect to.

However, you can also configure something like this:

```
Host host1
    HostName host1.example.com
```

Now, you may simply execute `ssh user@host1` to login to the machine at
`host1.example.com`.

## Selecting Key To SSH With

You may generate multiple public keys and give them to different
servers. You can specify a public key to use like this:

    ssh -i ~/.ssh/id_rsa user@host

The defaults include `.ssh/id_rsa` or `~/.ssh/id_ed25519`. I believe you
can use multiple keys. You give the address to the _private_ key.

You can use `.ssh/config` to set up a host so that it uses a specific
key by default:

```
Host gitserv
    Hostname remote.server.com
    IdentityFile ~/.ssh/id_rsa.github
```

Now when you `ssh user@gitserv`, the `~/.ssh/id_rsa.github` will be used
by default to connect to the SSH host at `remote.server.com`.

## SSH Keys and Github

Let us say you have multiple Github accounts. Thus you have multiple
public keys. You will add something like this to `.ssh/config`:

```
Host second-github-account-name
    HostName somewhere.com
    IdentityFile ~/.ssh/id_ed25519.second_github_account_name
```

Note that `second-github-account-name` can really be whatever you like.
Of course, the IdentityFile can also be whatever key you like. Nothing
has to match the actual name of the Github account.

How will git know to use this SSH host when connecting? You must set the
remote properly. First, note that when you `git clone
https://somewhere.com/repo.git`, git will generate a remote like this:

    git@somewhere.com:repo.git

The default username is `git`, and the host is chosen to be the domain
name. However, you may change this:

    git remote set-url main git@second-github-account-name:repo.git

Now when interacting with the `main` remote, git will SSH with a host of
`second-github-account-name`. This will still connect to
`somewhere.com`, but the identity used will be as specified in
`.ssh/config`.

## Sources

- Source: https://docs.github.com/en/authentication/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent
- Source: https://stackoverflow.com/questions/33243393/what-is-actually-in-known-hosts
- Source: https://stackoverflow.com/questions/4565700/how-to-specify-the-private-ssh-key-to-use-when-executing-shell-command-on-git
- Source: https://superuser.com/questions/503687/whats-the-difference-between-host-and-hostname-in-ssh-config
