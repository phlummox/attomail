# attomail [![Hackage version](https://img.shields.io/hackage/v/attomail.svg?label=Hackage)](https://hackage.haskell.org/package/attomail)

Minimal mail delivery agent (MDA) for local mail with maildir support - a Haskell re-implementation of femtomail

## quick installation

~~~
$ stack install attomail && sudo cp ~/.local/bin/attomail /usr/local/bin
$ cat << EOF | sudo tee -a /etc/attomail.conf
mailDir = /path/to/my/home/dir/Maildir/new 
userName = myuserid
EOF
$ mkdir -p /path/to/my/home/dir/Maildir/new
~~~

## prerequisites

- Haskell 
- unlikely to work properly on anything but a Linux system

## purpose

Acts as a minimal (local delivery only, many features un-implemented) mail
delivery agent, or MDA, delivering mail to a local `maildir` format spool.
Handy when you don't want to install an MTA (Mail Transfer Agent) or
fuller-featured MDA - you just want a program which accepts 
`sendmail`-style delivery of messages from local programs, and dumps them
somewhere you can read them. 

It is a port of [femtomail](<https://git.lekensteyn.nl/femtomail/>) to Haskell.
(See this [StackExchange](http://unix.stackexchange.com/questions/82093/minimal-mta-that-delivers-mail-locally-for-cron) posting for femtomail's inception.)

## configuration

By default, uses `/etc/attoparse.conf` as a configuration file.

`/etc/attoparse.conf` needs to contain two lines, specifying the path to
a directory where messages should be delivered, and the userid of the
person who owns that directory.

e.g.:

~~~
mailDir = /path/to/my/home/dir/Maildir/new 
userName = myuserid
~~~

So that other programs can find it, you probably want to install
`attomail` somewhere on the system path - e.g. in `/usr/local/bin/attomail`. 

`attomail` needs to be either run by the user specified in the config file, or
(more likely, if being called by, say, some cron job) root
(or some other account
with permission to change uid, etc.).
This is because it makes use of the `setresgid` and `setresuid` C functions to change its uid etc to that user, and ability to do that is normally restricted.

To use some other location for the configuration file, define CONF_PATH as a
macro when running ghc.

e.g.:

~~~   
stack build --ghc-options -optP-DCONF_PATH=/some/dir/my.conf 
~~~

or

~~~
cabal build --ghc-option=-optP-DCONF_PATH=/some/dir/tmp.conf  
~~~

No claims that this program is at all secure, use at your own risk.

## command-line arguments

Usage: 

*   `attomail [-f ADDRESS] [-F NAME] [-b MODE] [-i] [-o ARG] [-O ARG] [-B ARG]
    [-q ARG] [-v] RECIPIENTS...`

Arguments that are actually processed:

      -bm         Read input from stdin, deliver mail in the usual way (default).
      -Ffullname  Set the full name of the sender.
      -fname      Sets the name of the `from' person (i.e., the sender of the mail put on the enevelope)

Various ignored options, included only for compatibility with `sendmail`: `-i`, `-o`, `-O`, `-B`, `-q`, `-v`.

## testing 

-   If you have a `mail` program installed, just use that for testing the
    installation. Messages to any address at all, local or remote, should go
    to the mail spool specified.

-   Alternatively:

    ~~~
    $ cat | sendmail a@b.com << EOF 
    To: someone@somewhere
    Subject: mysubject
    
    some body
    EOF
    ~~~

    *`<ctrl-d>`*

## portability

Probably won't work on anything but Linux systems.

## API

None, yet, there's only an executable, not a library. But (*sssh*) take a peek
[here](https://hackage.haskell.org/package/attomail-0.1.0.2/docs) if you like, there should be some minimal documentation of the internal modules.

