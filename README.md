# attic-schedule

> A script I use to trigger [attic](https://attic-backup.org/) to backup my
> stuff to my local NAS.

A bash script I converted to Haskell/[Turtle](http://haddock.stackage.org/lts-3.11/turtle-1.2.2/Turtle.html)
so I can trust it to do what it should. It's supposed to work for my machine.
Sadly, my machine isn't yours so this probably won't work for you without
some larger modifications.

Why would I publish it if it's useless to most people? Well, I kinda like having
all my backup script backed up on GitHub and whenever I write scripts like
this and search for Turtle code snippets on GitHub, I wish more people
would put there stuff there so you see some real-world examples of the APIs -
whether or not the code is actually any good.

## Installation

```bash
stack install
```

## Usage

This is meant to be used in combination with a crontab which runs every hour or
or so. It mounts the partition via CIFS and if it doesn't find a backup that's
been created within the last 24h, it'll trigger a `attic create` to create a new
one.

```bash
attic-schedule -- -d /mnt/remotebackup -s ~/Documents/ -n documents
```

This will create a new tag with the name
`/mnt/remotebackup/pascal-documents.attic::2015-11-01-21h` unless there's another
backup created on the 11th of November 2015.

## License

BSD-3
