# attic-schedule [![Build Status](https://travis-ci.org/passy/attic-schedule.svg?branch=master)](https://travis-ci.org/passy/attic-schedule)

> A script I use to trigger [attic](https://attic-backup.org/) to backup my
> stuff to my local NAS.

This is a bash script I converted to
Haskell/[Turtle](http://haddock.stackage.org/lts-3.11/turtle-1.2.2/Turtle.html)
so I can trust it to do what it should. It's supposed to work for my machine.
Sadly, my machine isn't yours so this probably won't work for you without some
large(-ish) modifications.

Why would I publish it if it's useless to most people? Well, I kinda like having
all my backup scripts backed up on GitHub and whenever I write scripts like
this and search for Turtle code snippets on GitHub, I wish more people
would put there stuff there so you see some real-world examples of the APIs -
whether or not the code is actually any good.

## Installation

From the checked out repository:

```bash
stack install
```

From Hackage:

```bash
stack install attic-schedule
```

I also want to mount my NAS backup directory as user, so I add this here to
`/etc/sudoers.d/override`:

```
Cmnd_Alias MOUNT_REMOTEBACKUP = /usr/bin/mount /mnt/remotebackup

pascal ALL=(ALL) NOPASSWD: MOUNT_REMOTEBACKUP
```

If your name isn't `pascal`, you should a) blame your parents and b) change
the line above.

I mount the above via SMB, so my `fstab` entry looks something like this:

```
//passystation0/backup  /mnt/remotebackup/      cifs    rw,vers=1.0,cache=strict,username=backup,password=hahanotgonnatellya,domain=WORKGROUP,addr=fdbf:11c6:f107::9e4,posixpaths,serverino,acl,rsize=1048576,wsize=1048576,actimeo=1,noauto,user
```

## Usage

There's one initial setup step, you need to manually invoke with `attic`, which is
creating your attic repository. For the example below, you'd have to invoke:

```
attic create /mnt/remotebackup/pascal-projects.attic --encryption=keyfile
attic create /mnt/remotebackup/pascal-docs.attic --encryption=keyfile
```

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

### Crontab

You will also want to make this part of your crontab unless you really enjoy
typing long commands. (By the way, if you know how to do this with systemd
timers, please let me know, I actually had to install cronie first.)

```crontab
0   *   *   *   *   ionice -n3 -- attic-schedule -d /mnt/remotebackup -s ~/Projects/ -n pascal-project
5   *   *   *   *   ionice -n3 -- attic-schedule -d /mnt/remotebackup -s ~/Documents/ -n pascal-docs
```

(Don't forget the `ionice` even if it the network is most likely enough of a
bottleneck to not impact your system's I/O perf.)

## License

BSD-3
