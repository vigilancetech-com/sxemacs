#!/usr/bin/env bash
#
# A script to setup your git area to contribute back to SXEmacs
#
# Copyright (C) 2008 Nelson Ferreira
# Copyright (C) 2015 Steve Youngs
#
# This program is free software; you can redistribute it and/or modify it
# under a BSD-like licence.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# Redistributions of source code must retain the above copyright notice, this
# list of conditions and the following disclaimer.
# Redistributions in binary form must reproduce the above copyright notice,
# this list of conditions and the following disclaimer in the documentation
# and/or other materials provided with the distribution.
# Neither the name of the Technical University of Berlin nor the names of its
# contributors may be used to endorse or promote products derived from this
# software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#

### Commentary:
##
##    This script sets up your SXEmacs git WD so that contributing
##    back to the SXEmacs project is as easy as possible, in the way
##    that the maintainers like best. :-)
##
##    Much care has been taken to NOT overwrite your current settings.
##    Most times, a setting will only be set if it is not already set,
##    and you will be prompted before any changes are made, giving you
##    the option to bail out.
##
##    Before the script does anything it checks to make sure you are
##    running it in a SXEmacs source tree (bails out, if not).
##    And if you have a dirty WD (uncommitted changes) the script will
##    stash them first, just to be safe (they're restored at the end).
##
##    The stuff this script looks at and might sets/changes:
##
##      user.name, user.email, user.signingKey, commit.gpgSign,
##      format.numbered, format.to, format.subjectprefix,
##      format.headers, sendmail.to, sendmail.from
##
##    It also ensures that origin points to the right place
##    (https://git.sxemacs.org/sxemacs), optionally makes sure your
##    remote is set up correctly, that you have a "for-steve" tracking
##    branch, and adds a devkey.$INITIALS tag containing your public
##    GnuPG key if available, which can be pushed to your remote.
##
##    Oh, and it adds a swag of nice aliases.
##
LETSPOP=0
cat<<EOF
**[Welcome]***********************************************************
|               Hello, and welcome to the SXEmacs Team               |
|                                                                    |
| This script will help guide you through setting up your personal   |
| SXEmacs git repo in a way that will make your life as a developer, |
| and contributor, as easy as possible.                              |
|                                                                    |
| We just make sure a few basics are set like name, email, the right |
| tracking branch, remotes, etc, and add a few nice aliases and      |
| config options.  You will be prompted if we need to make any       |
| changes to your config, and you can bail out at any time with C-c. |
|--------------------------------------------------------------------|
| I hope you get immense fun and satisfaction from hacking SXEmacs   |
|      Steve Youngs <steve@sxemacs.org> "SteveYoungs" on IRC         |
**********************************************************************
                            Hit [RETURN] to continue, or C-c to abort.
EOF
read junk

# Work in the toplevel dir just to be on the safe side
pushd $(git rev-parse --show-toplevel) 1>/dev/null || 
    ( echo 1>&2 "Please run this script from _inside_ your SXEmacs repo."
	exit 1 )

# Lets not mess about in anything that isn't a SXEmacs repo
if [ "$(git show-ref v22.1.13 2>/dev/null|cut -c-8)" != "5ff9eebe" ]; then
    echo 1>&2 "This is NOT a SXEmacs repo, bailing out!"
    exit 1
fi


# Save the current branch name in case we move about
CURRENT_BRANCH=$(git rev-parse --abbrev-ref HEAD)

## Clean WD
clear_wd()
{
    cat<<EOF

**[Dirty Tree]********************************************************
Your working directory contains changes that have not been committed
yet.  Under certain conditions this script may do a couple of branch
jumps, so we will play it safe and store your changes out of the way
with 'git stash'.  We will 'pop' them back again when we are all done
here.
**********************************************************************
                            Hit [RETURN] to continue, or C-c to abort.
EOF
    read junk
    git stash save --all --quiet "git-for-steve.sh safety stash"
    LETSPOP=1
    echo
}
DIRTY=$(git status -u --ignored --porcelain -z)
[ -n "${DIRTY}" ] && clear_wd

## Name
have_noname()
{
    echo
       echo "Please enter your name as you would like it to appear in the logs"
    echo -n 'Protected with quotes (e.g. "John Doe"): '
    read NAME
}

set_name()
{
    [ $USER ] && NAMEGUESS=$(getent passwd $USER|cut -d: -f5)

    if [ -n "${NAMEGUESS}" ]; then
	echo
	echo "Git needs to know your name (for commit logs, etc)."
	echo -n "Can we use \"${NAMEGUESS}\"? [Y/n]: "
	read RESP
	if [ "${RESP}" = "Y" -o "${RESP}" = "y" -o "${RESP}" = "" ]; then
	    NAME=${NAMEGUESS}
	else
	    have_noname
	fi
    else
	have_noname
    fi

    git config user.name "${NAME}"
}
[ -n "$(git config user.name)" ] || set_name

## Email
set_email()
{
    echo
    echo "Git needs to know your email address (for commit logs, etc)."
    echo -n "Please enter your email address (e.g. john@doe.com): "
    read EMAIL
    git config user.email ${EMAIL}
}
[ -n "$(git config user.email)" ] || set_email

## Tracking branch "for-steve"
# Make sure origin points to https://git.sxemacs.org/sxemacs
CHECK_ORIGIN=${CHECK_ORIGIN:-true}
if [ "${CHECK_ORIGIN}" != "false" ]; then
    ORIGIN_URL=$(git config remote.origin.url)
    if [ "${ORIGIN_URL}" != "https://git.sxemacs.org/sxemacs" ]; then
	cat<<EOF

**[Bad origin]********************************************************
WARNING: origin URL is WRONG.

 It is currently set to: ${ORIGIN_URL}
But it SHOULD be set to: https://git.sxemacs.org/sxemacs

It you are absolutely 110% sure that your origin is correct, abort now
with C-c, and re-run this script with the environment variable, 
"CHECK_ORIGIN" set explicitly to the string: "false".  Otherwise we
are going to reset it for you.
**********************************************************************
                            Hit [RETURN] to continue, or C-c to abort.
EOF
	read junk
	git remote set-url origin https://git.sxemacs.org/sxemacs
    fi
fi

set_branch()
{
    echo
    echo "**[Tracking branch]***************************************************"
    echo 'Setting up a "for-steve" branch to track origin/master.'
    echo
    echo "This is the branch that you will merge your work into once it is"
    echo "ready for Steve to pull into his repo."
    echo "**********************************************************************"
    echo -n "                            Hit [RETURN] to continue, or C-c to abort."
    read junk
    # Does it make a difference from where we do this from?  Lets jump
    # into master if we're not there already, just to be on the safe
    # side.
    [ "${CURRENT_BRANCH}" != "master" ] && git checkout --quiet master
    git branch --track for-steve origin/master
    # Offer to leave them in for-steve, but only if we didn't stash
    if [ ${LETSPOP} -eq 0 ]; then
	echo
	echo -n 'Switch to the "for-steve" branch when this script exits? [Y/n]: '
	read RESP
	if [ "${RESP}" = "Y" -o "${RESP}" = "y" -o "${RESP}" = "" ]; then
	    git checkout --quiet for-steve
	fi
    fi
}
git branch | grep -q for-steve || set_branch

## Remotes
# myremote
set_myremote()
{
    cat<<EOF

**[Remote]************************************************************
You need to have a remote repository set up for you to push your
changes to.  Steve will need read-only access so he can fetch your
changes into his repo.  You can name this repo anything you like,
just as long as it is a single word, and that word is not "origin".

A remote repo can have a "Fetch URL", and a "Push URL".  The
former (Fetch URL) is the URL from which people would clone, pull,
fetch from.  And the latter (Push URL) is the URL you would use to
push (or write) to.

The Push URL to your remote needs to be writable for you, here are a
couple of examples...

    ssh://user@example.com/~/path/to/repo       (using ssh)
    https://user:pass@example.com/path/to/repo  (using "smart http")
**********************************************************************
-- more --                  Hit [RETURN] to continue, or C-c to abort.
EOF
read junk

    cat<<EOF

**[Remote (cont)]*****************************************************
You _could_ use a git protocol URL (git://), but because the git
protocol has no authentication if you allow write access you are
allowing write access for anyone who has an internet connection.  So
PLEASE DO NOT write to your remote via the git protocol.

Before we go ahead and add a remote to your repo, lets see if you have
one already.
**********************************************************************
                            Hit [RETURN] to continue, or C-c to abort.
EOF
    read junk
    REMOTES=($(git remote | grep -v origin))
    echo
    echo "**[Remotes]***********************************************************"
    echo "          Currently configured remotes (possibly empty list)"
    echo
    for (( i = 0; i <= ${#REMOTES}; i++ )); do
	echo -en "\t"${i} -- ${REMOTES[${i}]}" at: "
	echo $(git config remote.${REMOTES[${i}]}.url)
    done
    echo
    echo "**********************************************************************"
    echo -n ' Enter the number that corresponds to your remote, or "x" for none: '
    read index

    if [ "${index}" = "x" -o "${index}" = "" ]; then
	echo
	echo -n 'Enter the "Fetch URL" (read-only access) to your remote: '
	read MYREMOTE_FETCH
	echo -n 'Enter the "Push URL" (write-access for you) to your remote: '
	read MYREMOTE_PUSH
	echo "And what would you like to call this remote?  It MUST be a single"
	echo -n 'word, and CANNOT be "origin": '
	read MYREMOTE_NAME
	git remote add ${MYREMOTE_NAME} ${MYREMOTE_FETCH}
	git remote set-url --push ${MYREMOTE_NAME} ${MYREMOTE_PUSH}
	git config sxemacs.remote ${MYREMOTE_NAME}
    else
	TYPE="X"
	URL=$(git config remote.${REMOTES[${index}]}.url)
	while [ "${TYPE}" != "F" -a "${TYPE}" != "P" -a "${TYPE}" != "B" ]; do
	    echo 'Is "'${URL}'"'
	    echo -n "  used for Fetch, Push, or Both? [F/P/B]: "
	    read TYPE
	    TYPE=$(echo ${TYPE}|tr 'fpb' 'FPB')
	done
	case ${TYPE} in
	    F)
	        echo -n ${REMOTES[${index}]} '"Push URL" (write-access for you): '
		read TYPEURL
		git remote set-url --push ${REMOTES[${index}]} ${TYPEURL}
		;;
	    P)
	        echo -n ${REMOTES[${index}]} '"Fetch URL" (read-only access): '
		read TYPEURL
		git remote set-url ${REMOTES[${index}]} ${TYPEURL}
		git remote set-url --push ${REMOTES[${index}]} ${URL}
		;;
	    B)  ;; # do nothing
	esac
	git config sxemacs.remote ${REMOTES[${index}]}
    fi
}
if [ -z "$(git config sxemacs.remote)" ]; then
    cat<<EOF

**[Public repo]*******************************************************
The easiest, and quickest way to get your work and changes into the
main SXEmacs repository is if you have a publicly accessible remote
repo.  Well, technically, it does not need to be publicly accessible,
it just needs to allow Steve read-only access.  Nobody, except you,
will ever need to write to this repo.
**********************************************************************
EOF
    echo -n "                   Do you have a remote repo like this set up? [Y/n]: "
    read RESP
    if [ "${RESP}" = "Y" -o "${RESP}" = "y" -o "${RESP}" = "" ]; then
	set_myremote
    else
	echo "If you ever do set up a remote repo, please re-run this script."
    fi
fi

## GnuPG
set_tagblob()
{
    for word in $(git config user.name); do
	SXEINITIALS="${SXEINITIALS}$(echo ${word}|cut -c1)"
    done
    INTLS=${INITIALS:-${SXEINITIALS}}
    TAGNAME=devkey.${INTLS}

    git tag|grep -q ${TAGNAME} && TAGEXISTS=yes
    if [ "${TAGEXISTS}" = "yes" ]; then
	echo
	echo "**[Existing devkey]***************************************************"
	echo "There is already a developer key tag using your initials..."
	echo
	git show ${TAGNAME}|sed -n 2,5p
	echo
	echo "**********************************************************************"
	echo -n "                                                  Is it yours? [Y/n]: "
	read RESP
	if [ "${RESP}" = "Y" -o "${RESP}" = "y" -o "${RESP}" = "" ]; then
	    git config sxemacs.devkey $(git show-ref ${TAGNAME}|awk '{print $1;}')
	else
	    echo -n "Setting developer key tagname to: "
	    echo "${TAGNAME}.$(git config user.signingKey)"
	    TAGNAME=${TAGNAME}.$(git config user.signingKey)
	fi
    fi

    if [ -z "$(git config sxemacs.devkey)" ]; then
	TAGMSG=$(cat<<EOF
Developer Key -- $(git config user.name)

This is the GnuPG key used by $(git config user.name) <$(git config user.email)>
to sign commits, merges, and tags in this repository.

You may import this key into your GnuPG keyring with...

  'git show ${TAGNAME} | gpg --import'

To verify signed objects in the repo, use the '--show-signature'
option with the git-log and git-show commands.

To verify this tag, use 'git tag -v ${TAGNAME}'

EOF
)
	git tag -s ${TAGNAME} -m "${TAGMSG}" \
	    $(gpg --armor --export $(git config user.signingKey) |
	    git hash-object -w --stdin)
	git config sxemacs.devkey $(git show-ref ${TAGNAME} | 
	    awk '{print $1;}')

	# If something went wrong here we should probably bail out
	if [ $? -gt 0 ]; then
	    echo 1>&2 'Something has gone horribly wrong while trying to add your devkey.'
	    echo 1>&2 'Please check that your local GnuPG settings are correct, and then'
	    echo 1>&2 'attempt to re-run this script.'
	    echo
	    echo 1>&2 'If the problem persists, get in contact with us and we will do our'
	    echo 1>&2 'best to help you resolve it.'
	    exit 1
	else
	    echo
	    echo "Your devkey tag has been created successfully."
	fi
	echo -n "Can we now push ${TAGNAME} to $(git config sxemacs.remote)? [Y/n]: "
	read RESP
	if [ "${RESP}" = "Y" -o "${RESP}" = "y" -o "${RESP}" = "" ]; then
	    git push $(git config sxemacs.remote) ${TAGNAME}
	    cat<<EOF

**[Devkey]************************************************************
Please let Steve know that your devkey is ready to be fetched into the
main SXEmacs repo.

You can email Steve at steve@sxemacs.org, be sure to also include the
output from 'git config sxemacs.devkey', it will give Steve a way to
verify that the tag has not been tampered with.
**********************************************************************
                            Hit [RETURN] to continue, or C-c to abort.
EOF
	    read junk
	fi
    fi
}

set_keys()
{
    # Make sure they've got Steve's key in their keyring.  Safe to
    # call even if the key exists in the keyring.
    git tag|grep -q maintainer-pgp &&
        git show maintainer-pgp|gpg --import --quiet

    DEFKEY=$(grep '^default-key' ~/.gnupg/gpg.conf 2>/dev/null |
	awk '{print $2;}')
    if [ -z "${DEFKEY}" ]; then
	GUESS=$(gpg --list-keys $(git config user.email) |
	    awk '/^pub/ { split($2,k,"/"); print k[2] }')
	SIGNKEY=${GUESS:-NOTSET}
    else
	SIGNKEY=${DEFKEY}
    fi

    if [ "${SIGNKEY}" = "NOTSET" ]; then
	echo
	echo -n "Please enter the Key-ID of your default GnuPG key: "
	read RESP
	[ -n "${RESP}" ] && git config user.signingKey ${RESP}
    else
	git config user.signingKey ${SIGNKEY}
    fi

    git config --bool commit.gpgSign true
    [ -n "$(git config sxemacs.devkey)" ] || set_tagblob
}

if ! type gpg 1>/dev/null ; then
    cat<<EOF

**[No GnuPG]**********************************************************
WARNING:  We could not find a gpg executable!
          The GnuPG related setup in this script will be skipped.

We highly recommend that you have and use GnuPG (gpg) so that you can
both, verify the signed objects in the repository, and sign your own
commits.

Once you have GnuPG installed and set up please come back and re-run
this script.
**********************************************************************
                            Hit [RETURN] to continue, or C-c to abort.
EOF
    read junk
elif [ -z "$(git config user.signingKey)" ]; then
    set_keys
fi

# set_tagblog is called from set_keys but we may need to call it
# explicitly if user.signingKey was set prior to running this script.
[ -n "$(git config sxemacs.devkey)" ] || set_tagblob

## Misc config (format, sendemail, etc)
set_formats()
{
    [ -n "$(git config format.numbered)" ] ||
        git config format.numbered auto
    [ -n "$(git config format.to)" ] || git config format.to \
	"SXEmacs Patches <sxemacs-patches@sxemacs.org>"
    [ -n "$(git config format.subjectprefix)" ] ||
        git config format.subjectprefix Patch
    [ -n "$(git config format.headers)" ] || git config format.headers \
	"X-Git-Repo: $(git config remote.$(git config sxemacs.remote).url)
X-Git-Branch: for-steve"
    [ -n "$(git config sendemail.to)" ] || git config sendemail.to \
	"SXEmacs Patches <sxemacs-patches@sxemacs.org>"
    [ -n "$(git config sendemail.from)" ] || git config sendemail.from \
	"$(git config user.name) <$(git config user.email)>"

    echo
    echo "**[Format Summary]****************************************************"
    echo "        Here are the format and sendemail configs we just set."
    echo
    echo "      format.numbered --" $(git config format.numbered)
    echo "            format.to --" $(git config format.to)
    echo " format.subjectprefix --" $(git config format.subjectprefix)
    echo "       format.headers --" $(git config format.headers)
    echo "         sendemail.to --" $(git config sendemail.to)
    echo "       sendemail.from --" $(git config sendemail.from)
    echo
    echo "**********************************************************************"
    echo -n "                            Hit [RETURN] to continue, or C-c to abort."
    read junk
    git config --bool sxemacs.formats true
}
BOOL=$(git config sxemacs.formats)
if [ "${BOOL}" != "true" ]; then
    cat<<EOF

**[Formats]***********************************************************
We are going to set some format config values, but only if they are
not already set, so your existing ones are safe.
**********************************************************************
                            Hit [RETURN] to continue, or C-c to abort.
EOF
    read junk
    set_formats
fi

## Diff xfuncname
set_diff()
{
    # Configure 'git diff' hunk header format. (lifted from FSF's
    # autogen.sh)

    # This xfuncname is based on Git's built-in 'cpp' pattern.
    # The first line rejects jump targets and access declarations.
    # The second line matches top-level functions and methods.
    # The third line matches preprocessor and DEFUN macros.
    [ -n "$(git config diff.cpp.xfuncname)" ] ||
        git config diff.cpp.xfuncname \
	    '!^[ \t]*[A-Za-z_][A-Za-z_0-9]*:[[:space:]]*($|/[/*])
^((::[[:space:]]*)?[A-Za-z_][A-Za-z_0-9]*[[:space:]]*\(.*)$
^((#define[[:space:]]|DEFUN).*)$'
    # Elisp
    [ -n "$(git config diff.elisp.xfuncname)" ] ||
        git config diff.elisp.xfuncname \
	    '^\([^[:space:]]*def[^[:space:]]+[[:space:]]+([^()[:space:]]+)'
    # Texinfo
    [ -n "$(git config diff.texinfo.xfuncname)" ] ||
        git config diff.texinfo.xfuncname '^@node[[:space:]]+([^,[:space:]][^,]+)'

    git config --bool sxemacs.diffhunks true
}

BOOL=$(git config sxemacs.diffhunks)
if [ "${BOOL}" != "true" ]; then
    cat<<EOF

**[Diff Hunk Headers]*************************************************
git-diff can be configured to display a customised hunk header we, use
that feature to try to always print the function name the hunk is 
pointing to.  This will set the appropriate things in your config to 
cover C, Elisp, and Texinfo.  But only if you have not already set
these yourself.
**********************************************************************
EOF
    echo -n "               Would you like more helpful diff hunk headers? [Y/n]: "
    read HUNKS
    if [ "${HUNKS}" = "Y" -o "${HUNKS}" = "y" -o "${HUNKS}" = "" ]; then
	set_diff
    fi
fi

## transfer.fsckObjects
set_fsck()
{
    cat<<EOF

**[Data Integrity]****************************************************
The integrity of the data in our repo is of upmost importance to us.
Having git check it automatically for us is a big help so we are going
to set transfer.fsckObjects in your config.
**********************************************************************
EOF
    echo -n "                            Hit [RETURN] to continue, or C-c to abort."
    read junk
    git config --bool transfer.fsckObjects true
}
git config transfer.fsckObjects || set_fsck

## Hooks
set_hook()
{
    # post-commit hook
    if [ -f ".git/hooks/post-commit" ]; then
	if ! grep -q 'lines added by git-for-steve' .git/hooks/post-commit; then
	    cat>>.git/hooks/post-commit<<EOF

### Begin - lines added by git-for-steve.sh
LOG=\$(git rev-parse --show-toplevel)/++log
[ -f \${LOG} ] && rm -f \${LOG}
### End -- lines added by git-for-steve.sh
EOF
	fi
    elif [ -f ".git/hooks/post-commit.sample" ]; then
	cp .git/hooks/post-commit{.sample,}
	sed -i /Nothing/d .git/hooks/post-commit
	cat>>.git/hooks/post-commit<<EOF

### Begin - lines added by git-for-steve.sh
LOG=\$(git rev-parse --show-toplevel)/++log
[ -f \${LOG} ] && rm -f \${LOG}
### End -- lines added by git-for-steve.sh
EOF
    else
	cat>.git/hooks/post-commit<<EOF
#!/bin/sh
### Begin - lines added by git-for-steve.sh
LOG=\$(git rev-parse --show-toplevel)/++log
[ -f \${LOG} ] && rm -f \${LOG}
### End -- lines added by git-for-steve.sh
EOF
        chmod 755 .git/hooks/post-commit
    fi
}

HAVEHOOK=$(git config sxemacs.commithook)
if [ "${HAVEHOOK}" != "true" ]; then
    cat<<EOF

**[Commit Hook]*******************************************************
Some of the SXEmacs developers use a variation of the
'add-change-log-entry' defun (C-x 4 a) for logging their changes
It creates a log file in the toplevel directory (called '++log') which
you can use with the '-F' switch of 'git commit'.

If you think that is something you would use (we hope you do), then we
can add a post-commit hook to your repo that automatically deletes
that log file after a successful commit.
**********************************************************************
EOF
    echo -n "                                       Shall we add the hook? [Y/n]: "
    read HOOKME
    if [ "${HOOKME}" = "Y" -o "${HOOKME}" = "y" -o "${HOOKME}" = "" ]; then
	set_hook
	git config --bool sxemacs.commithook true
    fi
fi

## Aliases
set_aliases()
{
    echo
    echo "Which directory would you like to use as the output dir for"
    echo -n "git format-patch, and send-email? [${TMP:-/tmp}]: "
    read GITTMP
    GITTMP=${GITTMP:-${TMP:-/tmp}}

    [ -n "$(git config alias.alias)" ] ||
        git config alias.alias "config --get-regexp ^alias"
    [ -n "$(git config alias.bi)" ] || git config alias.bi bisect
    [ -n "$(git config alias.co)" ] || git config alias.co checkout
    [ -n "$(git config alias.cob)" ] || git config alias.cob "checkout -b"
    [ -n "$(git config alias.ci)" ] || git config alias.ci commit
    [ -n "$(git config alias.cam)" ] || git config alias.cam "commit -sam"
    [ -n "$(git config alias.sam)" ] || git config alias.sam "commit -sam"

    BOOL=$(git config sxemacs.commithook)
    if [ "${BOOL}" = "true" ]; then
	[ -n "$(git config alias.cwl)" ] ||
            git config alias.cwl \
            '!git commit -sF $(git rev-parse --show-toplevel)/++log'
        [ -n "$(git config alias.cawl)" ] ||
            git config alias.cawl \
            '!git commit -saF $(git rev-parse --show-toplevel)/++log'
    fi

    [ -n "$(git config alias.rbi)" ] || git config alias.rbi "rebase -i"
    [ -n "$(git config alias.prb)" ] || git config alias.prb "pull --rebase"

    REMOTE=$(git config sxemacs.remote)
    if [ -n "${REMOTE}" ]; then
	[ -n "$(git config alias.pfs)" ] ||
            git config alias.pfs "push ${REMOTE} for-steve"
    fi

    [ -n "$(git config alias.fp)" ] || git config alias.fp \
	"format-patch --minimal -o ${GITTMP} origin/master"
    [ -n "$(git config alias.fpc)" ] || git config alias.fpc \
	"format-patch --minimal --cover-letter -o ${GITTMP} origin/master"
    [ -n "$(git config alias.sp)" ] || git config alias.sp \
	"send-email ${GITTMP}"
    [ -n "$(git config alias.spc)" ] || git config alias.spc \
	"send-email --compose ${GITTMP}"
    [ -n "$(git config alias.wb)" ] || git config alias.wb \
	"rev-parse --abbrev-ref HEAD"

    echo
    echo "**[Your Aliases]******************************************************"
    echo "         The following aliases are now available for use..."
    echo
    git alias
    echo
    echo "**********************************************************************"
    echo -n "                            Hit [RETURN] to continue, or C-c to abort."
    read junk
    git config --bool sxemacs.aliases true
}

BOOL=$(git config sxemacs.aliases)
if [ "${BOOL}" != "true" ]; then
    cat<<EOF

**[Aliases]***********************************************************
And finally, lets set a few aliases.  We will only set them if they
have not already been set so all of your pre-existing aliases are
safe.
**********************************************************************
                            Hit [RETURN] to continue, or C-c to abort.
EOF
    read junk
    set_aliases
fi

popd 1>/dev/null

if [ ${LETSPOP} -eq 1 ]; then
    # If we changed branches on them, go back.
    if [ "${CURRENT_BRANCH}" != "$(git rev-parse --abbrev-ref HEAD)" ]; then
	git checkout --quiet ${CURRENT_BRANCH}
    fi
    git stash pop --quiet
fi

## All done, bar the shouting...
cat<<EOF

**[Thanks]************************************************************
| Thank you for taking the time to setup your repo so that you can   |
| contribute back to the SXEmacs Project.  We really appreciate all  |
| that you do, no matter how small or insignificant you may think it |
| is, it all counts.                                                 |
|                                                                    |
| If you have not already done so, please take a few minutes now to  |
| read through our policies and procedures manual,  We call it the   |
| "SPPM".  From inside SXEmacs you can get to it via 'C-h C-i sppm', |
| or from the command line via 'info sppm'.                          |
|                                                                    |
| Also, now would be a good time for you to head over to             |
| https://www.sxemacs.org/lists.html and subscribe to our mailing     |
| lists.                                                             |
|--------------------------------------------------------------------|
| Thank you, and do drop into #sxemacs on freenode IRC for a chat    |
| any time, especially if you need a hand with anything.             |
**********************************************************************
EOF

exit 0
### git-for-steve.sh ends here.
