#!/usr/bin/zsh

# Usage:
# $0 <project name>

WD=`pwd`
LOG=$WD/log

function exit_if_not_zero {
    "$@" 2>>$LOG >>$LOG
    if test $? -ne 0; then
        exit 1
    fi
}

# Need a second argument (project)
if test -z $1; then
    echo "Usage:"
    echo $0 "<project>"
    exit 1
fi

# Directory needs to exist
if test ! -d $1; then
    echo $1 "is not a directory."
    exit 1
fi


# Set up logging
if test ! -f $LOG; then
    touch $LOG
fi
echo "" >> $LOG
echo "" >> $LOG
echo "Running at $(date)..." >> $LOG


# cd into project
echo "Going into $1..." >> $LOG
cd $1


# Check if first-time use (exists? project_prev)
GIT_PREV_FILE=$WD/$1"_prev"
if test ! -f $GIT_PREV_FILE; then
    exit_if_not_zero touch $GIT_PREV_FILE
    git for-each-ref --format="%(refname)" --sort=-taggerdate refs/tags > $GIT_PREV_FILE
    echo "Ran first-time, now quitting." >> $LOG
    exit 0
else
    git_prev=$(cat $GIT_PREV_FILE)
fi

#exit_if_not_zero git pull
git_latest=$(git for-each-ref --format="%(refname)" --sort=-taggerdate refs/tags)

if test "$git_prev" = "$git_latest"; then
    # No change. Just exit.
    exit 0
fi

git_latest_tag=$(diff <(echo $git_prev) <(echo $git_latest) --unchanged-line-format="" --old-line-format="" | cut -d "/" -f 3) 
git_prev_tag=$(head -1 $GIT_PREV_FILE | cut -d "/" -f 3)

git diff $git_prev_tag $git_latest_tag -- NEWS

echo $git_latest > $GIT_PREV_FILE
