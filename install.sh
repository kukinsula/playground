#!/bin/sh

function create_simlink () {
		output=$(ln -s $1 $2)

		if [ "$?" = 0 ]
    then
        echo "Successfully created simlink $2"
    fi
}

create_simlink `pwd`/conkyrc ~/.conkyrc
create_simlink `pwd`/zshrc ~/.zshrc
create_simlink `pwd`/emacs/emacs.el ~/.emacs
create_simlink `pwd`/toprc ~/.toprc
create_simlink `pwd`/gitconfig ~/.gitconfig
create_simlink `pwd`/terminator ~/.config/terminator/config
create_simlink `pwd`/emacs/emacs.desktop ~/.local/share/applications/emacs.desktop
create_simlink `pwd`/rush_auto_completion ~/.rush_auto_completion
