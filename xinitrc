# Esto en el .bash_profile (.zprofile en zsh)
# if [[ "$(tty)" == "/dev/tty1" ]]; then
# 	pgrep awesome || startx "$XDG_CONFIG_HOME/X11/xinitrc"
# fi

xbanish &
xsetroot -cursor_name left_ptr	&

exec awesome
