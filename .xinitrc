setxkbmap -layout hu -option caps:ctrl_modifier
xsetroot -cursor_name left_ptr
nitrogen --restore &
dunst &
xmonad --recompile
exec xmonad
