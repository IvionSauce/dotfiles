# Load various terminal emulator settings
xrdb -merge ~/.Xresources

# Set blanking to 40mins
xset s off
xset dpms 0 2400 0

# Load stored settings for NVidia cards
nvidia-settings -l

# Set keyboard options
# (caps lock -> super) and (l_shift+r_shift -> caps lock)
# (swap l_ctrl and l_alt)
#setxkbmap -option caps:super,shift:both_capslock,ctrl:swap_lalt_lctl

# Because setxkbmap has only a limited amount of customization
# http://madduck.net/docs/extending-xkb/
xkbcomp -I$HOME/.xkb ~/.xkb/keymap/my-keymap $DISPLAY

# Set previous wallpaper
sh ~/.fehbg

exec dbus-launch --exit-with-session i3
