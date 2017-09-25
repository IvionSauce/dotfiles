export EDITOR="et"
export LANG="en_US.UTF-8"

# Forego these hackish env variables and instead rely on
# ForceFullCompositionPipeline and loading nvidia-settings in .xinitrc
#export __GL_SYNC_TO_VBLANK=1
#export __GL_SYNC_DISPLAY_DEVICE=DVI-D-0
#export __VDPAU_NVIDIA_SYNC_DISPLAY_DEVICE=DVI-D-0

ZDOTDIR=$HOME/.zsh
fpath=("$ZDOTDIR/functions" $fpath)
