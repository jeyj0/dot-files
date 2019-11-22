export PATH="$PATH:$HOME/.local/bin"

if [ -d "$HOME/.config/nvm" ]; then
	source $HOME/scripts/paths/nvm.sh
fi

if [ -d "$HOME/.pyenv" ]; then
	source $HOME/scripts/paths/pyenv.sh
fi

if [ -d "$HOME/.sdkman" ]; then
	source $HOME/scripts/paths/sdkman.sh
fi

if [ -d "$HOME/maven" ] && [[ $(ls -A $HOME/maven) ]]; then
	source $HOME/scripts/paths/maven.sh
fi
