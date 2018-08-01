# This script installs all necessary tools and software to get going!

# install curl
sudo apt-get install curl

# install git
sudo apt-get install git

## configure git basics
git config --global user.email "jeyj0.plus@gmail.com"
git config --global user.name "Jannis Jorre"

# install zsh
sudo apt-get install zsh
chsh -s $(which zsh) # make zsh the default shell

# install oh-my-zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

# install vs-code

## add vs-code repository
curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > microsoft.gpg
sudo mv microsoft.gpg /etc/apt/trusted.gpg.d/microsoft.gpg
sudo sh -c 'echo "deb [arch=amd64] https://packages.microsoft.com/repos/vscode stable main" > /etc/apt/sources.list.d/vscode.list'
sudo apt-get update

## actually install vs-code
sudo apt-get install code

# install intellij (ultimate)
sudo snap install intellij-idea-ultimate --classic --edge

# setup projects directory
mkdir ~/projects && cd ~/projects
mkdir gitlab && mkdir github
