# Installation steps for new Dev-Laptop on Ubuntu

1. Install Chrome (manually)
2. Install hyper.js (manually)
3. Install curl
	1. ```$ sudo apt-get install curl```
4. Install git
	1. ```$ sudo apt-get install git```
	2. ```$ git config --global user.email "jeyj0.plus@gmail.com"```
	3. ```$ git config --global user.name "Jannis Jorre"```
5. Install zsh & oh-my-zsh
	1. ```$ sudo apt-get install zsh```
	2. ```$ chsh -s $(which zsh)``` // make zsh the default shell
	3. ```$ sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"``` // install oh-my-zsh
6. Install VS-Code
	1. Add VSCode repository:
		```$ curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > microsoft.gpg
		$ sudo mv microsoft.gpg /etc/apt/trusted.gpg.d/microsoft.gpg
		$ sudo sh -c 'echo "deb [arch=amd64] https://packages.microsoft.com/repos/vscode stable main" > /etc/apt/sources.list.d/vscode.list'```
	2. ```$ sudo apt-get update```
	3. ```$ sudo apt-get install code```
7. Install IntelliJ
	1. ```$ sudo snap install intellij-idea-ultimate --classic --edge```
	2. (manually activate intellij)
8. ```$ mkdir ~/projects && cd ~/projects```
9. ```$ mkdir gitlab && mkdir github```
10. ```$ git clone https://github.com/jeyj0/dot-files.git```
