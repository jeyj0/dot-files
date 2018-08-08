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
11. Install docker
	1. ```$ sudo apt-get update```
	2. ```$ sudo apt-get install \
    		apt-transport-https \
    		ca-certificates \
    		curl \
    		software-properties-common```
	3. ```$ curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -```
	4. ```$ sudo add-apt-repository \
   			"deb [arch=amd64] https://download.docker.com/linux/ubuntu \
   			$(lsb_release -cs) \
   			stable"```
	5. ```$ sudo apt-get update```
	6. ```$ sudo apt-get install docker-ce```
	7. ```$ sudo groupadd docker```
	8. ```$ sudo usermod -aG docker $USER```
12. install sdkman
	1. installation
	2. add JAVA_HOME to exports: `export JAVA_HOME="~/.sdkman/candidates/java/current"`
12. Install maven
	1. MANUALLY download maven
	2. extract .zip/.tar.gz to ~/maven/VE.RS.ION
	3. `export PATH=$PATH:~/maven/VE.RS.ION/bin` // add maven to path to be executable
13. Install gcc
	1. `$ sudo apt-get install gcc`
14. Install pyenv (python version manager) (using pyenv-installer)
	1. `$ curl -L https://github.com/pyenv/pyenv-installer/raw/master/bin/pyenv-installer | bash`
	2. install pyenv dependencies:
		`$ sudo apt-get install -y make build-essential libssl-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev wget curl llvm libncurses5-dev libncursesw5-dev xz-utils tk-dev libffi-dev liblzma-dev`
	4. install python 2.7: `$ pyenv install 2.7.15`