runtime config/plugins.vim
runtime config/behavior.vim
runtime config/keys.vim
runtime config/visuals.vim

" from here on, only settings for different nvim versions (in vscode, terminal, gui,...) goes
if exists('g:vscode')
    runtime config/clients/vscode.vim
endif
