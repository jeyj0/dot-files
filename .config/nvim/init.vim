runtime config/plugins.vim
runtime config/behavior.vim
runtime config/keys.vim
runtime config/visuals.vim

" from here on, only settings for different nvim versions (in vscode, terminal, gui,...) goes
if exists('g:vscode')
    runtime config/clients/vscode.vim
endif

if has('nvim')
    runtime config/clients/neovim.vim
endif

" make sure that any further configs cannot do unsafe stuff
" this is important if exrc is set (`set exrc`)
" THIS SHOULD BE THE LAST EXECUTED LINE OF THE CONFIG
set secure
