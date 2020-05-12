runtime config/plugins.vim
runtime config/behavior.vim
runtime config/keys.vim
runtime config/visuals.vim

" fzf commands / shortcuts
let $FZF_DEFAULT_COMMAND = 'ag -g ""'

" do not map <leader>bd to close buffer (bclose.vim)
let g:bclose_no_plugin_maps = 1

" use ranger when opening a directory
let g:ranger_map_keys = 0
let g:ranger_replace_netrw = 1

" configure neovim-remote to work properly with git
autocmd FileType gitcommit,gitrebase,gitconfig,diff set bufhidden=delete

" coc.nvim config
set updatetime=300

" from here on, only settings for different nvim versions (in vscode, terminal, gui,...) goes
if exists('g:vscode')
    runtime config/clients/vscode.vim
endif
