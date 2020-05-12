" VSCode only settings

" KEY BINDINGS

" find file
nnoremap <leader><leader> :<C-u>call VSCodeNotify('workbench.action.quickOpen')<CR>

" file recent
nnoremap <leader>fr :<C-u>call VSCodeNotify('workbench.action.openRecent')<CR>

" file explorer
nnoremap <leader>fe :<C-u>call VSCodeNotify('workbench.files.action.focusFilesExplorer')<CR>

" open folder/project
nnoremap <leader>pp :<C-u>call VSCodeNotify('workbench.action.openWorkspace')<CR>

" file save
nnoremap <leader>fs :<C-u>call VSCodeNotify('workbench.action.files.save')<CR>

" buffer kill
nnoremap <leader>bk :<C-u>call VSCodeNotify('workbench.action.closeActiveEditor')<CR>

" code comment
nnoremap <leader>cc :<C-u>call VSCodeNotify('editor.action.commentLine')<CR>

" code rename
nnoremap <leader>ciw :<C-u>call VSCodeNotify('editor.action.rename')<CR>

" code format
nnoremap <leader>cf :<C-u>call VSCodeNotify('editor.action.formatDocument')<CR>

" open terminal
nnoremap <leader>ot :<C-u>call VSCodeNotify('workbench.panel.terminal.focus')<CR>

" remove search highlighting
nnoremap <esc> :noh<CR>

" window split
nnoremap <leader>ws :<C-u>call VSCodeNotify('workbench.editor.splitEditor')<CR>

" git go
nnoremap <leader>gg :<C-u>call VSCodeNotify('magit.status')<CR>

" search project
nnoremap <leader>sp :<C-u>call VSCodeNotify('workbench.action.findInFiles')<CR>

" reload/switch color theme
nnoremap <leader>hrt :<C-u>call VSCodeNotify('workbench.action.selectTheme')<CR>
