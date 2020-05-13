" share clipboard with system
set clipboard=unnamedplus

" use ranger when opening a directory
let g:ranger_replace_netrw = 1

" fzf commands / shortcuts
let $FZF_DEFAULT_COMMAND = 'ag -g ""'

" configure neovim-remote to work properly with git
autocmd FileType gitcommit,gitrebase,gitconfig,diff set bufhidden=delete

" coc.nvim config
set updatetime=300

" smart search
set ignorecase
set smartcase

" always keep signcolumn open
set signcolumn=yes

" setup terminal
autocmd TermOpen * setlocal nonumber norelativenumber " no line numbers in terminal buffer
autocmd TermOpen * startinsert " start terminal in terminal-mode

" taken from
" https://vi.stackexchange.com/questions/3725/swap-the-position-of-two-windows
function! SwitchWindows() abort
    let thiswin = winnr()
    let thisbuf = bufnr("%")
    let lastwin = winnr("#")
    let lastbuf = winbufnr(lastwin)

    exec  lastwin . " wincmd w" ."|".
        \ "buffer ". thisbuf ."|".
        \ thiswin ." wincmd w" ."|".
        \ "buffer ". lastbuf

    " let l:current_buf = winbufnr(0)
    " exe "buffer" . winbufnr(a:count)
    " exe a:count . "wincmd p"
    " exe "buffer" . l:current_buf
    " wincmd p
endfunction

" taken from coc.nvim readme
function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Original implementation taken from:
" https://www.reddit.com/r/vim/comments/ebaoku/function_to_google_any_text_object/
function! WebSearch(type, ...)
    let sel_save = &selection
    let &selection = "inclusive"
    let reg_save = @@

    if a:0  " Invoked from Visual mode, use '< and '> marks.
        silent exe "normal! `<" . a:type . "`>y"
    elseif a:type == 'line'
        silent exe "normal! '[V']y"
    elseif a:type == 'block'
        silent exe "normal! `[\<C-V>`]y"
    else
        silent exe "normal! `[v`]y"
    endif

    let search = substitute(trim(@@), ' \+', '+', 'g')
    silent exe "!open 'https://duckduckgo.com/?q=" . search . "'"

    let &selection = sel_save
    let @@ = reg_save
endfunction
