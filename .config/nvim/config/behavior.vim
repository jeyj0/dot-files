" share clipboard with system
set clipboard=unnamedplus

" set for-me-intuitive split opening
" set splitright
" set splitbelow

" smart search
set ignorecase
set smartcase

" always keep signcolumn open
set signcolumn=yes

" setup terminal
autocmd TermOpen * setlocal nonumber norelativenumber " no line numbers in terminal buffer
autocmd TermOpen * startinsert " start terminal in terminal-mode

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
