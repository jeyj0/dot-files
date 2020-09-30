" enable project-specific vim settings
set exrc

" share clipboard with system
set clipboard=unnamedplus

" immediately preview changes in :%s/foo/bar substitutions (or similars)
:set inccommand=nosplit

" allow switching from buffer if buffer is unsaved
set hidden

" ask to save buffer when closed unsaved
set confirm

" use ranger when opening a directory
let g:ranger_replace_netrw = 1

" fzf commands / shortcuts
let $FZF_DEFAULT_COMMAND = 'ag -g ""'

" fzf function to switch to project
function! CdToProject(project) abort
    execute 'cd' fnameescape("~/projects/" . a:project)
endfunction
function! SwitchToProject() abort
    call fzf#run(fzf#wrap('fzf_cd_project', { 'source': 'ls ~/projects/', 'sink': funcref('CdToProject') }, 0))
endfunction

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

" taken from
" https://stackoverflow.com/a/37866336/4554254
function! CloseAllBuffersButCurrent()
  let curr = bufnr("%")
  let last = bufnr("$")

  if curr > 1    | silent! execute "1,".(curr-1)."bd"     | endif
  if curr < last | silent! execute (curr+1).",".last."bd" | endif
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
    silent exe "!open 'https://search.jorre.dev/?q=" . search . "'"

    let &selection = sel_save
    let @@ = reg_save
endfunction

" taken from coc.nvim readme and modified
function! ShowDocumentation()
  try
    if (index(['vim','help'], &filetype) >= 0)
      execute 'h '.expand('<cword>')
    else
      call CocAction('doHover')
    endif
  catch
    echo "Sorry, no documentation or help found"
  endtry
endfunction

" code comment toggling
" inspired by https://stackoverflow.com/a/24046914/4554254

let s:comment_map = {
    \   "c": '\/\/',
    \   "cpp": '\/\/',
    \   "go": '\/\/',
    \   "java": '\/\/',
    \   "javascript": '\/\/',
    \   "lua": '--',
    \   "scala": '\/\/',
    \   "php": '\/\/',
    \   "python": '#',
    \   "ruby": '#',
    \   "rust": '\/\/',
    \   "sh": '#',
    \   "desktop": '#',
    \   "fstab": '#',
    \   "conf": '#',
    \   "profile": '#',
    \   "bashrc": '#',
    \   "bash_profile": '#',
    \   "mail": '>',
    \   "eml": '>',
    \   "bat": 'REM',
    \   "ahk": ';',
    \   "vim": '"',
    \   "tex": '%',
    \   "Dockerfile": '#',
    \   "nix": '#',
    \   "fish": '#',
    \   "haskell": '--',
    \   "typescript": '\/\/',
    \   "yaml.docker-compose": '#',
    \ }

function! ToggleCodeComment() range
    if has_key(s:comment_map, &filetype)
        let l:win_view = winsaveview()
        let l:comment_symbol = s:comment_map[&filetype]

        let l:should_comment = match(getline(a:firstline), '^\s*' . l:comment_symbol . ' ') == -1

        if l:should_comment
            execute "silent " . a:firstline . "," . a:lastline . "s/\\(^\\s*\\)\/\\1" . l:comment_symbol . " /"
        else
            execute "silent " . a:firstline . "," . a:lastline . "s/\\(^\\s*\\)" . l:comment_symbol . "\\s*/\\1/"
        endif

        call winrestview(l:win_view)
    else
        echo "No comment symbol found for filetype"
    endif
endfunction

command! -range ToggleCodeComment <line1>,<line2>call ToggleCodeComment()

