" visual settings
set number relativenumber

set scrolloff=3
set nowrap

let g:gruvbox_italic = 1
let g:gruvbox_contrast_dark = 'hard'
let g:gruvbox_sign_column = 'bg0'
colorscheme gruvbox
" customize current-line-color (less extreme)
highlight CursorLine ctermbg=black

set noshowmode
let g:airline_powerline_fonts = 1
let g:airline_section_y = 'B:%{bufnr("%")}' " buf.nr. instead of encoding
let g:airline_section_z = '%l/%L:%c' " minimalistic line indicator
let g:airline_theme='gruvbox'
let g:bufferline_echo = 0
let g:airline_mode_map = {}
let g:airline_mode_map['ic'] = 'INSERT'

" highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

" highlight current line (in current window)
augroup CursorLine
	au!
	au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
	au WinLeave * setlocal nocursorline
augroup END
