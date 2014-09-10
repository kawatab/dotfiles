set nocompatible

set title	" Display the file name
syntax on	" colored syntax
set tabstop=4	" indent with 4 spaces
set smartindent	" auto indent

" set ignorecase	"ignore cast
" set smartcase	" cast sensitive if a capital letter is included
set wrapscan

set autoindent	" indent in new line as well as previous line

" set expandtab	" use spaces instead of tabs
set hidden	" edit several files without saving

set incsearch	" turn on incremental search

set showmatch

set smarttab

" set number
set ruler

set t_Co=256	" 256-color mode

" Download color theme and copy to ~/.vim/colors
colorscheme wombat256

" set guioptions+=m
set wildmenu
set wildmode=longest,list

" set statusline=%<%f\ %{fugitive#statusline()}\ %h%m%r%=%-14.(%l,%c%V%)\ %P

set laststatus=2

if has('vim_starting')
		set runtimepath+=~/.vim/bundle/neobundle.vim/
		set runtimepath+=~/.vim/bundle/im_control.vim/plugin/im_control.vim
endif

call neobundle#rc(expand('~/.vim/bundle/'))

" Let NeoBundle manage NeoBundle
NeoBundleFetch 'Shougo/neobundle.vim'

" Recommended to install
" After install, turn shell ~/.vim/bundle/vimproc, (n.g)make -f
" your_machines_makefile
NeoBundle 'Shougo/vimproc', {
                        \  'build' : {
                        \ 'windows' : 'make -f make_mingw32.mak',
                        \ 'cygwin' : 'make -f make_cygwin.mak',
                        \ 'mac' : 'make -f make_mac.mak',
                        \ 'unix' : 'make -f make_unix.mak',
                        \ },
                        \ }

filetype plugin indent on       " Required!

"Brief help
" :NeoBundleList        - list configured bundles
" :NeoBiundleInstall(!) - install(update) bundles
" :NeoBundleClean(!)    - confirm(or auto-approve) removal of unused bundle

" Installation check.
NeoBundleCheck

" GitHubリポジトリにあるプラグインを利用する
" --> NeoBundle 'USER/REPOSITORY-NAME'
" NeoBundle 'Shougo/neocomplcache'
" NeoBundle 'Shougo/neosnippet'
" NeoBundle 'thinca/vim-quickrun'
" NeoBundle 'davidoc/taskpaper.vim'
" NeoBundle 'altercation/vim-colors-solarized'
" NeoBundle 'Shougo/vimshell.vim'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/vimfiler.vim'
NeoBundle 'itchyny/lightline.vim'
NeoBundle 'airblade/vim-gitgutter'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'wlangstroth/vim-racket'
NeoBundle 'ds26gte/scmindent'
NeoBundle 'kovisoft/slimv'
NeoBundle 'fuenor/im_control.vim'

"GitHub以外のGitリポジトリにあるプラグインを利用する
" NeoBundle 'git://git.wincent.com/command-t.git'

" vim-scripts リポジトリにあるプラグインを利用する
" NeoBundle 'surround.vim'
" NeoBundle 'RainbowParenthsis.vim'

"Git以外のリポジトリにあるプラグインを利用する
" NeoBundle 'http://svn.macports.org/repository/macports/contrib/mpvim/'
" NeoBundle 'https://bitbucket.org/ns9tks/vim-fuzzyfinder'

runtime! ../../rc.d/*.vim
highlight SignColumn ctermbg=black
highlight GitGutterAdd ctermfg=green
highlight GitGutterDelete ctermfg=red
highlight GitGutterChange ctermfg=yellow

if has("autocmd")
		au BufReadPost *.rkt,*.rktl set filetype=racket
endif

" File Specific Settings
" ------------------------------------------------------------

autocmd FileType javascript setl tabstop=8 expandtab shiftwidth=2 softtabstop=2

au FileType xhtml,html,htm,php,xml setlocal tabstop=2
au FileType xhtml,html,htm,php,xml setlocal shiftwidth=2
"au FileType xhtml,html,htm,php,xml setlocal expandtab      " (et) expand tabs to spaces (use :retab to redo entire file)
au FileType xhtml,html,htm,php,xml setlocal softtabstop=2   " (sts) makes spaces feel like tabs (like deleting)

au FileType c,h,java,js setlocal mps+==:;                   " allow the match pairs operation (%) to work with '=' and ';'

au FileType c,h setlocal cindent                            " enable the intelligent cindent (cin) feature for the following files
au FileType java,js setlocal smartindent                    " enable the smartindenting (si) feature for the following files

au FileType txt setlocal fo+=tn

" before writing to any file, this function call will remove any extra white space at the end of a line
" au! BufWrite,FileWritePre * call RemoveWhiteSpace()


" for Japanese Input
" 「日本語入力固定モード」の動作モード
let IM_CtrlMode = 1
" 「日本語入力固定モード」切替キー
inoremap <silent> <C-j> <C-r>=IMState('FixMode')<CR>

" IBus 1.5以降
function! IMCtrl(cmd)
  let cmd = a:cmd
  if cmd == 'On'
    let res = system('ibus engine ' . g:IM_PrevMode)
  elseif cmd == 'Off'
    " let IM_PrevMode = system('ibus engine')
    let res = system('ibus engine "xkb:us:altgr-intl:eng"')
  endif
  return ''
endfunction
.
" <ESC>押下後のIM切替開始までの反応が遅い場合はttimeoutlenを短く設定してみてください。
" IMCtrl()のsystem()コマンド実行時に&を付けて非同期で実行するという方法でも体感速度が上がる場合があります。
set timeout timeoutlen=3000 ttimeoutlen=100 
