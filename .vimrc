" set lines=62
" set columns=160
" :80 vs

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

" set guioptions+=m
set wildmenu
set wildmode=full

" set statusline=%F%m%r%h%w\ [FORMAT=%{&ff}]\ [TYPE=%Y]\ [ASCII=\%03.3b]\ [HEX=\%02.2B]\ [POS=%04l,%04v][%p%%]\ [LEN=%L]
set laststatus=2

inoremap <c-a> <home>
inoremap <c-e> <end>
inoremap <c-n> <down>
inoremap <c-p> <up>
inoremap <c-b> <left>
inoremap <c-f> <right>

if has('vim_starting')
        set runtimepath+=~/.vim/bundle/neobundle.vim/
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
" NeoBundle 'Shougo/unite.vim'
" NeoBundle 'thinca/vim-quickrun'
" NeoBundle 'davidoc/taskpaper.vim'
" NeoBundle 'itchyny/lightline.vim'
" NeoBundle 'altercation/vim-colors-solarized'
NeoBundle 'wlangstroth/vim-racket'
NeoBundle 'ds26gte/scmindent'
NeoBundle 'kovisoft/slimv'

"GitHub以外のGitリポジトリにあるプラグインを利用する
" NeoBundle 'git://git.wincent.com/command-t.git'

" vim-scripts リポジトリにあるプラグインを利用する
" NeoBundle 'surround.vim'
" NeoBundle 'RainbowParenthsis.vim'

"Git以外のリポジトリにあるプラグインを利用する
" NeoBundle 'http://svn.macports.org/repository/macports/contrib/mpvim/'
" NeoBundle 'https://bitbucket.org/ns9tks/vim-fuzzyfinder'

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
