# Remote controll Visual Studio with Emacs Manual
Copyright (C) 2011-2012 FINAP(http://finap.blog49.fc2.com/)

## rc-vstudio.elの使い方


### Emacsの設定
秀丸エディタからVisualStudioを制御するマクロ(http://d.hatena.ne.jp/ohtorii/20110402/1301719953)
上記のohtoriiさんのエントリを辿って、visual_studio_hidemaru.exeをダウンロードして、PATHの通った場所に置きます。


GitHub(https://github.com/finap/emacs-remote-vstudio)
からrc-vstudio.elをダウンロードして、LoadPathの通ったところに置きます。
Emacsの設定で、rc-vstudioをrequireします。

`(require 'rc-vstudio)`


### 実行
　コマンドの流れは、

1. visual studioを起動して、Emacsから操作したいソリューションを開く
2. 開いたソリューションで管理されているファイルを開く
3. 動作させたいコマンドを実行する（vstudio-pid-を除くコマンド名）

か、

1. visual studioを起動して、Emacsから操作したいソリューションを開く
2. M-x vstudio-select で、操作したいソリューションファイルパスを選択する
3. 動作させたいコマンドを実行する（vstudio-pid-で続くコマンド名）

お好きな方法でどうぞ。


### 操作したいソリューションファイルパスを選択する操作方法
　C-nで次のプロジェクトへ、C-pで前のプロジェクトへ選択を移動します。
RETで操作するプロジェクトを決定します。

### 動作するコマンド
　コマンドは、以下のものがinteractiveで呼び出せます。

    vstudio-activate … VisualStudioのウィンドウをアクティブにする
    vstudio-build … VisualStudioのビルドコマンドを実行する
    vstudio-build-cancel … VisualStudioのビルドをキャンセルする
    vstudio-rebuild … VisualStudioのリビルドコマンドを実行する
    vstudio-clean … VisualStudioのクリーンコマンドを実行する
    vstudio-debug … VisualStudioのデバッグの開始コマンドを実行する
    vstudio-run … VisualStudioのデバッグなしで開始コマンドを実行する
    vstudio-stop … VisualStudioのデバッグを停止する
    vstudio-compile … 現在開いているファイルで、VisualStudioのコンパイルコマンドを実行する
    vstudio-activate-with-openfile … 現在Emacsで開いているファイルを、VisualStudioで開く
    string-current-vstudio-output-console … 現在の出力ウィンドウの内容の文字列を取得する
    vstudio-select … 操作するVisualStudioを選択する

以下は、vstudio-selectで選択したソリューションに対して実行される

    vstudio-pid-activate … VisualStudioのウィンドウをアクティブにする
    vstudio-pid-build … VisualStudioのビルドコマンドを実行する
    vstudio-pid-build-cancel … VisualStudioのビルドをキャンセルする
    vstudio-pid-rebuild … VisualStudioのリビルドコマンドを実行する
    vstudio-pid-clean … VisualStudioのクリーンコマンドを実行する
    vstudio-pid-debug … VisualStudioのデバッグの開始コマンドを実行する
    vstudio-pid-run … VisualStudioのデバッグなしで開始コマンドを実行する
    vstudio-pid-stop … VisualStudioのデバッグを停止する
    vstudio-pid-openfile-compile … 現在開いているファイルで、VisualStudioのコンパイルコマンドを実行する

*以下は内部用*

    vstudio-project-absolute-path-list
    vstudio-select-current-pid-count-line
    vstudio-select-current-pid-next-line
    vstudio-select-current-pid-previous-line


### c++のcompile-commandに設定する
　以下の設定を追加することで、VisualStudioのソリューションにあるcppファイルを開いている時に
C-c cをすると、Emacs上でコンパイルすることができます。

    ;; visual studioのコンパイルをEmacsに設定する
    (require 'compile)
    (add-hook 'c++-mode-hook
               (lambda ()
    	       (set (make-local-variable 'compile-command)
    		    (vstudio-compile-command)))) 
    )

動作イメージ。
http://blog-imgs-17.fc2.com/f/i/n/finap/rc-vstudio.jpg


### その他、やっておくといい設定
　VisualStudio.NETとMeadowの連携( http://www.bookshelf.jp/pukiwiki/pukiwiki.php?Windows%2FVisualStudio.NET%A4%C8Meadow%A4%CE%CF%A2%B7%C8#content_1_2 )
を参考にして、VisualStudioで開いているファイルをEmacsで開けるようにしておき、
vstudio-activate-with-openfileを何かのキーに割り当てておくと、互いの行き来が楽です。

例：

`(global-set-key [M-return] 'vstudio-activate-with-openfile)`
