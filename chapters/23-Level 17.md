# Level 17 (テキスト処理ツール)
## 1. 実践演習編の概要

### 1-1. これまでの学習内容

Level 1〜16 で Common Lisp の基礎を学んだ。

- **基礎**：S式、評価、データ型、変数、関数
- **制御**：条件分岐、繰り返し、再帰
- **データ**：リスト、ベクタ、ハッシュテーブル
- **関数型**：高階関数、クロージャ、ラムダ
- **入出力**：ファイル操作、ストリーム
- **文字列**：文字列操作、フォーマット
- **エラー**：コンディション、リスタート
- **OOP**：CLOS、ジェネリック関数

### 1-2. 実践演習編の目的

Level 17〜25 では、これらの知識を組み合わせて実践的なプログラムを作成する。単なる文法の確認ではなく、設計から実装まで一連の流れを体験する。

### 1-3. Level 17 の目標

テキスト処理ツールを作成する。以下の機能を実装する

- 単語の出現頻度を数える
- 行数・単語数・文字数を数える（wc コマンド相当）
- テキストの検索と置換
- 簡易的な grep 機能



## 2. プロジェクト構成

### 2-1. ファイル構成

```
text-tools/
├── text-tools.asd        ; システム定義
├── package.lisp          ; パッケージ定義
├── utils.lisp            ; ユーティリティ関数
├── counter.lisp          ; カウント機能
├── search.lisp           ; 検索・置換機能
└── main.lisp             ; メインインターフェース
```

### 2-2. パッケージ設計

テキスト処理に必要な機能を1つのパッケージにまとめる。外部に公開する関数と内部で使う関数を明確に分ける。

```lisp:package.lisp
(defpackage :text-tools
  (:use :cl)
  (:export
   ;; カウント機能
   :count-lines
   :count-words
   :count-chars
   :word-frequency
   :wc
   :wc-file
   :wc-report
   ;; 検索・置換機能
   :grep
   :grep-file
   :replace-all
   :replace-in-file
   ;; ユーティリティ
   :read-file-contents
   :write-file-contents
   :split-into-words))
```



## 3. ユーティリティ関数

### 3-1. ファイル読み書き

まず、ファイルの読み書きを行う基本関数を作成する。これらは他の機能から繰り返し使われる。

日本語などのマルチバイト文字を正しく扱うため、`:external-format :utf-8` を指定する。また、UTF-8 ではバイト数と文字数が一致しないため、`file-length` を使わず行単位で読み込む方式を採用する。

```lisp:utils.lisp
(in-package :text-tools)

(defun read-file-contents (path)
  "ファイルの内容を文字列として読み込む。
   ファイルが存在しない場合は NIL を返す。"
  (handler-case
      (with-open-file (stream path 
                              :direction :input
                              :external-format :utf-8)  ; UTF-8 で読み込み
        ;; 行単位で読み込んで結合（UTF-8 対応）
        (with-output-to-string (out)
          (loop for line = (read-line stream nil nil)
                for first-line = t then nil
                while line
                do (progn
                     (unless first-line
                       (write-char #\Newline out))  ; 行間に改行を挿入
                     (write-string line out)))))
    ;; ファイルエラーを捕捉して NIL を返す
    (file-error (e)
      (format *error-output* "ファイルエラー: ~A~%" e)
      nil)))

(defun write-file-contents (path contents &key (if-exists :supersede))
  "文字列をファイルに書き込む。
   成功時は T、失敗時は NIL を返す。"
  (handler-case
      ;; ファイルを書き込みモードで開く
      (with-open-file (stream path
                       :direction :output
                       :if-exists if-exists           ; 既存ファイルは上書き
                       :if-does-not-exist :create     ; なければ作成
                       :external-format :utf-8)       ; UTF-8 で書き込み
        (write-string contents stream)
        t)  ; 成功時は T を返す
    (file-error (e)
      (format *error-output* "ファイルエラー: ~A~%" e)
      nil)))
```

### 3-2. 文字列分割

テキストを単語に分割する関数。空白文字（スペース、タブ、改行）で区切る。

```lisp:utils.lisp
(defun whitespace-p (char)
  "文字が空白文字かどうかを判定する。"
  (member char '(#\Space #\Tab #\Newline #\Return)))

(defun split-into-words (text)
  "テキストを単語のリストに分割する。
   空白文字で区切り、空文字列は除外する。"
  (let ((words nil)            ; 結果を格納するリスト
        (current-word nil))    ; 現在構築中の単語（文字リスト）
    (loop for char across text
          do (if (whitespace-p char)
                 ;; 空白文字 → 現在の単語を確定してリストに追加
                 (when current-word
                   (push (coerce (nreverse current-word) 'string) words)
                   (setf current-word nil))
                 ;; 通常の文字 → 単語に文字を追加
                 (push char current-word)))
    ;; ループ後、最後の単語を処理（末尾が空白でない場合）
    (when current-word
      (push (coerce (nreverse current-word) 'string) words))
    (nreverse words)))  ; push で逆順になるので反転

(defun split-into-lines (text)
  "テキストを行のリストに分割する。"
  (let ((lines nil)
        (current-line nil))
    (loop for char across text
          do (if (char= char #\Newline)
                 ;; 改行 → 現在の行を確定
                 (progn
                   (push (coerce (nreverse current-line) 'string) lines)
                   (setf current-line nil))
                 ;; 改行以外 → 行に文字を追加
                 (push char current-line)))
    ;; 最後の行を処理（改行で終わらない場合）
    (when current-line
      (push (coerce (nreverse current-line) 'string) lines))
    (nreverse lines)))
```



## 4. カウント機能

### 4-1. 基本カウント関数

行数、単語数、文字数を数える関数を実装する。Unix の `wc` コマンドに相当する機能。

```lisp:counter.lisp
(in-package :text-tools)

(defun count-lines (text)
  "テキストの行数を数える。
   改行文字の数 + 1（最終行が改行で終わらない場合）。"
  (if (zerop (length text))
      0  ; 空文字列は 0 行
      (let ((newlines (count #\Newline text)))  ; 改行の数を数える
        ;; 最後が改行でなければ +1（最終行をカウント）
        (if (char= (char text (1- (length text))) #\Newline)
            newlines
            (1+ newlines)))))

(defun count-words (text)
  "テキストの単語数を数える。"
  ;; split-into-words で分割した結果の長さ
  (length (split-into-words text)))

(defun count-chars (text &key (include-whitespace t))
  "テキストの文字数を数える。
   include-whitespace が NIL なら空白を除く。"
  (if include-whitespace
      (length text)  ; 全文字をカウント
      (count-if-not #'whitespace-p text)))  ; 空白以外をカウント
```

### 4-2. wc 関数

行数、単語数、文字数を一度に返す関数。複数の値を返すため `values` を使用する。

```lisp:counter.lisp
(defun wc (text)
  "テキストの行数、単語数、文字数を返す（多値）。
   Unix の wc コマンドに相当。"
  ;; values で複数の値を返す
  (values (count-lines text)
          (count-words text)
          (count-chars text)))

(defun wc-file (path)
  "ファイルの行数、単語数、文字数を返す。
   ファイルが読めない場合は NIL を返す。"
  (let ((contents (read-file-contents path)))
    (if contents
        (wc contents)  ; ファイル内容に対して wc を実行
        (values nil nil nil))))  ; エラー時は全て NIL

(defun wc-report (path)
  "ファイルの wc 結果を整形して表示する。"
  ;; multiple-value-bind で多値を受け取る
  (multiple-value-bind (lines words chars) (wc-file path)
    (if lines
        ;; ~8D は 8 桁の 10 進数（右寄せ）
        (format t "~8D ~8D ~8D ~A~%" lines words chars path)
        (format t "読み取りエラー: ~A~%" path))))
```

### 4-3. 単語頻度

テキスト内の単語の出現頻度を数える。ハッシュテーブルを使って効率的に集計する。

```lisp:counter.lisp
(defun word-frequency (text &key (case-sensitive nil))
  "単語の出現頻度をハッシュテーブルで返す。
   case-sensitive が NIL なら大文字小文字を区別しない。"
  (let ((freq (make-hash-table :test 'equal))  ; 文字列比較用
        (words (split-into-words text)))
    (dolist (word words)
      ;; 大文字小文字を区別しないなら小文字に統一
      (let ((key (if case-sensitive
                     word
                     (string-downcase word))))
        ;; gethash の第3引数はデフォルト値（未登録なら 0）
        (incf (gethash key freq 0))))
    freq))

(defun top-words (text n &key (case-sensitive nil))
  "出現頻度の高い上位 N 個の単語を返す。
   (単語 . 出現回数) のリストを返す。"
  (let* ((freq (word-frequency text :case-sensitive case-sensitive))
         (pairs nil))
    ;; ハッシュテーブルをリストに変換
    (maphash (lambda (word count)
               (push (cons word count) pairs))  ; (単語 . 回数) のペア
             freq)
    ;; cdr（回数）で降順ソートして上位 N 個を返す
    (subseq (sort pairs #'> :key #'cdr)
            0
            (min n (length pairs)))))  ; N が全体数より大きい場合に対応

(defun print-word-frequency (text &key (top 10) (case-sensitive nil))
  "単語の出現頻度を整形して表示する。"
  (let ((top-words (top-words text top :case-sensitive case-sensitive)))
    (format t "~%=== 単語出現頻度（上位 ~D）===~%" top)
    (format t "~10A ~8A~%" "単語" "回数")
    ;; ~10,,,'-A は 10 文字幅で '-' で埋める
    (format t "~10,,,'-A ~8,,,'-A~%" "" "")
    (dolist (pair top-words)
      (format t "~10A ~8D~%" (car pair) (cdr pair)))))
```



## 5. 検索・置換機能

### 5-1. 文字列検索

テキスト内で文字列を検索する機能。すべての出現位置を返す。

```lisp:search.lisp
(in-package :text-tools)

(defun find-all-positions (text pattern &key (case-sensitive t))
  "テキスト内でパターンが出現するすべての位置を返す。"
  (let ((positions nil)
        ;; 大文字小文字を区別しないなら両方を小文字に
        (text-to-search (if case-sensitive
                            text
                            (string-downcase text)))
        (pattern-to-find (if case-sensitive
                             pattern
                             (string-downcase pattern))))
    ;; search で順次検索、見つかるたびに位置を記録
    (loop for pos = (search pattern-to-find text-to-search)
                then (search pattern-to-find text-to-search
                             :start2 (1+ pos))  ; 前回位置の次から検索
          while pos  ; 見つからなくなるまで
          do (push pos positions))
    (nreverse positions)))

(defun count-occurrences (text pattern &key (case-sensitive t))
  "テキスト内でパターンが出現する回数を返す。"
  (length (find-all-positions text pattern :case-sensitive case-sensitive)))
```

### 5-2. grep 機能

パターンを含む行を抽出する機能。Unix の `grep` コマンドに相当する。

```lisp:search.lisp
(defun grep (text pattern &key (case-sensitive t) (line-numbers nil))
  "パターンを含む行のリストを返す。
   line-numbers が T なら (行番号 . 行内容) のリストを返す。"
  (let ((lines (split-into-lines text))
        (results nil)
        (pattern-to-find (if case-sensitive
                             pattern
                             (string-downcase pattern))))
    ;; 各行を走査、行番号は 1 から開始
    (loop for line in lines
          for line-num from 1
          do (let ((line-to-search (if case-sensitive
                                       line
                                       (string-downcase line))))
               ;; パターンが含まれていれば結果に追加
               (when (search pattern-to-find line-to-search)
                 (push (if line-numbers
                           (cons line-num line)  ; (番号 . 行)
                           line)                 ; 行のみ
                       results))))
    (nreverse results)))

(defun grep-file (path pattern &key (case-sensitive t))
  "ファイル内でパターンを含む行を検索して表示する。"
  (let ((contents (read-file-contents path)))
    (if contents
        (let ((matches (grep contents pattern
                             :case-sensitive case-sensitive
                             :line-numbers t)))
          (if matches
              (progn
                (format t "~A:~%" path)
                ;; 各マッチを表示（行番号: 行内容）
                (dolist (match matches)
                  (format t "  ~4D: ~A~%" (car match) (cdr match)))
                (format t "~%~D 件の一致~%" (length matches)))
              (format t "~A: 一致なし~%" path)))
        (format t "ファイルを開けません: ~A~%" path))))
```

### 5-3. 置換機能

テキスト内の文字列を置換する機能。非破壊的に新しい文字列を返す。

```lisp:search.lisp
(defun replace-all (text old-str new-str &key (case-sensitive t))
  "テキスト内のすべての old-str を new-str に置換した新しい文字列を返す。"
  (if (zerop (length old-str))
      text  ; 空文字列の置換は何もしない
      (let ((result (make-array 0
                                :element-type 'character
                                :adjustable t        ; サイズ変更可能
                                :fill-pointer 0))    ; 可変長配列
            (text-to-search (if case-sensitive
                                text
                                (string-downcase text)))
            (pattern (if case-sensitive
                         old-str
                         (string-downcase old-str)))
            (start 0))  ; 検索開始位置
        (loop for pos = (search pattern text-to-search :start2 start)
              while pos
              do (progn
                   ;; マッチ前の部分を結果にコピー
                   (loop for i from start below pos
                         do (vector-push-extend (char text i) result))
                   ;; 置換文字列を追加
                   (loop for char across new-str
                         do (vector-push-extend char result))
                   ;; 次の検索開始位置を更新
                   (setf start (+ pos (length old-str))))
              finally
                 ;; 残りの部分をコピー
                 (loop for i from start below (length text)
                       do (vector-push-extend (char text i) result)))
        (coerce result 'string))))

(defun replace-in-file (path old-str new-str &key (case-sensitive t)
                                                  (backup t))
  "ファイル内のすべての old-str を new-str に置換する。
   backup が T なら元ファイルを .bak として保存する。"
  (let ((contents (read-file-contents path)))
    (if contents
        (let ((new-contents (replace-all contents old-str new-str
                                         :case-sensitive case-sensitive))
              (count (count-occurrences contents old-str
                                        :case-sensitive case-sensitive)))
          ;; バックアップを作成（安全のため）
          (when backup
            (write-file-contents (concatenate 'string path ".bak") contents))
          ;; 新しい内容を書き込む
          (write-file-contents path new-contents)
          (format t "~D 箇所を置換しました~%" count)
          count)
        (progn
          (format t "ファイルを開けません: ~A~%" path)
          nil))))
```



## 6. メインインターフェース

### 6-1. 対話的インターフェース

ユーザーが対話的にツールを使えるインターフェースを作成する。

```lisp:main.lisp
(in-package :text-tools)

(defun print-menu ()
  "メニューを表示する。"
  (format t "~%=== テキスト処理ツール ===~%")
  (format t "1. ファイルの文字数・単語数・行数を表示~%")
  (format t "2. 単語の出現頻度を表示~%")
  (format t "3. パターン検索（grep）~%")
  (format t "4. 文字列置換~%")
  (format t "5. 終了~%")
  (format t "~%選択 (1-5): "))

(defun prompt-read (prompt)
  "プロンプトを表示して入力を読み取る。"
  (format t "~A" prompt)
  (force-output)     ; 出力バッファを強制フラッシュ
  (read-line))       ; 1行読み取り

(defun run-wc ()
  "wc 機能を実行する。"
  (let ((path (prompt-read "ファイルパス: ")))
    (wc-report path)))

(defun run-word-freq ()
  "単語頻度機能を実行する。"
  (let* ((path (prompt-read "ファイルパス: "))
         (contents (read-file-contents path)))
    (if contents
        (print-word-frequency contents :top 20)
        (format t "ファイルを開けません~%"))))

(defun run-grep ()
  "grep 機能を実行する。"
  (let ((path (prompt-read "ファイルパス: "))
        (pattern (prompt-read "検索パターン: ")))
    (grep-file path pattern)))

(defun run-replace ()
  "置換機能を実行する。"
  (let ((path (prompt-read "ファイルパス: "))
        (old-str (prompt-read "置換前: "))
        (new-str (prompt-read "置換後: ")))
    (replace-in-file path old-str new-str)))

(defun main ()
  "メインループ。対話的にツールを実行する。"
  (loop
    (print-menu)
    (force-output)
    (let ((choice (read-line)))
      ;; string= で文字列比較（= は数値用）
      (cond
        ((string= choice "1") (run-wc))
        ((string= choice "2") (run-word-freq))
        ((string= choice "3") (run-grep))
        ((string= choice "4") (run-replace))
        ((string= choice "5")
         (format t "終了します~%")
         (return))  ; loop を抜ける
        (t (format t "無効な選択です~%"))))))
```



## 7. システム定義

### 7-1. ASDF ファイル

プロジェクト全体をまとめる ASDF システム定義を作成する。

```lisp:text-tools.asd
(defsystem "text-tools"
  :version "1.0.0"
  :author "Your Name"
  :license "MIT"
  :description "テキスト処理ユーティリティ"
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "counter")
               (:file "search")
               (:file "main")))
```

### 7-2. 使用方法

```lisp
;; ASDFがシステム定義(.asd)を探すディレクトリを追加
;; カレントディレクトリを追加
(push *default-pathname-defaults* asdf:*central-registry*)
;; あるいは、絶対パスを指定
(push #P"/Path/To/text-tools/" asdf:*central-registry*)  

;; システムをロード
(ql:quickload "text-tools")

;; パッケージに切り替え
(in-package :text-tools)

;; 対話的インターフェースを起動
(main)

;; または個別の関数を使用
(wc-file "sample.txt")
(grep-file "sample.txt" "error")
(print-word-frequency (read-file-contents "sample.txt"))
```

## 8. テストとデバッグ

### 8-1. 手動テスト

各関数を REPL で個別にテストする。

```lisp
;; テスト用テキスト
(defparameter *test-text*
  "Hello World
This is a test.
Hello again, World!
Testing one two three.")

;; カウント機能のテスト
(count-lines *test-text*)             ; → 4
(count-words *test-text*)             ; → 12
(count-chars *test-text*)             ; → 68

;; wc のテスト
(multiple-value-bind (l w c) (wc *test-text*)
  (format t "~D lines, ~D words, ~D chars~%" l w c))  
;; → 4 lines, 12 words, 68 chars

;; 単語頻度のテスト
(word-frequency *test-text*)
(top-words *test-text* 5)

;; grep のテスト
(grep *test-text* "Hello")
;; → ("Hello World" "Hello again, World!")

;; 置換のテスト
(replace-all *test-text* "Hello" "Hi")
;; → "Hi World..." （Hello が Hi に置換される）
```

### 8-2. 日本語テキストのテスト

日本語を含むテキストもテストする。

```lisp
;; 日本語テスト用テキスト
(defparameter *japanese-text*
  "こんにちは 世界
これは テスト です
こんにちは また 世界")

;; カウント機能のテスト
(count-lines *japanese-text*)         ; → 3
(count-words *japanese-text*)         ; → 8

;; grep のテスト
(grep *japanese-text* "こんにちは")
;; → ("こんにちは 世界" "こんにちは また 世界")

;; 置換のテスト
(replace-all *japanese-text* "世界" "ワールド")
;; → "こんにちは ワールド
;;    これは テスト です
;;    こんにちは また ワールド"
```

### 8-3. エッジケースのテスト

特殊なケースもテストする。

```lisp
;; 空文字列
(count-lines "")                      ; → 0
(count-words "")                      ; → 0
(wc "")                               ; → 0, 0, 0

;; 改行のみ
(count-lines (format nil "~%~%~%"))   ; → 3

;; 空白のみ
(split-into-words "   ")              ; → NIL

;; パターンが見つからない
(grep *test-text* "xyz")              ; → NIL

;; 置換対象がない
(replace-all *test-text* "xyz" "abc") ; → 元のテキストと同じ
```



## 9. 拡張のアイデア

### 9-1. 正規表現対応

`cl-ppcre` ライブラリを使って正規表現に対応させる。

```lisp
;; cl-ppcre を使った grep
(ql:quickload "cl-ppcre")

(defun grep-regex (text pattern)
  "正規表現でパターンを含む行を返す。"
  (let ((lines (split-into-lines text)))
    ;; cl-ppcre:scan は正規表現マッチを行う
    (remove-if-not (lambda (line)
                     (cl-ppcre:scan pattern line))
                   lines)))

;; 使用例（.* は任意の文字列にマッチ）
(grep-regex *test-text* "Hello.*World")
;; → ("Hello World" "Hello again, World!")
```

### 9-2. コマンドライン対応

スクリプトとしてコマンドラインから実行できるようにする。

```lisp:run.lisp
(require :asdf)
(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system "text-tools")

(defun cli-main ()
  "コマンドライン引数を処理する。"
  (let ((args (uiop:command-line-arguments)))
    (cond
      ((null args)
       (format t "Usage: text-tools <command> [args]~%")
       (format t "Commands:~%")
       (format t "  wc <file>...         行数・単語数・文字数~%")
       (format t "  grep <pattern> <file> パターン検索~%")
       (format t "  freq <file>          単語頻度~%"))
      ((string= (first args) "wc")
       (dolist (file (rest args))
         (text-tools:wc-report file)))
      ((string= (first args) "grep")
       (when (>= (length args) 3)
         (text-tools:grep-file (third args) (second args))))
      ((string= (first args) "freq")
       (when (second args)
         (let ((contents (text-tools:read-file-contents (second args))))
           (when contents
             (text-tools:print-word-frequency contents)))))
      (t
       (format t "Unknown command: ~A~%" (first args))))))

(cli-main)
```

コマンドラインから実行

```shell
## wc コマンド
sbcl --script run.lisp wc sample.txt

## grep コマンド
sbcl --script run.lisp grep "error" logfile.txt

## 単語頻度
sbcl --script run.lisp freq document.txt
```

### 9-3. 実行ファイル作成

SBCLでは `sb-ext:save-lisp-and-die` という関数を使用し、引数に `:executable t` を指定することで、単独で実行可能なファイルを作成できます。

配布先のPCにSBCLやLispの環境がインストールされていなくても、そのファイルだけでプログラムを動かすことができます。ただし、実行ファイルにはLispコンパイラやランタイムがすべて含まれるため、ファイルサイズは大きくなります。

また、Windowsで作った実行ファイルはLinuxでは動きませんし、Intel Macで作ったものは（Rosettaなしの）Apple Silicon Macでは動きません。共有ライブラリのバージョンが古い場合も、エラーが発生することがあります。


実行ファイルの作成には、コマンドラインからSBCLを起動し、run.lispをロードした後、作成します（VSCode上やEmacs上ではエラーになりました）。

```lisp
(load "run.lisp")

;; 実行ファイルを作成して保存する
(sb-ext:save-lisp-and-die "text-tools.exe"     ;; 出力ファイル名
                          :toplevel #'cli-main ;; 最初に実行する関数
                          :executable t)       ;; 実行可能バイナリにする

```

```shell
## wc コマンド
text-tools.exe wc sample.txt

## grep コマンド
text-tools.exe grep "error" logfile.txt

## 単語頻度
text-tools.exe freq document.txt
```

### 9-4. 並列処理

複数ファイルを並列に処理する。

```lisp
(ql:quickload "bordeaux-threads")

(defun wc-files-parallel (paths)
  "複数ファイルを並列に処理する。"
  (let ((results (make-hash-table :test 'equal))
        (lock (bt:make-lock))
        (threads nil))
    (dolist (path paths)
      ;; let で path をコピー（重要！）
      (let ((p path))
        (push (bt:make-thread
               (lambda ()
                 (multiple-value-bind (l w c) (text-tools:wc-file p)
                   (bt:with-lock-held (lock)
                     (setf (gethash p results) (list l w c))))))
              threads)))
    (dolist (thread threads)
      (bt:join-thread thread))
    results))
```

使用例

```lisp
;; 複数ファイルを並列処理
(defparameter *results*
  (wc-files-parallel '("file1.txt" "file2.txt" "file3.txt")))

;; 結果を表示
(maphash (lambda (path counts)
           (format t "~A: ~{~D ~}~%" path counts))
         *results*)
;; file1.txt: 10 50 300
;; file2.txt: 20 100 600
;; file3.txt: 15 75 450
```


## 10. 練習課題

### 課題1：行の重複除去

テキストから重複する行を除去する関数を作れ。

```lisp
(unique-lines (format nil "a~%b~%a~%c~%b"))
;; → "a
;;    b
;;    c"
```

**解答**

```lisp
(defun unique-lines (text)
  "重複する行を除去したテキストを返す。"
  (let ((lines (split-into-lines text))
        (seen (make-hash-table :test 'equal))  ; 出現済みの行を記録
        (result nil))
    (dolist (line lines)
      ;; まだ出現していない行だけを追加
      (unless (gethash line seen)
        (setf (gethash line seen) t)  ; 出現済みとしてマーク
        (push line result)))
    ;; ~{~A~^~%~} はリストを改行区切りで出力
    (format nil "~{~A~^~%~}" (nreverse result))))
```



### 課題2：行の並び替え

テキストの行をアルファベット順にソートする関数を作れ。

```lisp
(sort-lines (format nil "banana~%apple~%cherry"))
;; → apple
;;   banana
;;   cherry
```

**解答**

```lisp
(defun sort-lines (text &key (descending nil))
  "行をソートしたテキストを返す。"
  (let ((lines (split-into-lines text)))
    (format nil "~{~A~^~%~}"
            ;; copy-list で元リストを保護（sort は破壊的）
            (sort (copy-list lines)
                  (if descending #'string> #'string<)))))
```



### 課題3：文字種別カウント

テキスト内の文字種別（アルファベット、数字、空白、その他）をカウントする関数を作れ。

**解答**

```lisp
(defun count-char-types (text)
  "文字種別ごとのカウントを返す。"
  (let ((alpha 0) (digit 0) (space 0) (other 0))
    (loop for char across text
          do (cond
               ((alpha-char-p char) (incf alpha))   ; アルファベット
               ((digit-char-p char) (incf digit))   ; 数字
               ((whitespace-p char) (incf space))   ; 空白
               (t (incf other))))                   ; その他
    ;; プロパティリスト形式で返す
    (list :alphabetic alpha
          :digit digit
          :whitespace space
          :other other)))

;; 使用例
(count-char-types "Hello 123!")
;; → (:ALPHABETIC 5 :DIGIT 3 :WHITESPACE 1 :OTHER 1)
```



### 課題4：前後の空行を除去

テキストの先頭と末尾の空行を除去する関数を作れ。

**解答**

```lisp
(defun trim-blank-lines (text)
  "先頭と末尾の空行を除去する。"
  (let ((lines (split-into-lines text)))
    ;; 先頭の空行を除去（長さ 0 の行を削除）
    (loop while (and lines (zerop (length (first lines))))
          do (pop lines))
    ;; 末尾の空行を除去（リストを反転して先頭を処理）
    (setf lines (nreverse lines))
    (loop while (and lines (zerop (length (first lines))))
          do (pop lines))
    ;; 元の順序に戻して文字列化
    (format nil "~{~A~^~%~}" (nreverse lines))))

;; 使用例
(trim-blank-lines (format nil "~%~%Line 1~%Line 2~%~%~%"))
;; → Line 1
;;   Line 2
```



### 課題5：差分検出

2つのテキストの差分（追加・削除された行）を検出する簡易的な関数を作れ。

**解答**

```lisp
(defun simple-diff (text1 text2)
  "2つのテキストの差分を表示する（簡易版）。"
  (let ((lines1 (split-into-lines text1))
        (lines2 (split-into-lines text2))
        (set1 (make-hash-table :test 'equal))  ; text1 の行セット
        (set2 (make-hash-table :test 'equal))) ; text2 の行セット
    ;; 各テキストの行をセットに登録
    (dolist (line lines1) (setf (gethash line set1) t))
    (dolist (line lines2) (setf (gethash line set2) t))
    ;; text1 にあって text2 にない行 = 削除された行
    (format t "--- 削除された行 ---~%")
    (dolist (line lines1)
      (unless (gethash line set2)
        (format t "- ~A~%" line)))
    ;; text2 にあって text1 にない行 = 追加された行
    (format t "~%--- 追加された行 ---~%")
    (dolist (line lines2)
      (unless (gethash line set1)
        (format t "+ ~A~%" line)))))

;; 使用例
(simple-diff (format nil "Line 1~%Line 2~%Line 3")
             (format nil "Line 2~%Line 3~%Line 4"))
;; →　
;; --- 削除された行 ---
;; - Line 1
;; --- 追加された行 ---
;; + Line 4
```



## 11. まとめ

### 作成した機能

| 関数 | 機能 |
|------|------|
| `read-file-contents` | ファイル読み込み（UTF-8対応） |
| `write-file-contents` | ファイル書き込み（UTF-8対応） |
| `split-into-words` | 単語分割 |
| `split-into-lines` | 行分割 |
| `count-lines/words/chars` | 各種カウント |
| `wc` | 行・単語・文字数を一括取得 |
| `word-frequency` | 単語頻度分析 |
| `grep` | パターン検索 |
| `replace-all` | 文字列置換 |

### 使用した技術

- **パッケージ**：機能をモジュール化
- **ハッシュテーブル**：単語頻度の集計
- **多値**：wc で複数の値を返す
- **エラー処理**：ファイル操作の安全性確保
- **高階関数**：`remove-if-not`、`count-if` など
- **外部形式**：`:external-format :utf-8` で日本語対応

### 設計のポイント

- 小さな関数を組み合わせて大きな機能を作る
- ユーティリティ関数は再利用可能に設計
- エラー処理を適切に行う
- テストしやすい関数設計
- 日本語などのマルチバイト文字に対応

