# Level 1 (Lisp is Different)

## 1. Lispとは何か — “計算ではなく思考を扱う機械” の構想

### 1-1. 誕生の背景：コンピュータ観の転換

1950年代後半、世界のコンピュータ研究は **Fortran を中心に数値計算** へ向かっていた。しかし MIT の **John McCarthy（ジョン・マッカーシー）** は、他とは異なる仮説に到達していた。

> **「人間の推論・知識・記号操作は、数値と同じように“計算可能”ではないか」**

つまり彼は、**AIを成立させるための言語** を構想していた。

### 1-2. 設計理念

| マッカーシーの理念     | 内容                            |
| ------------- | ----------------------------- |
| 記号処理の第一級市民化   | 値を数値としてではなく「意味を持つ記号」として扱う     |
| リスト構造の中心化     | 木や構造データを自然に扱うための最低限にして最大の表現構造 |
| 関数適用ベースの評価    | 「命令列の実行」から「式の評価」へ概念転換         |
| プログラム = データ構造 | 言語自身を加工・構築・生成できる世界観（マクロの伏線）   |

これにより Lisp は **「自己評価可能なプログラミング言語」** として誕生し、コンピュータ史で唯一無二の思想的革命を起こした。

### 1-3. 歴史的エピソード

マッカーシーは Lisp の評価器を「理論モデル」として提示し、**「実装しなくてよい」** と述べたが、学生の **Steve Russell** がそれを「理論だが動作可能」と判断し実装した。

> その瞬間、コンピュータ史上初の
> **“プログラムをデータとして扱える言語宇宙”** が生まれた。

これは Python、Java、C++ でも完全には再現できない世界観である。


### 1-4. Lispの進化と“方言の森”

Lispは柔軟すぎたため、研究機関ごとに別の言語へ“突然変異”した。

| 系統                | 機関             | 特徴                  |
| ----------------- | -------------- | ------------------- |
| MacLisp           | MIT            | 実用性向上、Common Lisp基盤 |
| InterLisp         | BBN／Xerox PARC | 強力なIDE・研究特化         |
| Scheme            | MIT            | 数理主義・ミニマリズム         |
| Lisp Machine Lisp | Symbolics      | Lisp専用OS／ハードウェア     |

Lispは単なる“言語”ではなく、**「自分で進化可能な概念基盤」** だった。

しかし互換性問題が深刻化し、研究成果共有が難しくなった。

### 1-5. 標準化の決断 — Common Lispの誕生

1980年代、Unix と C 言語が産業界の標準となり、Lisp マシン企業も競争に敗れ、Lispは分裂によって **絶滅危機** に陥った。

そこでコミュニティは歴史的合意に至る。

> **「代表的機能を統合し、産業向け大規模言語を規格化せよ」**

これが **Common Lisp** の誕生であり、1994年に **ANSI 規格** として正式標準化された。

Common Lisp は研究用にとどまらず、**重厚で堅牢な“長期運用型言語”** として完成した。

## 2. Common Lispの特徴と教育的価値

| 特徴                | 解説                 |
| ----------------- | ------------------ |
| マクロ               | 文法を自作できるメタ言語的仕組み   |
| CLOS（多重ディスパッチOOP） | Python・Javaを超える柔軟性 |
| 動的＋静的宣言両立         | 性能調整と高速化が可能        |
| 条件システム            | 回復可能エラーの仕組み        |
| REPL文化            | ライブ実験・思考試作向け       |

特に CLOS は、**“後から構造を変えられるOOP”** であり、Java型の固定クラス設計とは対極に位置する。

#### 得意・不得意領域

| 得意         | 理由                   |
| ---------- | -------------------- |
| AI・探索・記号処理 | 記号操作と言語設計が容易         |
| DSL        | マクロで文法レベル拡張が可能       |
| 大規模ロジック    | 後から改変可能、技術的負債が蓄積しにくい |

| 不得意         | 理由            |
| ----------- | ------------- |
| モバイル開発      | エコシステムの不足     |
| “最新ライブラリ依存” | Python優位分野が多い |


## 3. REPLとは何か —「思考実験装置としてのプログラミング」

REPLは **Read → Eval → Print → Loop** の循環システムであり、コードは「設計後に書く」ものではなく、**思考と同時に書き、修正し、成長させるもの** である。

これが **研究者・発明者・言語設計者向き** の思考環境である理由である。


## 4.　SBCLとは何か ― 処理系選択の意義

SBCL (Steel Bank Common Lisp) は、Common Lisp 処理系の中でも **高速性・標準準拠性・堅牢性** に優れ、研究用途から商用利用まで幅広く採用されている。

**主な特徴**

* ネイティブコンパイルによる高速実行
* ANSI Common Lisp 標準準拠
* 強力なデバッガとコンディションシステム
* マルチプラットフォーム対応（Linux / macOS / Windows）
* オープンソース（コア開発が継続）

### 4-1. 他処理系との比較

| 処理系              | 最大の強み            | 推奨用途         |
| ---------------- | ---------------- | ------------ |
| **SBCL**         | 高速・標準準拠・本番利用に耐える | 学習・研究・実プロダクト |
| CCL (Clozure CL) | 起動高速・FFIが扱いやすい   | macOS中心開発    |
| ECL              | C言語に変換可能・埋め込み向け  | IoT・組み込み     |
| CLISP            | 軽量・移植性           | 教育・携帯用途      |

> **指導者注**：SBCLを採用する理由は「学習目的としての簡便性」ではなく **「プロダクション品質の開発体験を早期に提供すること」** にある。

### 4-2　SBCL のインストール

OSに応じて以下の方法で導入する。UbuntuでSBCLを使う場合は `rlwrap` を併せてインストールしておくことを強く推奨する。`sbcl`はコマンドラインで履歴移動やカーソルの前後移動ができない。`rlwrap` はコマンドラインの編集操作を快適にするためのラッパーで、対話型開発や REPL で扱うなら、rlwrap は「ほぼ必須」レベルの便利ツールである。

SBCLの最新バージョンは外部ライブラリがまだ対応していない場合があり、QUICKLISPでライブラリのインストール時にエラーが発生することがある。外部ライブラリを使う場合は、1つ前あたりのバージョンをインストールしておく方がいいようだ。

https://sourceforge.net/projects/sbcl/files/sbcl/

https://www.sbcl.org/getting.html##compile

#### Linux (Ubuntu/Debian)

```bash
sudo apt update
sudo apt install sbcl rlwrap
```

#### macOS（Homebrew利用）

```bash
brew install sbcl rlwrap
```

#### Windows（Scoop推奨）
[Scoop](ScoopはWindows用のパッケージ管理ツールで)はWindows用のパッケージ管理ツールです。
```powershell
scoop install sbcl
```

動作確認：

```bash
sbcl --version
```

### 4-3　REPL操作基礎

REPLは **試行錯誤型プログラミング** に最適化された Lisp の中心技術である。

`sbcl`で起動できるが、`rlwrap`をインストールしてある場合は、
```bash
rlwrap sbcl
```
として起動する。

```:入力例
(+ 1 2 3)
(defparameter *name* "太郎")
*name*
```

```:出力例
6
"太郎"
```

終了する場合は、`(quit)` あるいは `(exit)` と入力する。前後にカッコをつけなければならない。

```
(quit)
```

### 4-4　Quicklispによるライブラリ管理

#### Quicklispの役割

Quicklispは多様な外部ライブラリを依存関係解決付きで利用可能にする仕組みであり、Python の pip、Rust の cargo に相当する。

特徴：

* ライブラリ検索・取得・更新が容易
* 依存関係の自動解決
* プロジェクト開発に必須のインフラ

#### セットアップ手順

コマンドライン上で

```bash
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp
```

以下は`sbcl`起動後

```lisp:SBCLで実行
(quicklisp-quickstart:install)
```

```lisp:自動ロード設定
(ql:add-to-init-file)
```

```lisp:動作確認
(ql:quickload :alexandria)
```
#### 実用ライブラリの導入 ― Quicklisp活用の第一歩

Common Lisp の強みは言語自体の柔軟性にあるが、実務レベルの開発では外部ライブラリの活用は不可欠である。本節では、Web通信・データ処理・ユーティリティ向けに広く利用されるライブラリ群を導入する。


| ライブラリ名         | 機能分類    | 主な用途                      | 備考         |
| -------------- | ------- | ------------------------- | ---------- |
| **dexador**    | HTTP通信  | GET/POST、ヘッダ操作、JSON API連携 | 実用性高く標準選択肢 |
| **cl-json**    | JSON処理  | JSON ⇔ Lisp 変換            | 軽量・扱いやすい   |
| **alexandria** | ユーティリティ | 関数拡張、操作支援                 | 開発生産性向上    |
| **local-time** | 日時処理    | タイムスタンプ、ログ                | 記録処理向け     |

#### ライブラリインストール

```lisp
(ql:quickload '(:dexador :cl-json :alexandria :local-time))
```

ロード成功時には、以下の様に表示される。

```
(:DEXADOR :CL-JSON :ALEXANDRIA :LOCAL-TIME)
```

> **補足**：初回ロード時は依存関係のコンパイルが行われ時間を要するが、次回以降は高速化される。

#### VSCode を使う

VSCode上で実行するには、拡張機能のAliveをインストールします。

![image.png](https://qiita-image-store.s3.ap-northeast-1.amazonaws.com/0/864075/d0c43585-a77e-4ded-ad33-dea8c8e6f0ed.png)

詳しくはこちらで

https://zenn.dev/k41531/articles/6a68fa6f1bb08c



## 5. 現在利用できる Common Lisp 処理系

Common Lisp には複数の処理系が存在し、用途や環境に応じて選択できる。本節では2025年現在、活発に開発・利用されている主要な処理系を紹介する。

### 5-1. オープンソース処理系

Common Lisp Community Survey 2024 の結果によると、全体の約88%がSBCLユーザーであり、SBCL が事実上の標準的な選択肢となっている。

| 処理系 | 特徴 | 対応OS | ライセンス |
|--------|------|--------|------------|
| **SBCL** | 高速・標準準拠・月次リリースで安定 | Linux, macOS, Windows, FreeBSD, NetBSD, OpenBSD, Solaris | 修正BSD/パブリックドメイン |
| **CCL** | 起動高速・FFIが扱いやすい | macOS, Linux, Windows, FreeBSD, Solaris | Apache 2.0 |
| **ECL** | C言語に変換可能・省メモリ | Linux, macOS, Windows, FreeBSD, NetBSD, OpenBSD, Solaris, Android | LGPL |
| **CLISP** | 軽量・バイトコードで移植性が高い | Linux, macOS, Windows, その他多数 | GPL |
| **ABCL** | JVM上で動作・Javaコード呼び出し可能 | JVMが動作する全OS | GPL |
| **Clasp** | LLVMベース・C++連携 | Linux, macOS | LGPL |

**補足**
- **SBCL** は最も幅広いプラットフォームをサポートしており、Linux では x86, x86_64, ARM, ARM64, PowerPC, MIPS, SPARC など多くのアーキテクチャに対応している
- **CCL** は Raspberry Pi（Linux ARM 32bit）でも動作実績がある
- **ECL** は Android への移植実績があり、組み込み用途に適している
- **CLISP** はバイトコードインタプリタのため、新しいプラットフォームへの移植が比較的容易

### 5-2. 商用処理系

商用処理系は強力なIDEやサポートを提供しており、企業での本番利用に適している。いずれも機能制限付きの無料版を提供している。

| 処理系 | 開発元 | 対応OS | 無料版の制限 |
|--------|--------|--------|--------------|
| **Allegro CL** | Franz Inc. | Linux, macOS, Windows, FreeBSD | ヒープサイズ制限 |
| **LispWorks** | LispWorks Ltd. | Windows, macOS, Linux (x86_64/ARM/ARM64), FreeBSD, Solaris, iOS, Android | ヒープサイズ・使用時間制限 |

**LispWorks の特筆すべき点**
- Apple Silicon (M1/M2/M3) にネイティブ対応
- iOS / Android 向けランタイムを提供（モバイルアプリ開発が可能）
- ARM64 Linux に対応（Raspberry Pi 4 等で動作可能）

商用処理系の無料版は、ANSI Common Lispのコードを書いて学習し、実行はオープンソース処理系で行うという使い方も可能である。Emacsに馴染めない場合の選択肢として検討する価値がある。

### 5-3. 処理系選択の指針

| 目的 | 推奨処理系 |
|------|-----------|
| 初めてCommon Lispを学ぶ | SBCL |
| 本格的な開発・本番運用 | SBCL |
| macOSでの開発（Intel/Apple Silicon） | SBCL または LispWorks |
| 組み込み・省リソース環境 | ECL |
| Javaとの連携が必要 | ABCL |
| Raspberry Pi で動かしたい | SBCL (ARM), CCL (ARM 32bit), LispWorks (ARM64) |
| IDEを使いたい（Emacs以外） | LispWorks または Allegro CL（無料版） |
| モバイルアプリ開発 | LispWorks（iOS/Android Runtime） |


## 6. マイコン向け Lisp — uLisp

Common Lisp の完全な処理系をマイコンで動作させることは困難だが、Lisp のサブセットを実装した **uLisp** がマイコン向けに提供されている。

### 6-1. uLisp とは

uLisp は、限られたRAMを持つマイコン上で動作するよう設計された Lisp 処理系である。Common Lisp のサブセットを実装しており、uLisp で書いたプログラムは Common Lisp でも動作する（逆は必ずしも成り立たない）。

**特徴**
- インタプリタ方式（コンパイル不要で即座に実行）
- ガベージコレクション搭載
- Arduino 拡張（GPIO操作、PWM、I2C、SPI等）
- Wi-Fi 拡張（ESP32系）
- グラフィックス拡張（TFT対応ボード）
- ワークスペースの保存・読み込み機能

### 6-2. uLisp 対応ボード一覧

#### Arduino 系（AVR）

| ボード | MCU | RAM | Flash | 備考 |
|--------|-----|-----|-------|------|
| Arduino Uno / Nano | ATmega328 | 2KB | 32KB | 最小構成、シンプルな用途向け |
| Arduino Mega 2560 | ATmega2560 | 8KB | 256KB | より複雑なアプリケーション向け |
| Arduino Nano Every | ATmega4809 | 6KB | 48KB | 低コスト |
| AVR128DA48 / DB48 | AVR DA/DB | 16KB | 128KB | 高性能AVR |

#### ARM 系

| ボード | MCU | RAM | Flash | 備考 |
|--------|-----|-----|-------|------|
| Arduino Zero / MKRZero | SAMD21 | 32KB | 256KB | SDカード対応（MKRZero） |
| Adafruit Feather M0/M4 | SAMD21/51 | 32KB/192KB | 256KB/512KB | DataFlash搭載版あり |
| Adafruit PyGamer / PyBadge | SAMD51 | 192KB | 512KB | TFTディスプレイ搭載 |
| BBC micro:bit V2 | nRF52833 | 128KB | 512KB | 教育向け |
| Teensy 4.0 / 4.1 | iMXRT1062 | 1MB | 2MB | **最高性能**（600MHz） |
| Arduino Uno R4 | RA4M1 | 32KB | 256KB | 5V動作 |

#### Raspberry Pi Pico 系（RP2040 / RP2350）

| ボード | MCU | RAM | Flash | 備考 |
|--------|-----|-----|-------|------|
| **Raspberry Pi Pico** | RP2040 | 264KB | 2MB | **対応** |
| Raspberry Pi Pico W | RP2040 | 264KB | 2MB | Wi-Fi搭載 |
| **Raspberry Pi Pico 2** | RP2350 | 520KB | 4MB | **対応**（ARM/RISC-V） |
| Adafruit Feather RP2040/RP2350 | RP2040/RP2350 | 264KB/520KB | 8MB | Adafruit版 |
| Seeed XIAO RP2040 | RP2040 | 264KB | 2MB | 超小型 |

#### ESP32 系

| ボード | MCU | RAM | Flash | 備考 |
|--------|-----|-----|-------|------|
| ESP32 各種 | ESP32 | 520KB | 4MB | Wi-Fi/Bluetooth対応 |
| ESP32-S2 | ESP32-S2 | 320KB | 4MB | USB対応 |
| ESP32-C3 | ESP32-C3 (RISC-V) | 400KB | 4MB | RISC-Vコア |
| ESP32 + TFTディスプレイ | ESP32系 | - | - | グラフィックス拡張対応 |

#### M5Stack 系（ESP32ベース）

| ボード | 対応状況 | 備考 |
|--------|----------|------|
| **M5Stack Basic/Gray/Fire** | ○（有志版） | ESP32ベース、TFTディスプレイ搭載 |
| **M5Stack Core2** | ○（有志版） | タッチスクリーン |
| **M5StickC / Plus** | ○（有志版） | 超小型 |
| **M5Stack Cardputer** | ○ | キーボード付き、ポータブルLispマシンとして利用可能 |

**注意**: M5Stack 向けの uLisp は公式ではなく、有志による拡張版（ulisp-esp-m5stack）が GitHub で公開されている。TFTディスプレイやボタンなど M5Stack 固有のハードウェアを Lisp から制御できる。

#### RISC-V 系

| ボード | MCU | RAM | Flash | 備考 |
|--------|-----|-----|-------|------|
| Sipeed MAiX | Kendryte K210 | 8MB | 16MB | デュアルコア 400MHz |

### 6-3. uLisp 非対応のマイコン

以下のマイコンは、RAM やアーキテクチャの制約により uLisp が動作しない、または公式サポートされていない：

| マイコン | 理由 |
|----------|------|
| **Arduino Uno R3 以前の一部** | RAM 2KB未満のものは不可 |
| **ATtiny シリーズ** | RAM/Flash が不足 |
| **STM32 シリーズ** | 公式未対応（移植プロジェクトは存在） |

### 6-4. uLisp の使用例

Raspberry Pi Pico で LED を点滅させる例：

```lisp
(defun blink (&optional x)
  (pinmode :led-builtin t)
  (digitalwrite :led-builtin x)
  (delay 1000)
  (blink (not x)))

(blink t)
```

ESP32 で Wi-Fi に接続する例：

```lisp
(wifi-connect "SSID" "password")
(wifi-localip)  ; IPアドレスを取得
```

### 6-5. uLisp 学習リソース

| リソース | URL | 説明 |
|----------|-----|------|
| uLisp 公式サイト | ulisp.com | ドキュメント、ダウンロード |
| uLisp フォーラム | forum.ulisp.com | コミュニティサポート |
| uLisp GitHub | github.com/technoblogy/ulisp | ソースコード |
| ulisp-esp-m5stack | github.com/m-g-r/ulisp-esp-m5stack | M5Stack 対応版 |


## 7. Common Lisp 学習リソース

### 7-1. 書籍（日本語）

#### 入門〜初級

| 書籍名 | 著者 | 出版社 | 備考 |
|--------|------|--------|------|
| **実践Common Lisp** | Peter Seibel（佐野匡俊 他訳） | オーム社 | 前半は入門、後半は実践。日本語で読めるCL入門書の決定版。原書はWeb公開あり |
| **Land of Lisp** | Conrad Barski（川合史朗 訳） | オライリー | ゲームを作りながら学ぶ。マンガやイラストが豊富で読みやすい |

#### 中級〜上級

| 書籍名 | 著者 | 出版社 | 備考 |
|--------|------|--------|------|
| **On Lisp** | Paul Graham（野田開 訳） | オーム社 | マクロに特化。まずCommon Lispが書けるようになってから読むべき |
| **実用Common Lisp** | Peter Norvig（杉本宣男 訳） | 翔泳社 | 古典AIをCommon Lispで実装。中堅から専門家へのステップアップに |
| **COMMON LISP 第2版** | Guy L. Steele Jr.（井田昌之 他訳） | 共立出版 | 言語仕様の詳細なリファレンス。ANSI CL以前の記述に注意 |

**注意：Common Lispのバージョンについて**

Common Lispには大別して3つのバージョンがある：

1. **初代 Common Lisp（1984年）**
2. **CLtL2（Common Lisp the Language 2nd Edition, 1990年）**
3. **ANSI Common Lisp（1994年〜現在）**

『On Lisp』や『COMMON LISP 第2版』はANSI CL以前の記述で書かれている部分があるため、現在の処理系で動作させる際には注意が必要である。CLtL2からANSI CLへの変換表がオンラインで公開されているので参照するとよい。

### 7-2. 書籍（英語）

| 書籍名 | 著者 | 備考 |
|--------|------|------|
| **Practical Common Lisp** | Peter Seibel | 『実践Common Lisp』原書。全文Web公開 |
| **Common Lisp Recipes** | Edmund Weitz | クックブック形式。電子版セール時は$10以下のことも |
| **The Common Lisp Condition System** | Michal Herda | コンディションシステムに特化した初の書籍 |
| **Programming Algorithms in Lisp** | Vsevolod Domkin | 様々なアルゴリズムをANSI CLで実装 |
| **Let Over Lambda** | Doug Hoyte | マクロの高度なテクニック（邦訳あり） |
| **The Art of the Metaobject Protocol** | Kiczales 他 | CLOSの設計思想。名著とされる |

### 7-3. オンラインリソース

#### 公式・リファレンス

| リソース | URL | 説明 |
|----------|-----|------|
| **Common Lisp HyperSpec** | lispworks.com/documentation/HyperSpec/ | ANSI CL仕様書のHTML版。必携 |
| **CLHS日本語訳プロジェクト** | — | 有志による翻訳（一部） |
| **Quicklisp** | quicklisp.org | ライブラリ管理システム |

#### チュートリアル・入門

| リソース | URL | 説明 |
|----------|-----|------|
| **Practical Common Lisp（原書）** | gigamonkeys.com/book/ | 無料で全文閲覧可能 |
| **Common Lisp Cookbook** | lispcookbook.github.io/cl-cookbook/ | 実用的なレシピ集 |
| **A Road to Common Lisp（日本語訳）** | gist.github.com/y2q-actionman/ | 学習ロードマップ |

#### 日本語情報

| リソース | URL | 説明 |
|----------|-----|------|
| **逆引きCommon Lisp** | — | 目的別の関数・マクロ検索 |
| **Zenn - Common Lisp関連記事** | zenn.dev | 処理系比較、学習本紹介など |
| **Qiita - Common Lispタグ** | qiita.com/tags/commonlisp | 日本語の技術記事 |

#### 処理系情報

| リソース | 説明 |
|----------|------|
| **2025年のCommon Lispの処理系はこれだ!** | 処理系の最新状況を毎年更新しているZenn記事 |
| **2025年のCommon Lispの学習本はこれだ!** | 書籍情報を毎年更新しているZenn記事 |
| **2025年のCommon LispのFAQではない質問** | よくある疑問への回答集 |

### 7-4. 学習の進め方（推奨）

```
1. 処理系のインストール（SBCL推奨）
   ↓
2. Quicklispのセットアップ
   ↓
3. 入門書で基礎を学ぶ
   └─ 『実践Common Lisp』または『Land of Lisp』
   ↓
4. Common Lisp Cookbookで実践的な書き方を学ぶ
   ↓
5. 興味のある分野の書籍・ライブラリを探索
   └─ マクロ → 『On Lisp』
   └─ AI → 『実用Common Lisp』
   └─ OOP → CLOS関連資料
   ↓
6. 自分のプロジェクトを開発
```

**補足**

- 書籍は20〜30年前のものでも内容が古くなっていない場合が多い。これはANSI CL規格が1994年以降ほぼ変更されていないためである
- 「Lisp村」では参考書の議論が盛り上がるが、コードをあまり書いていない人でも『On Lisp必読』などと勧めてくることがあるので、話半分で聞くのがよい
- まずCommon Lispを書けるようになることが最優先。その後でマクロなど高度な話題に進むべきである


## 補足：なぜ2025年にCommon Lispを学ぶのか

ANSI Common Lisp規格は1994年に制定されて以降、実質的な改訂がほぼ0であるにもかかわらず、2025年時点でも利用され続けている。これは規格策定時に徹底的な議論とユーザーからのフィードバックが反映され、仕様に抜けや漏れが非常に少ないためである。

コードが言語仕様で定められている範囲であれば、少なくとも30年（旧仕様と共通する範囲であれば40年）安定しているといってよい。これは「技術的負債が蓄積しにくい」という本記事冒頭で述べたCommon Lispの特徴を裏付けるものである。

