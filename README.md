# algo_basic

algo basicをOCamlのAlgebraic Effect Handlerを用いて実装したリポジトリです．
現在は一つのターミナルで2人が対戦することができる．(改良予定)

## ゲームの始め方
OCaml >= 5.0.0 もしくは ocaml-malticoreのライブラリをインストール(その場合は構文が異なるため適宜書き換え必須)
```
dune build
```
```
./_build/default/algo.ml
```

## ToDo
- ゲームの要素追加
    - 攻撃者が成功した際の選択
    - チップ
- local環境での2人対戦の実装