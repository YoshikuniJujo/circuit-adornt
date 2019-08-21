メモ
====

* パッケージをわける
	+ circuit-adornt: メタパッケージ
	+ circuit-adornt-simulate: シミュレーション
	+ circuit-adornt-diagram: 回路図
	+ circuit-adornt-build: 回路データの作成

パッケージの階層 (現時点での)
-----------------------------

* circuit-adornt
	+ circuit-adornt-parts
		- circuit-adornt-build
* circuit-adornt-diagram
	+ circuit-diagram-dsl
	+ circuit-adornt-parts
		- circuit-adornt-build

TODO
----

つぎのような感じにする。

* circuit-adornt
	+ circuit-adornt-simulate
		+ circuit-adornt-build
	+ circuit-adornt-diagram
		+ circuit-diagram-dsl
		+ circuit-adornt-build
	+ circuit-adornt-samples
		+ circuit-adornt-parts
			- circuit-adornt-build

circuit-adonrtのなかみを、circuit-adornt-samplesにうつす。
circuit-adorntはメタパッケージにする。
あるいは、メタパッケージではなくて、使いやすくいろいろをまとめたパッケージにするか。
