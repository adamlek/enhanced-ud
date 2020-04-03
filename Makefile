test:: en_ewt-ud-dev.fixed.conllu
	python3 iwpt20_xud_eval.py data/UD_English-EWT/en_ewt-ud-dev.conllu en_ewt-ud-dev.fixed.conllu

en_ewt-ud-dev.fixed.conllu: symbolic-fixer/Main data/UD_English-EWT/en_ewt-ud-dev.conllu
	symbolic-fixer/Main data/UD_English-EWT/en_ewt-ud-dev.conllu > $@

symbolic-fixer/Main: symbolic-fixer/*.hs
	cd symbolic-fixer && make


