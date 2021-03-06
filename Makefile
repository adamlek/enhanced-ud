test2:: data/dev-morph/ru_syntagrus-ud-dev.fixed

test:: data/UD_English-EWT/stanford-dev.fixed
	python3 iwpt20_xud_eval.py data/UD_English-EWT/en_ewt-ud-dev.conllu $<

english-simple:: data/UD_English-EWT/en_ewt-ud-dev.fixed
	python3 iwpt20_xud_eval.py data/UD_English-EWT/en_ewt-ud-dev.conllu $<

tamil-simple:: data/UD_Tamil/ta_ttb-ud-dev.fixed
	python3 iwpt20_xud_eval.py data/UD_Tamil/ta_ttb-ud-dev.conllu $<

lithuanian-simple:: data/UD_Lithuanian-ALKSNIS/lt_alksnis-ud-dev.fixed
	python3 iwpt20_xud_eval.py data/UD_Lithuanian-ALKSNIS/lt_alksnis-ud-dev.conllu $<

%.fixed: %.conllu symbolic-fixer/Main
	symbolic-fixer/Main $< > $@

symbolic-fixer/Main: symbolic-fixer/*.hs
	cd symbolic-fixer && make

data/UD_English-EWT/stanford-dev.conllu: data/UD_English-EWT/en_ewt-ud-dev.conllu
	java -cp stanford-corenlp-full-2018-10-05/*: edu.stanford.nlp.parser.lexparser.LexicalizedParser -maxLength 150 -outputFormatOptions includePunctuationDependencies -outputFormat conll2007 edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz $< > $@
