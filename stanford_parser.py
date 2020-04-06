import stanza
from IPython import embed

parser = stanza.Pipeline("en")

with open('data/UD_English-EWT/en_ewt-ud-dev.txt') as f:
    text = f.read()

o = open('data/UD_English-EWT/stanford-out-dev.conllu', '+w')

parsed_text = parser(text)

for sent in parsed_text.sentences:
    for word in sent.words:
        o.write('\t'.join([word.id,
                         word.text,
                         word.lemma,
                         word.upos,
                         word.xpos,
                         '_' if word.feats == None else word.feats,
                         str(word.head),
                         word.deprel,
                         ':'.join([str(word.head), word.deprel]),
                           '_'])+'\n')
    o.write('\n')
o.close()
