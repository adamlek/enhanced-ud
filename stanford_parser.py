import stanza
import os
from IPython import embed


def parse_lang(in_file, out_file):
    print(in_file, out_file)
    parser = stanza.Pipeline(in_file[:-4])

    o = open(f'data/blind-test/{out_file}', '+w')

    with open(f'data/blind-test/{in_file}') as f:
        text = f.readlines()

    texts = [[]]
    for ln in text:
        ln = ln.rstrip()
        if ln == '':
            texts.append([])
        else:
            texts[-1].append(ln)

    for paragraph in texts:
        if paragraph == []:
            continue
        paragraph = ' '.join(paragraph)
        #print(paragraph)
        parsed_text = parser(paragraph)

        for sent in parsed_text.sentences:
            for word in sent.words:
                if 'xpos' in word.to_dict().keys():
                    xpos = word.xpos
                else:
                    xpos = '_'

                o.write('\t'.join([word.id,
                                   word.text,
                                   word.lemma,
                                   word.upos,
                                   xpos,
                                   '_' if word.feats == None else word.feats,
                                   str(word.head),
                                   word.deprel,
                                   ':'.join([str(word.head), word.deprel]),
                                   '_'])+'\n')
            o.write('\n')
    o.close()

if __name__ == '__main__':
    #for in_file in list(filter(lambda x: x.endswith('.txt'), os.listdir('data/blind-test/'))):
    #    parse_lang(in_file, in_file[:-4]+'.conllu')

    parse_lang('lv.txt', 'lv.conllu')
#    for lang in ['lt', 'nl', 'ru', 'sv', 'uk']:
#        parse_lang(f'{lang}.txt', f'{lang}.conllu')
