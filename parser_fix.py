import os
import pickle
from IPython import embed
import string
import codecs
import subprocess

def is_valid_line(line):
    if line.startswith('#'):
        if line.startswith('# sent_id') or line.startswith('# text'):
            return 1
        else:
            return 0
    else:
        return 1

def fix_file(path, out_path, in_path):
    mwt_tokens = {}
    file_out = open(out_path+path+'.fixed', '+w')
    additional_steps = 2
    i = 0
    with open(in_path+path) as f:
        for line in f.readlines():
            
            if line.startswith('#'):
                continue
            else:
                line_split = line.split('\t')
                if '-' in line_split[0]:
                    #if path == 'en.conllu':
                    #    print(i, line)
                    mwt_tokens[i+additional_steps] = line
                else:
                    file_out.write(line)
                    if line == '\n':
                        additional_steps += 2

            i += is_valid_line(line)

    if len(mwt_tokens.keys()) > 0:
        with open(out_path+path+'.pkl', '+wb') as o:
            pickle.dump(mwt_tokens, o)

def lang_encoding(path):
    if path.split('.')[0] in ['ru', 'uk', 'bg', 'cs', 'pl']:
        c = 'latin-1'
    elif path.split('.')[0] in ['cs', 'et', 'sk', 'lt']:
        c = 'iso-8859-15'
    else:
        c = 'utf-8'
    return c

def rewrite_text(path, out_path, in_path):
    sentences=[]
    skips = 0
    bad_mwt = False
    
    c = lang_encoding(path.split('.')[0])
        
    with codecs.open(in_path+path) as f:
        for line in f.readlines():
            if line.startswith('# sent'):
                sentence = {'sent_id':line,
                            'text':[],
                            'words':[]}
                
            elif line.startswith('# text'):
                pass
            
            elif line == '\n':
                sentences.append(sentence)    

            else:
                try:
                    #sentence['words'].append(line)
                    id, word_form, *_ = line.split('\t')
                except:
                    embed()
                    assert False
                    pass
                    
                if '-' in id:
                    s, e = id.split('-')
                    if s == e:
                        sentence['text'].append(word_form)
                        #sentence['words'].append(line.replace('32-32','32'))
                        bad_mwt = word_form
                        skips = 1
                        pass
                    else:
                        bad_mwt = False
                        skips = len(list(range(int(s),int(e)+1)))
                        sentence['text'].append(word_form)
                        sentence['words'].append(line)
                    continue

                if skips > 0:
                    skips -= 1
                    if bad_mwt != False:
                        l = line.split('\t')
                        l[1] = bad_mwt
                        line = '\t'.join(l)
                    sentence['words'].append(line)
                    continue
                else:
                    sentence['text'].append(word_form)
                    sentence['words'].append(line)

    lang = path.split('.')[0]
    with codecs.open(out_path+lang+'.conllu', '+w') as out:
        for sentence in sentences:
            out.write(sentence['sent_id'])
            out.write('# text = '+ ' '.join(sentence['text'])+'\n')
            for word in sentence['words']:
                out.write(word)
            out.write('\n')
            
            
def reconstruct_file(path, out_path, in_path, file_suffix):
    try:
        with open(in_path+path.split('.')[0]+file_suffix+'.pkl', 'rb') as p:
            mwt_tokens = pickle.load(p)
    except:
        mwt_tokens = {}
            
    out_file = codecs.open(out_path+path[:-6]+'.rec', '+w', encoding='utf-8')
    
    c = lang_encoding(path.split('.')[0])
    
    with codecs.open(in_path+path, encoding='utf-8') as f:
        pos = 0
        for i, line in enumerate(f.readlines()):
            #if line.startswith('#'):
                #out_file.write(line)
                #continue
            if pos in mwt_tokens.keys():
                out_file.write(mwt_tokens[pos])
                out_file.write(line)
                pos += 2
            else:
                out_file.write(line)
                pos += 1

def run_haskell(path, out_path, in_path):
    out_file = out_path+path[:-6]+'.hsout'
    in_file = in_path+path
    with open(out_file, '+w') as out:
        subprocess.run(['symbolic-fixer/Main', in_file], stdout=out)


def compare_strings(txt_file, out_file):
    txt_text = []
    out_text = []

    with codecs.open(txt_file) as f:
        for line in f.readlines():
            txt_text.append(line)

    with codecs.open(out_file) as f:
        for line in f.readlines():
            if line.startswith('# text'):
                out_text.append(' '.join(line.split('=')[1:]))

    txt = ''.join(txt_text).replace(' ','').replace('\t','').replace('\n','').lower()
    out = ''.join(out_text).replace(' ','').replace('\t','').replace('\n','').lower()

    for xt, xo in zip(txt, out):
        print(xt, xo, xt==xo)


def test_file(treebank, gold, system):
    subprocess.run(['python3', 'iwpt20_xud_eval.py', gold+treebank+'.collapsed', system+treebank])
        

def run_dev(lang=None):
    base_dir = 'data/dev-morph/'
    in_path = 'data/dev/'
    out1_path = 'data/dev/'
    out2_path = 'data/dev-out/'

    #in_path = 'data/gold-conllu/'
    #out1_path = 'data/gold-conllu/'
    #out2_path = 'data/gold-out/'
    
    ### COLLAPSE EMPY NODES:
    #for file in filter(lambda x: x.endswith('.conllu'), os.listdir(base_dir)):
    #    #file_path = 'data/blind-test/'+file
    #    in_file = base_dir+file
    #    out_file = out1_path+file+'.collapsed'
    #    with open(out_file, '+w') as out:
    #        print('Collapsing:', in_file)
    #        subprocess.run(['perl',
    #                        '/home/adamek/git/VARIOUS-TOOLS/tools/enhanced_collapse_empty_nodes.pl',
    #                        in_file],
    #                       stdout=out)

    lang = 'ru'
    
    ### RUN BEFORE INPUT TO HASKELL
    ### input-f: x.conllu.collapsed
    ### output-f: x.conllu.fixed
    for file in filter(lambda x: x.endswith('.collapsed'), os.listdir(in_path)):
        if not file.startswith(lang):
            continue
        file_path = base_dir+file
        print('mwt-tokens:', file_path)
        fix_file(file, in_path, in_path)

    ### ADD DEPENDENCIES
    ### input-f: x.conllu.collapsed.fixed
    ### output-f: x.conllu.hsout
    for file in filter(lambda x: x.endswith('.fixed'), os.listdir(in_path)):
        if not file.startswith(lang):
            continue
        print('tree-rewrite:', file)
        run_haskell(file, in_path, in_path)

    #embed()
    
    ### RUN ON HASKELL OUTPUT
    # input-f: x.conllu.hsout
    # output-f: x.conllu.rec
    for file in filter(lambda x: x.endswith('.hsout'), os.listdir(out1_path)):
        if not file.startswith(lang):
            continue
        print('reconstruct:', file)
        file_ending = '.conllu.collapsed'
        reconstruct_file(file, out2_path, in_path, file_ending)

    ### FIX METADATA TEXT
    # input-f: x.conllu.rec
    # output-f: x.conllu
    for file in filter(lambda x: x.endswith('.rec'), os.listdir(out2_path)):
        if not file.startswith(lang):
            continue
        print('fix metadata text', file)
        rewrite_text(file, out2_path, out2_path)

    ### TEST DATA
    for file in sorted(filter(lambda x: x.endswith('.conllu'), os.listdir(out2_path))):
        if not file.startswith(lang):
            continue
        print('test', file)
        test_file(file, in_path, out2_path)
        print()

    ### run UD-validation script
    #for file in filter(lambda x: x.endswith('.conllu'), os.listdir('data/blind-test-output/')):
    #    print('fix metadata text', file)
    #    validate_files(file)

    
def fix_test_languages():

    in_path = 'data/gold-conllu/'
    out1_path = 'data/gold-conllu/'
    out2_path = 'data/gold-out/'
    
    #compare_strings('data/blind-test/fi.txt', 'data/blind-test-output/fi.conllu')
    #assert False
    
    ### RUN BEFORE INPUT TO HASKELL
    # input-f: x.conllu
    # output-f: x.conllu.fixed
    for file in filter(lambda x: x.endswith('.conllu'), os.listdir('data/blind-test/')):
        file_path = 'data/blind-test/'+file
        print('mwt-tokens:', file_path)
        fix_file(file, in_path, in_path)

    ### ADD DEPENDENCIES
    # input-f: x.conllu.fixed
    # output-f: x.conllu.hsout
    #for file in filter(lambda x: x.endswith('conllu.fixed'), os.listdir('data/blind-test/')):
    #    print('tree-rewrite:', file)
    #    run_haskell(file+'.fixed')
        
    ### RUN ON HASKELL OUTPUT
    # input-f: x.conllu.hsout
    # output-f: x.conllu.rec
    # changed to fixed, because no haskell proessing
    for file in filter(lambda x: x.endswith('collapsed.fixed'),
                      os.listdir(out1_path)):
        print('reconstruct:', file)
        file_ending = '.conllu.collapsed'
        reconstruct_file(file, out2_path, in_path, file_ending)

    ### FIX METADATA TEXT
    # input-f: x.conllu.rec
    # output-f: x.conllu
    #for file in filter(lambda x: x.endswith('.conllu.rec'),
    #                  os.listdir(out2_path)):
    #    print('fix metadata text', file)
    #    rewrite_text(file, out2_path, out2_path)

    ### run UD-validation script
    for file in filter(lambda x: x.endswith('.conllu.rec'),
                      os.listdir(out2_path)):
        print('fix metadata text', file)
        validate_files(file)



def fix_single_language(language):
    ### RUN BEFORE INPUT TO HASKELL

    file = language+'.conllu'
    fix_file(file)

    run_haskell(file+'.fixed')
    reconstruct_file(file+'.hsout')

    rewrite_text(file+'.rec')    
        
if __name__ == '__main__':
    #fix_single_language('ar-dev')
    #rewrite_text('it.conllu')
    run_dev()
    #fix_test_languages()
    
    # current errors: encoding is WIERD! ... bullcrap
