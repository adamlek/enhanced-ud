import os
import pickle
from IPython import embed
import string
import codecs
import subprocess

def fix_file(path):
    mwt_tokens = {}
    file_out = open('data/blind-test/'+path+'.fixed', '+w')
    additional_steps = 2
    with open('data/blind-test/'+path) as f:
        for i, line in enumerate(f.readlines()):
            line_split = line.split('\t')
            if '-' in line_split[0]:
                if path == 'en.conllu':
                    print(i, line)
                mwt_tokens[i+additional_steps] = line
            else:
                file_out.write(line)
                if line == '\n':
                    additional_steps += 2
                

    if len(mwt_tokens.keys()) > 0:
        with open('data/blind-test/'+path[:-6]+'pkl', '+wb') as o:
            pickle.dump(mwt_tokens, o)

def rewrite_text(path):
    sentences=[]
    skips = 0
    with codecs.open('data/blind-test-output/'+path) as f:
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
                sentence['words'].append(line)
                try:
                    id, word_form, *_ = line.split('\t')
                except:
                    pass
                    #embed()
                    #assert False
                    
                if '-' in id:
                    s, e = id.split('-')
                    skips = len(list(range(int(s),int(e)+1)))
                    sentence['text'].append(word_form)
                    continue

                if skips > 0:
                    skips -= 1
                    continue
                else:
                    sentence['text'].append(word_form)

    lang = path.split('.')[0]
    with codecs.open('data/blind-test-output/'+lang+'.conllu', '+w', encoding='utf-8') as out:
        for sentence in sentences:
            out.write(sentence['sent_id'])
            out.write('# text = '+ ' '.join(sentence['text'])+'\n')
            for word in sentence['words']:
                out.write(word)
            out.write('\n')
            
            
def reconstruct_file(path):
    try:
        with open('data/blind-test/'+path.split('.')[0]+'.pkl', 'rb') as p:
            mwt_tokens = pickle.load(p)
    except:
        mwt_tokens = {}
            
    out_file = codecs.open('data/blind-test-output/'+path[:-6], '+w', encoding='utf-8')

    if path.split('.')[0] in ['ru', 'uk', 'bg', 'cs']:
        c = 'latin1'
    elif path.split('.')[0] in ['cs', 'pl', 'lt', 'et', 'sk']:
        c = 'iso-8859-15'
    else:
        c = 'utf-8'
    
    with codecs.open('data/blind-test-output/'+path, encoding=c) as f:
        pos = 0
        for i, line in enumerate(f.readlines()):
            #if line.startswith('#'):
                #out_file.write(line.encode('utf-8').decode('utf-8'))
                #continue
            if pos in mwt_tokens.keys():
                out_file.write(mwt_tokens[pos].encode('utf-8').decode('utf-8'))
                out_file.write(line.encode('utf-8').decode('utf-8'))
                pos += 2
            else:
                out_file.write(line.encode('utf-8').decode('utf-8'))
                pos += 1

def run_haskell(path):
    out_file = 'data/blind-test-output/'+path[:-6]+'.hsout'
    in_file = 'data/blind-test/'+path
    with open(out_file, '+w') as out:
        subprocess.run(['symbolic-fixer/Main', in_file], stdout=out)
    
            
if __name__ == '__main__':
    ### RUN BEFORE INPUT TO HASKELL
    for file in filter(lambda x: x.endswith('.conllu'), os.listdir('data/blind-test/')):
        file_path = 'data/blind-test/'+file
        print('mwt-tokens:', file_path)
        fix_file(file)

    ### ADD DEPENDENCIES
    for file in filter(lambda x: x.endswith('conllu.fixed'), os.listdir('data/blind-test/')):
        print('tree-rewrite:', file)
        run_haskell(file)
        
    ### RUN ON HASKELL OUTPUT
    for file in filter(lambda x: x.endswith('.hsout'), os.listdir('data/blind-test-output/')):
        print('reconstruct:', file)
        reconstruct_file(file)

    ### FIX METADATA TEXT
    for file in filter(lambda x: x.endswith('.conllu'), os.listdir('data/blind-test-output/')):
        print('fix metadata text', file)
        rewrite_text(file)

    ### RUN ON OUTPUT FROM ABOVE
    #rewrite_text('it.conllu')

    # current errors: encoding is WIERD! ... bullcrap
