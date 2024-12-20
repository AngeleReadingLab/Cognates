clear all
cd D:\Bernhard\Documents\Experiments\ExperimentSHUF\English

boycow = load('boycow_short.mat');

bc = boycow.sentence;
%rebuild structure array from imported data
%same names as PSC array

for i=1:length(bc.sentence_nr)
    
    sentnr = bc.sentence_nr(i);
    wordnr = bc.word_nr(i);
    
    sentence(sentnr).words(wordnr) = bc.word(i);
    sentence(sentnr).wl1(wordnr) = length(bc.word{i});
    sentence(sentnr).words_no_pct(wordnr) = bc.word_no_pct(i);
    sentence(sentnr).wl2(wordnr) = length(bc.word_no_pct{i});
    sentence(sentnr).wl3(wordnr) = length(bc.word_no_pct{i});
    sentence(sentnr).fclx(wordnr) = bc.freq_celex(i);
    sentence(sentnr).fbnc(wordnr) = bc.freq_bnc(i);
    sentence(sentnr).pos(wordnr) = bc.pos(i);
    sentence(sentnr).pred(wordnr) = -1; %dummy for predictability
    
    %now make the part-of-speech information compatible with the potsdam
    %wcat system
    
    % 1 noun
    % 2 verb
    % 3 adjective
    
    % 4 adverbs: nur Vorne Sogar gestern schon auch noch leider nie Manchmal liebsten Hier sehr Angeblich oft Manchmal genauso Schon immer
    % selbst gern nur mehr erst Vielleicht bald wieder unglücklicherweise stets besonders wieder wieder sofort unbedingt Heute morgen
    % fast nie nicht gerne nur selten bestimmt Meistens gern miteinander vormittags samstags mehr genau gerade gegenseitig so Jetzt
    % doch mal so wieder gerne oft sehr gelegentlich
    
    % 5 seinem/seiner man sich Wir ihr/ihre/ihrer/ihrem Er keine/keinen/Keiner meisten viel/viele/vielen Kein/keine Jede/jeden/Jeder Unsere/unserer
    % Es mehrere Manche Einige wenig/wenige seinen alles/alle Sie dieses was etwas Ich beiden wen mir deiner beiden
    
    % 6 prepositions
    % 7 und/oder/ob/als/wie
    % 8 ab/ nicht/ zu/ zusammen/ an/ herum/ auf/ statt/ an/ bitte/ vor/ zurueck
    % 9 der/die/das/dem/eine etc
    % 10 numbers
    
    switch bc.pos{i}
        case 'noun'
            wcat = 1;
        case 'verb'
            wcat = 2;
        case 'adjective'
            wcat = 3;
        case 'adverb'
            wcat = 4;
        case {'determiner', 'pronoun'}
            wcat = 5;
        case 'preposition'
            wcat = 6;
        case 'conjunction'
            wcat = 7;
        case {'not','to'}
            wcat = 8;
        case 'article'
            wcat = 9;
        case 'numeral'
            wcat = 10;
        otherwise
            warning('No wcat information found for sentence %i, word %i: %s',sentnr,wordnr,bc.word{i})
            wcat = NaN;
    end
    
    sentence(sentnr).wcat(wordnr) = wcat;
end


for j = 1:length(sentence)
    sentence(j).nw = length(sentence(j).words);
end

save shuffle_corpus_english.mat sentence