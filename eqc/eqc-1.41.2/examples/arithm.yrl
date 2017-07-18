Nonterminals E uminus digits.
Terminals '*' '-' number digit.
Rootsymbol E.

Left 100 '-'.
Left 200 '*'.
Unary 300 uminus.

E -> E '-' E: #{minus => {'$1','$3'}}.
E -> E '*' E: {times,'$1','$3'}.
E -> uminus: '$1'.
E -> number: element(3,'$1').

uminus -> '-' E: {minus,'$2'}. 

%% test shrinking
digits -> digit digits: ['$1'|'$2'].
digits -> digit: ['$1'].


