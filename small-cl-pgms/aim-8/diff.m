-*- mode:m-expression; coding:utf-8 -*-

diff=λ[[y;x];
       [atom[y]
         ⎯⟶ [y=x ⎯⟶ ONE;
               1 ⎯⟶ ZERO];
        first[y]=PLUS
         ⎯⟶ combine[PLUS;
                    maplist[rest[y];λ[[z];diff[first[z];x]]]];
        first[y]=TIMES
         ⎯⟶ combine[PLUS;
                    maplist[rest[y];
                            λ[[z];
                              combine[TIMES;
                                      maplist[rest[y];
                                              λ[[w];
                                                [z≠w ⎯⟶ first[w];
                                                   1 ⎯⟶ diff[first[w];x]]]]]]]]]
